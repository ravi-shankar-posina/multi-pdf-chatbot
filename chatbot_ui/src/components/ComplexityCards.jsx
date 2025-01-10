import React, { useState } from "react";
import ReactApexChart from "react-apexcharts";

const chartColors = [
  "#008FFB",
  "#00E396",
  "#FEB019",
  "#FF4560",
  "#775DD0",
  "#3F51B5",
  "#546E7A",
  "#D4526E",
  "#8D5B4C",
  "#F86624",
];

const CardWithPieChart = ({ complexityPiechartInfo, onPieChartSelect }) => {
  if (
    !complexityPiechartInfo ||
    Object.keys(complexityPiechartInfo).length === 0
  ) {
    return (
      <div className="bg-white rounded-lg shadow p-4 h-[300px]">
        <h3 className="font-bold text-xl">Count of Number by Location</h3>
      </div>
    );
  }
  const countryData = {};
  Object.keys(complexityPiechartInfo).forEach((key) => {
    const country = key.split(" - ")[0]; // Get the country name (first part before '-')
    const count = complexityPiechartInfo[key]; // Get the count associated with the location

    if (countryData[country]) {
      countryData[country] += count; // Add the count to the existing country entry
    } else {
      countryData[country] = count; // If country is not yet in the object, initialize it
    }
  });
  const chartLabels = Object.keys(countryData);
  const chartSeries = Object.values(countryData);

  const options = {
    chart: {
      type: "pie",
      events: {
        dataPointSelection: (event, chartContext, config) => {
          const label = chartContext.w.config.labels[config.dataPointIndex];
          onPieChartSelect(label);
        },
      },
    },
    colors: chartColors,
    labels: chartLabels,
    legend: {
      position: "bottom",
      horizontalAlign: "center",
      floating: false,
      fontSize: "12px",
      offsetY: 7,
      height: 40,
    },
    title: {
      text: "Count of Number by Location",
      align: "center",
      style: {
        fontSize: "14px",
        fontWeight: 600,
      },
    },
    dataLabels: {
      enabled: true,
      formatter: function (val) {
        return val.toFixed(1) + "%";
      },
    },
    responsive: [
      {
        breakpoint: 480,
        options: {
          legend: {
            position: "bottom",
          },
        },
      },
    ],
  };

  return (
    <div className="bg-white rounded-lg shadow p-4 h-[300px]">
      <ReactApexChart
        options={options}
        series={chartSeries}
        type="pie"
        height={280}
      />
    </div>
  );
};

const CardWithBarChart = ({
  complexityBarChart1Info,
  title,
  chartTitle,
  filters,
  onSubCategory1Select,
  isLevel2 = false,
  pieChartFilter = null,
}) => {
  const filteredData = isLevel2
    ? pieChartFilter
      ? Object.fromEntries(
          Object.entries(complexityBarChart1Info).filter(([key]) =>
            key.includes(pieChartFilter)
          )
        )
      : {}
    : complexityBarChart1Info;
  let chartLabels = [];
  let chartSeries = [];

  if (isLevel2) {
    // Initialize grouped data with zero values
    let groupedData = {
      Simple: { Simplify: 0, Innovate: 0, Reduce: 0 },
      Medium: { Simplify: 0, Innovate: 0, Reduce: 0 },
      Complex: { Simplify: 0, Innovate: 0, Reduce: 0 },
    };

    // Loop through filteredData and update groupedData
    Object.entries(filteredData).forEach(([key, value]) => {
      // Extract complexity (Simple, Medium, Complex) from the key
      const match = key.match(/^(Simple|Medium|Complex).*?\((.*?)\)$/);
      if (match) {
        const [_, complexity] = match; // Extract complexity from the matched group
        // Ensure the complexity exists in groupedData
        if (groupedData[complexity]) {
          // Process each category (Innovate, Reduce, Simplify)
          Object.entries(value).forEach(([category, categoryValue]) => {
            const numericValue = Number(categoryValue);

            // Check if the value is a valid number
            if (!isNaN(numericValue)) {
              // Update the groupedData for the specific category
              groupedData[complexity][category] += numericValue;
            } else {
              console.warn(`Invalid value for ${category}: ${categoryValue}`);
            }
          });
        } else {
          console.warn(`Invalid Complexity: ${complexity}`);
        }
      } else {
        console.warn(`Key format mismatch: ${key}. Assigning to 'Other'`);
        // If the key doesn't match, assign values to 'Other' in 'Simple'
        Object.entries(value).forEach(([category, categoryValue]) => {
          const numericValue = Number(categoryValue);
          if (!isNaN(numericValue)) {
            groupedData["Simple"]["Other"] += numericValue;
          }
        });
      }
    });
    // Prepare chart data from groupedData
    chartLabels = Object.keys(groupedData); // ["Simple", "Medium", "Complex"]
    chartSeries = Object.keys(groupedData.Simple).map((subCategory) => ({
      name: subCategory,
      data: chartLabels.map(
        (complexity) => groupedData[complexity][subCategory]
      ),
    }));
  } else {
    const countryData = {};
    Object.keys(filteredData).forEach((key) => {
      const country = key.split(" - ")[0]; // Get the country name (first part before '-')
      const count = filteredData[key]; // Get the count associated with the location

      if (countryData[country]) {
        countryData[country] += count; // Add the count to the existing country entry
      } else {
        countryData[country] = count; // If country is not yet in the object, initialize it
      }
    });

    // Step 2: Prepare chart data
    chartLabels = Object.keys(countryData); // Country names are the labels

    chartSeries = [
      {
        name: "Count",
        data: Object.values(countryData), // Count values are the data for the chart
      },
    ];
  }

  const options = {
    chart: {
      type: "bar",
      height: isLevel2 ? 280 : 600,
      stacked: isLevel2, // Enable stacking for Level 2
      toolbar: {
        show: true,
      },
      events: {
        dataPointSelection: (event, chartContext, config) => {
          const label = chartLabels[config.dataPointIndex];
          onSubCategory1Select(label);
        },
      },
    },
    colors: ["#FF5733", "#33FF57", "#3357FF"], // Colors for subcategories
    plotOptions: {
      bar: {
        horizontal: false,
        columnWidth: "45%",
        distributed: !isLevel2, // Only distribute for Level 1
      },
    },
    dataLabels: {
      enabled: true,
      formatter: function (val) {
        return val.toFixed(0);
      },
      style: {
        fontSize: "12px",
      },
    },
    legend: {
      position: "top",
      horizontalAlign: "center",
      show: isLevel2, // Show legend only for Level 2
    },
    xaxis: {
      categories: chartLabels,
      title: {
        text: title,
        style: {
          fontSize: "14px",
        },
      },
      labels: {
        style: {
          fontSize: "12px",
        },
      },
    },
    yaxis: {
      title: {
        text: "Count",
        style: {
          fontSize: "14px",
        },
      },
    },
    title: {
      text:
        isLevel2 && pieChartFilter
          ? `${chartTitle} (${pieChartFilter})`
          : chartTitle,
      style: {
        fontSize: "14px",
        fontWeight: 600,
      },
    },
    tooltip: {
      shared: true,
      intersect: false,
      y: {
        formatter: function (val) {
          return val.toFixed(0);
        },
      },
    },
  };

  return (
    <div
      className={`bg-white rounded-lg shadow p-4 ${
        isLevel2 ? "h-[300px]" : "h-full"
      }`}
    >
      <ReactApexChart
        options={options}
        series={chartSeries}
        type="bar"
        height={isLevel2 ? 280 : 600}
      />
    </div>
  );
};

const ComplexityCards = ({
  complexityPiechartInfo,
  complexityBarChart1Info,
  onPieChartSelect,
  filters,
}) => {
  const [level1SelectedKey, setLevel1SelectedKey] = useState(null);
  const [pieChartFilter, setPieChartFilter] = useState(null);

  const handlePieChartSelect = (selectedKey) => {
    setPieChartFilter(selectedKey);
    setLevel1SelectedKey(null);
    onPieChartSelect(selectedKey);
  };

  const handleSubCategory1Select = (selectedKey) => {
    setLevel1SelectedKey(selectedKey);
    setPieChartFilter(null);
  };

  return (
    <div className="w-full h-screen flex gap-4">
      <div className="w-[40%] flex flex-col gap-4">
        <CardWithPieChart
          complexityPiechartInfo={complexityPiechartInfo}
          onPieChartSelect={handlePieChartSelect}
        />
        <CardWithBarChart
          complexityBarChart1Info={complexityBarChart1Info}
          title="Count of  Number by Location"
          chartTitle=""
          filters={filters}
          onSubCategory1Select={handleSubCategory1Select}
          isLevel2={true}
          pieChartFilter={pieChartFilter}
        />
      </div>
      <div className="w-[60%] h-full">
        <CardWithBarChart
          complexityBarChart1Info={complexityPiechartInfo}
          title=""
          chartTitle=""
          filters={{}}
          onSubCategory1Select={handleSubCategory1Select}
        />
      </div>
    </div>
  );
};

export default ComplexityCards;
