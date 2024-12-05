// //cards.jsx
import React, { useState } from "react";
import ReactApexChart from "react-apexcharts";

const CardWithPieChart = ({ piechartInfo, onPieChartSelect }) => {
  if (!piechartInfo || Object.keys(piechartInfo).length === 0) {
    return (
      <div className="card pie-chart-card">
        <h3>Overall Analysis</h3>
      </div>
    );
  }
  const chartLabels = Object.keys(piechartInfo);
  const chartSeries = Object.values(piechartInfo);

  const chartOptions = {
    series: chartSeries,
    options: {
      chart: {
        type: "pie",
        events: {
          dataPointSelection: (event, chartContext, config) => {
            const label = config.w.config.labels[config.dataPointIndex];
            onPieChartSelect(label);
          },
        },
      },
      labels: chartLabels,
      responsive: [
        {
          breakpoint: 480,
          options: {
            chart: {
              width: 200,
            },
            legend: {
              position: "bottom",
            },
          },
        },
      ],
      title: {
        text: "Overall Analysis",
        align: "center",
      },
      plotOptions: {
        pie: {
          donut: {
            labels: {
              show: false,
            },
          },
        },
      },
      dataLabels: {
        enabled: false,
      },
      legend: {
        position: "right",
        offsetY: 0,
        height: 230,
      },
    },
  };

  return (
    <div className="card pie-chart-card">
      <ReactApexChart
        options={chartOptions.options}
        series={chartOptions.series}
        type="pie"
        width="95%"
      />
    </div>
  );
};

const CardWithBarChart = ({
  subcategoryInfo,
  title,
  chartTitle,
  filters,
  onSubCategory1Select,
}) => {
  const filterKeys = Object.entries(filters)
    .filter(([_, value]) => value !== null)
    .map(([key, value]) => value);

  const filteredSubcategoryInfo = Object.fromEntries(
    Object.entries(subcategoryInfo).filter(
      ([key]) =>
        filterKeys.length === 0 ||
        filterKeys.every((filterKey) => key.includes(filterKey))
    )
  );

  if (
    !filteredSubcategoryInfo ||
    Object.keys(filteredSubcategoryInfo).length === 0
  ) {
    return (
      <div className="card bar-chart-card">
        <h3>{title}</h3>
        {filterKeys.length > 0 && <p>No data found for filters</p>}
      </div>
    );
  }

  const chartLabels = Object.keys(filteredSubcategoryInfo);
  const chartSeries = [
    {
      name: "Subcategory Counts",
      data: Object.values(filteredSubcategoryInfo),
    },
  ];

  const barChartData = {
    series: chartSeries,
    options: {
      chart: {
        type: "bar",
        height: 350,
        events: {
          dataPointSelection: (event, chartContext, config) => {
            const label =
              config.w.config.xaxis.categories[config.dataPointIndex];
            onSubCategory1Select(label.split(" (")[0]);
          },
        },
      },
      plotOptions: {
        bar: {
          horizontal: false,
          columnWidth: "60%",
        },
      },
      dataLabels: {
        enabled: false,
      },
      xaxis: {
        categories: chartLabels,
        title: {
          text: title,
        },
        labels: {
          rotate: -45,
          rotateAlways: true,
        },
      },
      yaxis: {
        title: {
          text: "Count",
        },
      },
      title: {
        text: filterKeys.length > 0 ? `${chartTitle} (Filtered)` : chartTitle,
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            xaxis: {
              labels: {
                rotate: -90,
              },
            },
          },
        },
      ],
    },
  };

  return (
    <div className="card bar-chart-card">
      <ReactApexChart
        options={barChartData.options}
        series={barChartData.series}
        type="bar"
        height={350}
      />
    </div>
  );
};

const CardWithStepLineChart = ({ subcategoryInfo, title, filters }) => {
  const filterKeys = Object.entries(filters)
    .filter(([_, value]) => value !== null)
    .map(([key, value]) => value);

  const filteredSubcategoryInfo = Object.fromEntries(
    Object.entries(subcategoryInfo).filter(
      ([key]) =>
        filterKeys.length === 0 ||
        filterKeys.every((filterKey) => key.includes(filterKey))
    )
  );

  if (
    !filteredSubcategoryInfo ||
    Object.keys(filteredSubcategoryInfo).length === 0
  ) {
    return (
      <div className="card step-line-chart-card">
        <h3>{title}</h3>
        {filterKeys.length > 0 && <p>No data found for filters</p>}
      </div>
    );
  }

  const chartLabels = Object.keys(filteredSubcategoryInfo);
  const chartSeries = [
    {
      name: "Counts",
      data: Object.values(filteredSubcategoryInfo),
    },
  ];

  const stepLineChartData = {
    series: chartSeries,
    options: {
      chart: {
        type: "line",
        height: 350,
      },
      stroke: {
        curve: "stepline",
      },
      xaxis: {
        categories: chartLabels,
        labels: {
          rotate: -45,
          show: true,
        },
        title: {
          text: "Subcategories",
        },
        min: 0,
      },
      yaxis: {
        title: {
          text: "Counts",
        },
        tickAmount: 5,
        max:
          Math.ceil(Math.max(...Object.values(filteredSubcategoryInfo)) / 5) *
          5,
      },
      title: {
        text: filterKeys.length > 0 ? `${title} (Filtered)` : title,
        align: "center",
      },
      tooltip: {
        enabled: true,
      },
      responsive: [
        {
          breakpoint: 480,
          options: {
            xaxis: {
              labels: {
                rotate: -90,
              },
            },
          },
        },
      ],
    },
  };

  return (
    <div className="card step-line-chart-card">
      <ReactApexChart
        key={filterKeys.join("-") || "default"}
        options={stepLineChartData.options}
        series={stepLineChartData.series}
        type="line"
        height={350}
      />
    </div>
  );
};

const Cards = ({
  piechartInfo,
  subcategoryInfo,
  subcategory2Info,
  primaryKey2Info,
  onPieChartSelect,
  filters,
}) => {
  const [level1SelectedKey, setLevel1SelectedKey] = useState(null);
  const [level2SelectedKey, setLevel2SelectedKey] = useState(null);
  const [pieChartFilter, setPieChartFilter] = useState(null);

  const handleSubCategory1Select = (selectedKey) => {
    setLevel1SelectedKey(selectedKey);
    setLevel2SelectedKey(null);
    setPieChartFilter(null);
  };

  const handleSubCategory2Select = (selectedKey) => {
    setLevel2SelectedKey(selectedKey);
    setPieChartFilter(null);
  };

  const handlePieChartSelect = (selectedKey) => {
    setPieChartFilter(selectedKey);
    setLevel1SelectedKey(null);
    setLevel2SelectedKey(null);
    onPieChartSelect(selectedKey);
  };

  const level2Filters = {
    ...filters,
    level1: level1SelectedKey,
    pieChart: pieChartFilter,
  };

  // Determine filters for Level 3
  const level3Filters = {
    ...filters,
    level1: level1SelectedKey,
    level2: level2SelectedKey,
    pieChart: pieChartFilter,
  };

  return (
    <div className="card-container">
      <div className="row">
        <CardWithPieChart
          piechartInfo={piechartInfo}
          onPieChartSelect={handlePieChartSelect}
        />
        <CardWithBarChart
          subcategoryInfo={subcategoryInfo}
          title="Level 1 Bucket"
          chartTitle="Level 1 Bucket"
          filters={filters}
          onSubCategory1Select={handleSubCategory1Select}
        />
      </div>
      <div className="row">
        <CardWithBarChart
          subcategoryInfo={primaryKey2Info}
          title="Level 2 Bucket"
          chartTitle="Level 2 Bucket"
          filters={level2Filters}
          onSubCategory1Select={handleSubCategory2Select}
        />
        <CardWithStepLineChart
          subcategoryInfo={subcategory2Info}
          title="Level 3 Bucket"
          filters={level3Filters}
        />
      </div>
    </div>
  );
};

export default Cards;
