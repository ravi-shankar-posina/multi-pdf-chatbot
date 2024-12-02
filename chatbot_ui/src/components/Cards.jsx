//Cards.jsx
import React from "react";
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
            const value = config.w.config.series[config.dataPointIndex];

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
  filterKey,
}) => {
  const filteredSubcategoryInfo = filterKey
    ? Object.fromEntries(
        Object.entries(subcategoryInfo).filter(([key, _]) =>
          key.includes(filterKey)
        )
      )
    : subcategoryInfo;

  if (
    !filteredSubcategoryInfo ||
    Object.keys(filteredSubcategoryInfo).length === 0
  ) {
    return (
      <div className="card bar-chart-card">
        <h3>{title}</h3>
        {filterKey && <p>No data found for {filterKey}</p>}
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
        text: filterKey
          ? `${chartTitle} (Filtered by ${filterKey})`
          : chartTitle,
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

const Cards = ({
  piechartInfo,
  subcategoryInfo,
  subcategory2Info,
  primaryKey2Info,
  onPieChartSelect,
  filterKey,
}) => {
  return (
    <div className="card-container">
      <div className="row justify-content-center items-center">
        <CardWithPieChart
          piechartInfo={piechartInfo}
          onPieChartSelect={onPieChartSelect}
        />
        <CardWithBarChart
          subcategoryInfo={subcategoryInfo}
          title="Level 1 Bucket"
          chartTitle="Level 1 Bucket"
          filterKey={filterKey}
        />
      </div>
      <div className="row">
        <CardWithBarChart
          subcategoryInfo={primaryKey2Info}
          title="Level 2 Bucket"
          chartTitle="Level 2 Bucket"
          filterKey={filterKey}
        />
        <CardWithBarChart
          subcategoryInfo={subcategory2Info}
          title="Level 3 Bucket"
          chartTitle="Level 3 Bucket"
          filterKey={filterKey}
        />
      </div>
    </div>
  );
};

export default Cards;
