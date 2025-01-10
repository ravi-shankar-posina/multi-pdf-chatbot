import React, { useState } from "react";
import * as XLSX from "xlsx";
import "../App.css";
import Cards from "../components/Cards";
import TabNavigation from "../components/TabNavigation";
import ComplexityCards from "../components/ComplexityCards";

const COLORS = {
  primary: "#FFA500",
  secondary: "#00AECF",
  dark: "#073161",
};

const IncidentAnalysis = () => {
  const [data, setData] = useState([]);
  const [piechartInfo, setPiechartInfo] = useState({});
  const [subcategoryInfo, setSubcategoryInfo] = useState({});
  const [subcategory2Info, setSubcategory2Info] = useState({});
  const [primaryKey2Info, setPrimaryKey2Info] = useState({});
  const [complexityPiechartInfo, setComplexityPiechartInfo] = useState({});
  const [complexityBarChart1Info, setComplexityBarChart1Info] = useState({});
  const [filters, setFilters] = useState({
    primaryKey1: null,
    subCategory1: null,
  });
  const [complexityFilters, setComplexityFilters] = useState({
    Location: null,
    Complexity: null,
    RISE: null,
  });
  const [file, setFile] = useState(null);
  const [isLoading, setIsLoading] = useState(false);
  const [activeTab, setActiveTab] = useState("analysis");

  const handleFileUpload = async () => {
    if (file) {
      const arrayBuffer = await file.arrayBuffer();
      const workbook = XLSX.read(arrayBuffer, { type: "buffer" });
      const sheetNames = workbook.SheetNames;
      const firstSheet = workbook.Sheets[sheetNames[0]];

      const jsonData = XLSX.utils.sheet_to_json(firstSheet);

      const filteredData = jsonData.map((row) => ({
        Number: row["Number"],
        "Primary Key 1": row["Primary Key 1"] || "",
        "Primary Key 2": row["Primary Key 2"] || "",
        "Sub-Category 1": row["Sub-Category 1"] || "",
        "Sub-Category 2": row["Sub-Category 2"] || "",
        Location: row["Location"] || "",
        Complexity: row["Complexity"] || "",
        RISE: row["RISE"] || "",
      }));

      setData(filteredData);
      // Primary Key 1 Pie Chart
      const primaryKeyCounts = {};
      filteredData.forEach((row) => {
        const primaryKey = row["Primary Key 1"];
        if (primaryKey) {
          primaryKeyCounts[primaryKey] =
            (primaryKeyCounts[primaryKey] || 0) + 1;
        }
      });

      const repeatedPrimaryKeys = Object.fromEntries(
        Object.entries(primaryKeyCounts).filter(([_, count]) => count > 1)
      );

      setPiechartInfo(repeatedPrimaryKeys);
      //Location Pie Chart
      const locationCounts = {};
      filteredData.forEach((row) => {
        const location = row["Location"];
        if (location) {
          locationCounts[location] = (locationCounts[location] || 0) + 1;
        }
      });

      const repeatedLocations = Object.fromEntries(
        Object.entries(locationCounts).filter(([_, count]) => count > 1)
      );
      setComplexityPiechartInfo(repeatedLocations);
      //complexity bar chart
      // First, create a nested structure to organize the data
      const complexityLocationRiseCounts = {};
      filteredData.forEach((row) => {
        const complexity = row["Complexity"];
        const location = row["Location"];
        const rise = row["RISE"];

        if (complexity && location && rise) {
          // Initialize nested objects if they don't exist
          if (!complexityLocationRiseCounts[complexity]) {
            complexityLocationRiseCounts[complexity] = {};
          }
          if (!complexityLocationRiseCounts[complexity][location]) {
            complexityLocationRiseCounts[complexity][location] = {
              Innovate: 0,
              Reduce: 0,
              Simplify: 0,
            };
          }

          // Increment the count for the specific RISE value
          complexityLocationRiseCounts[complexity][location][rise]++;
        }
      });

      // First, create a data structure to count by complexity and RISE
      const complexityCounts = {};
      filteredData.forEach((row) => {
        const complexity = row["Complexity"];
        const location = row["Location"];
        const rise = row["RISE"];

        if (complexity && location && rise) {
          // For the pie chart total counts
          const compositeKey = `${complexity} (${location})`;
          if (!complexityCounts[compositeKey]) {
            complexityCounts[compositeKey] = {
              Innovate: 0,
              Reduce: 0,
              Simplify: 0,
              total: 0,
            };
          }
          complexityCounts[compositeKey][rise]++;
          complexityCounts[compositeKey].total++;
        }
      });

      // Filter to only include entries with total count > 1
      const repeatedComplexity = Object.fromEntries(
        Object.entries(complexityCounts)
          .filter(([_, counts]) => counts.total > 1)
          .map(([key, counts]) => {
            // Extract just the RISE counts, excluding the total
            const { total, ...riseCounts } = counts;
            return [key, riseCounts];
          })
      );

      setComplexityBarChart1Info(repeatedComplexity);
      // Sub-Category 1 Bar Chart
      const subcategoryCounts = {};
      filteredData.forEach((row) => {
        const subcategory = row["Sub-Category 1"];
        const primaryKey = row["Primary Key 1"];
        if (subcategory) {
          const compositeKey = `${subcategory} (${primaryKey})`;
          subcategoryCounts[compositeKey] =
            (subcategoryCounts[compositeKey] || 0) + 1;
        }
      });

      const repeatedSubcategories = Object.fromEntries(
        Object.entries(subcategoryCounts).filter(([_, count]) => count > 1)
      );

      setSubcategoryInfo(repeatedSubcategories);

      // Primary Key 2 Bar Chart
      const primaryKey2Counts = {};
      filteredData.forEach((row) => {
        const primaryKey2 = row["Primary Key 2"];
        const primaryKey = row["Primary Key 1"];
        const subCategory1 = row["Sub-Category 1"];
        if (primaryKey2) {
          const compositeKey = `${primaryKey2} (${primaryKey}) (${subCategory1})`;
          primaryKey2Counts[compositeKey] =
            (primaryKey2Counts[compositeKey] || 0) + 1;
        }
      });

      const repeatedPrimaryKey2 = Object.fromEntries(
        Object.entries(primaryKey2Counts).filter(([_, count]) => count > 1)
      );

      setPrimaryKey2Info(repeatedPrimaryKey2);

      // Sub-Category 2 Bar Chart
      const subcategory2Counts = {};
      filteredData.forEach((row) => {
        const subcategory2 = row["Sub-Category 2"];
        const primaryKey = row["Primary Key 1"];
        const subCategory1 = row["Sub-Category 1"];
        const primaryKey2 = row["Primary Key 2"];
        if (subcategory2) {
          const compositeKey = `${subcategory2} (${primaryKey}) (${subCategory1}) (${primaryKey2})`;
          subcategory2Counts[compositeKey] =
            (subcategory2Counts[compositeKey] || 0) + 1;
        }
      });

      const repeatedSubcategory2 = Object.fromEntries(
        Object.entries(subcategory2Counts).filter(([_, count]) => count > 1)
      );

      setSubcategory2Info(repeatedSubcategory2);
    }
  };

  const handlePieChartSelect = (selectedKey) => {
    setFilters((prev) => ({
      ...prev,
      primaryKey1: selectedKey,
    }));
  };
  const handleComplexityPieChartSelect = (selectedKey) => {
    setComplexityFilters((prev) => ({
      ...prev,
      complexity: selectedKey,
      RISE: selectedKey,
    }));
  };
  const handleSubCategory1Select = (selectedKey) => {
    setFilters((prev) => ({
      ...prev,
      subCategory1: selectedKey,
    }));
  };

  const clearFilters = () => {
    setFilters({
      primaryKey1: null,
      subCategory1: null,
    });

    // Reset level-specific states
    setPiechartInfo(
      data.reduce((acc, row) => {
        const primaryKey = row["Primary Key 1"];
        if (primaryKey) {
          acc[primaryKey] = (acc[primaryKey] || 0) + 1;
        }
        return acc;
      }, {})
    );

    setSubcategoryInfo(
      data.reduce((acc, row) => {
        const subcategory = row["Sub-Category 1"];
        const primaryKey = row["Primary Key 1"];
        if (subcategory) {
          const compositeKey = `${subcategory} (${primaryKey})`;
          acc[compositeKey] = (acc[compositeKey] || 0) + 1;
        }
        return acc;
      }, {})
    );

    setPrimaryKey2Info(
      data.reduce((acc, row) => {
        const primaryKey2 = row["Primary Key 2"];
        const primaryKey = row["Primary Key 1"];
        const subCategory1 = row["Sub-Category 1"];
        if (primaryKey2) {
          const compositeKey = `${primaryKey2} (${primaryKey}) (${subCategory1})`;
          acc[compositeKey] = (acc[compositeKey] || 0) + 1;
        }
        return acc;
      }, {})
    );

    setSubcategory2Info(
      data.reduce((acc, row) => {
        const subcategory2 = row["Sub-Category 2"];
        const primaryKey = row["Primary Key 1"];
        const subCategory1 = row["Sub-Category 1"];
        const primaryKey2 = row["Primary Key 2"];
        if (subcategory2) {
          const compositeKey = `${subcategory2} (${primaryKey}) (${subCategory1}) (${primaryKey2})`;
          acc[compositeKey] = (acc[compositeKey] || 0) + 1;
        }
        return acc;
      }, {})
    );
  };

  const renderTabContent = () => {
    switch (activeTab) {
      case "analysis":
        return (
          <Cards
            piechartInfo={piechartInfo}
            subcategoryInfo={subcategoryInfo}
            subcategory2Info={subcategory2Info}
            primaryKey2Info={primaryKey2Info}
            onPieChartSelect={handlePieChartSelect}
            onSubCategory1Select={handleSubCategory1Select}
            filters={filters}
          />
        );
      case "complexity":
        return (
          <ComplexityCards
            complexityPiechartInfo={complexityPiechartInfo}
            complexityBarChart1Info={complexityBarChart1Info}
            onPieChartSelect={handleComplexityPieChartSelect}
            filters={complexityFilters}
          />
        );
      case "resourcing":
        return (
          <div className="flex items-center justify-center h-[calc(100vh-150px)]">
            <h2 className="text-2xl text-gray-500">
              Resourcing View Coming Soon
            </h2>
          </div>
        );
      default:
        return null;
    }
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-screen">
        Loading....
      </div>
    );
  }

  return (
    <div className="w-full">
      <nav className="sticky top-0 z-10 w-full flex justify-between items-center p-4 border-b bg-white shadow-md">
        <TabNavigation activeTab={activeTab} onTabChange={setActiveTab} />
        <div className="flex items-center space-x-4">
          <label className="relative cursor-pointer">
            <input
              type="file"
              accept=".xlsx, .xls, .csv"
              onChange={(e) => {
                const file = e.target.files[0];
                setFile(file);
              }}
              className="absolute inset-0 w-full h-full opacity-0 cursor-pointer"
            />
            <div className="flex items-center px-4 py-2 text-blue-600 font-medium bg-blue-100 rounded-lg transition-all hover:bg-blue-200">
              {file ? file.name : "Choose File"}
            </div>
          </label>
          <button
            className={`px-4 py-2 rounded-lg font-medium text-white transition-all ${
              file
                ? "bg-green-500 hover:bg-green-600"
                : "bg-green-300 cursor-not-allowed"
            }`}
            onClick={() => {
              setIsLoading(true);
              setTimeout(() => {
                handleFileUpload();
                alert("Process Completed");
                setIsLoading(false);
              }, 5000);
            }}
            disabled={!file}
          >
            Run
          </button>

          {(filters.primaryKey1 || filters.subCategory1) && (
            <button
              className="px-4 py-2 rounded-lg font-medium text-white bg-gray-700 hover:bg-gray-800 transition-all"
              onClick={clearFilters}
            >
              Clear Filters
            </button>
          )}
        </div>
      </nav>
      <div className="mt-4">{renderTabContent()}</div>
    </div>
  );
};

export default IncidentAnalysis;
