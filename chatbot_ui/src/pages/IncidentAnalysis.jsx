import React, { useState } from "react";
import Cards from "../components/Cards";
import * as XLSX from "xlsx";
import "../App.css";

const IncidentAnalysis = () => {
  const [data, setData] = useState([]);
  const [piechartInfo, setPiechartInfo] = useState({});
  const [subcategoryInfo, setSubcategoryInfo] = useState({});
  const [subcategory2Info, setSubcategory2Info] = useState({});
  const [primaryKey2Info, setPrimaryKey2Info] = useState({});
  const [filterKey, setFilterKey] = useState(null);
  const [file, setFile] = useState(null);
  const [isLoading, setIsLoading] = useState(false);

  const handleFileUpload = async () => {
    if (file) {
      const arrayBuffer = await file.arrayBuffer();
      const workbook = XLSX.read(arrayBuffer, { type: "buffer" });
      console.log(workbook);

      const sheetNames = workbook.SheetNames;
      const firstSheet = workbook.Sheets[sheetNames[0]];

      const jsonData = XLSX.utils.sheet_to_json(firstSheet);

      const filteredData = jsonData.map((row) => ({
        Number: row["Number"],
        "Primary Key 1": row["Primary Key 1"] || "",
        "Primary Key 2": row["Primary Key 2"] || "",
        "Sub-Category 1": row["Sub-Category 1"] || "",
        "Sub-Category 2": row["Sub-Category 2"] || "",
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
        if (primaryKey2) {
          const compositeKey = `${primaryKey2} (${primaryKey})`;
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
        if (subcategory2) {
          const compositeKey = `${subcategory2} (${primaryKey})`;
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
    setFilterKey(selectedKey);
  };

  if (isLoading) {
    return (
      <div
      className="flex items-center justify-center h-screen font-semibold text-2xl"
       
      >
        Processing.....
      </div>
    );
  }

  return (
    <div className="App">
      <nav className="navbar">
        <h1>Data dump of incidents</h1>
        <div>
          <input
            type="file"
            accept=".xlsx, .xls, .csv"
            onChange={(e) => {
              const file = e.target.files[0];
              setFile(file);
            }}
          />
          <button
            className="bg-green-500 hover:bg-green-700 text-white font-bold py-1 px-4 rounded"
            onClick={() => {
              setIsLoading(true);
              setTimeout(() => {
                handleFileUpload();
                alert("Proceess Completed");
                setIsLoading(false);
              }, 5000);
            }}
          >
            Run
          </button>
          {filterKey && (
            <button onClick={() => setFilterKey(null)}>Clear Filter</button>
          )}
        </div>
      </nav>
      <Cards
        piechartInfo={piechartInfo}
        subcategoryInfo={subcategoryInfo}
        subcategory2Info={subcategory2Info}
        primaryKey2Info={primaryKey2Info}
        onPieChartSelect={handlePieChartSelect}
        filterKey={filterKey}
      />
    </div>
  );
};

export default IncidentAnalysis;
