import React, { useState } from "react";
import Cards from "../components/Cards"; 
import * as XLSX from "xlsx";
import '../App.css';

const IncidentAnalysis = () => {
  const [data, setData] = useState([]);
  const [piechartInfo, setPiechartInfo] = useState({});
  const [subcategoryInfo, setSubcategoryInfo] = useState({});
  const [subcategory2Info, setSubcategory2Info] = useState({});
  const [primaryKey2Info, setPrimaryKey2Info] = useState({});
  const [filterKey, setFilterKey] = useState(null);

  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (file) {
      const reader = new FileReader();
      reader.onload = (e) => {
        const binaryStr = e.target.result;
        const workbook = XLSX.read(binaryStr, { type: "binary" });

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
            primaryKeyCounts[primaryKey] = (primaryKeyCounts[primaryKey] || 0) + 1;
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
            subcategoryCounts[compositeKey] = (subcategoryCounts[compositeKey] || 0) + 1;
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
            primaryKey2Counts[compositeKey] = (primaryKey2Counts[compositeKey] || 0) + 1;
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
            subcategory2Counts[compositeKey] = (subcategory2Counts[compositeKey] || 0) + 1;
          }
        });

        const repeatedSubcategory2 = Object.fromEntries(
          Object.entries(subcategory2Counts).filter(([_, count]) => count > 1)
        );

        setSubcategory2Info(repeatedSubcategory2);
      };

      reader.readAsBinaryString(file);
    }
  };

  const handlePieChartSelect = (selectedKey) => {
    setFilterKey(selectedKey);
  };

  return (
    <div className="overflow-y-scroll">
      <nav className="navbar">
        <h1>Data dump of incidents</h1>
        <div>
          <input 
            type="file" 
            accept=".xlsx, .xls, .csv" 
            onChange={handleFileUpload} 
          />
          {filterKey && (
            <button onClick={() => setFilterKey(null)}>
              Clear Filter
            </button>
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