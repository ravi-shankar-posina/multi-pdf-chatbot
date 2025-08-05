import { Loader, Search, ChevronRight, Zap, Database, Brain, CheckCircle } from 'lucide-react';
import { useEffect, useState } from 'react';
import * as XLSX from 'xlsx';

const ApSuite = () => {
  const [sourceData, setSourceData] = useState([]);
  const [targetData, setTargetData] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isMappingLoading, setIsMappingLoading] = useState(false);
  const [consoleMessages, setConsoleMessages] = useState([]);
  const [selectedRow, setSelectedRow] = useState(null);

  const handleFileUpload = (e) => {
    const file = e.target.files[0];
    if (!file) return;

    setIsLoading(true);

    const reader = new FileReader();
    reader.onload = (evt) => {
      const data = new Uint8Array(evt.target.result);
      const workbook = XLSX.read(data, { type: 'array' });
      const sheetName = workbook.SheetNames[0];
      const worksheet = workbook.Sheets[sheetName];
      const jsonData = XLSX.utils.sheet_to_json(worksheet, { header: 1 });
      const headers = jsonData[0];
      const rows = jsonData.slice(1).map((row) => {
        const obj = {};
        headers.forEach((header, index) => {
          obj[header] = row[index];
        });
        return obj;
      });

      setSourceData(rows);
      setIsLoading(false);
    };
    reader.readAsArrayBuffer(file);
  };

  const handleTableFieldChange = (rowIndex, fieldName, value) => {
    setTargetData(prev => prev.map((row, index) => {
      if (index === rowIndex) {
        return { ...row, [fieldName]: value };
      }
      return row;
    }));
  };

  const simulateAISteps = () => {
    return new Promise((resolve) => {
      const steps = [
        '🔍 Analyzing source data patterns...',
        '🧠 AI processing field mappings...',
        '🔗 Matching with SAP schema...',
        '✅ Mapping completed successfully!'
      ];

      let stepIndex = 0;
      const interval = setInterval(() => {
        if (stepIndex < steps.length) {
          setConsoleMessages(prev => [...prev, {
            type: 'info',
            message: steps[stepIndex]
          }]);
          stepIndex++;
        } else {
          clearInterval(interval);
          resolve();
        }
      }, 1000);
    });
  };

  const handleDoMapping = async () => {
    if (sourceData.length === 0) {
      setConsoleMessages(prev => [...prev, {
        type: 'error',
        message: '❌ Please upload an Excel file first'
      }]);
      return;
    }

    setIsMappingLoading(true);
    setConsoleMessages(prev => [...prev, {
      type: 'info',
      message: '🚀 Starting AI-powered mapping process...'
    }]);

    try {
      // Simulate AI processing steps
      await simulateAISteps();

      // Prepare payload from source data
      const payload = sourceData.map(row => ({
        entityType: Object.values(row)[0] || '', // First column
        typeOfData: Object.values(row)[1] || ''  // Second column
      }));

      setConsoleMessages(prev => [...prev, {
        type: 'info',
        message: `📤 Sending ${payload.length} records for mapping...`
      }]);

      // Make API call to get-data endpoint
      const response = await fetch(`${import.meta.env.VITE_API_URL}/apsuite-data`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          mappingData: payload
        })
      });

      const data = await response.json();

      setTargetData(data);
      
      setConsoleMessages(prev => [...prev, {
        type: 'success',
        message: `🎉 Successfully mapped ${data.length} records!`
      }, {
        type: 'response',
        message: `Mapping Results:\n${data.map((item, idx) => 
          `${idx + 1}. ${item.apsuiteName} → ${item.sapTableName}.${item.sapFieldName}`
        ).join('\n')}`
      }]);

    } catch (error) {
      setConsoleMessages(prev => [...prev, {
        type: 'error',
        message: `❌ Error during mapping: ${error.message}`
      }]);
    } finally {
      setIsMappingLoading(false);
    }
  };

  const handleSuggestion = async (row, index) => {
    setSelectedRow(index);
    setConsoleMessages(prev => [...prev, {
      type: 'info',
      message: `🔄 Generating suggestion for ${row.apsuiteName}...`
    }]);

    const query = `Provide the mapping for SAP table for the field name: ${row.apsuiteName || 'N/A'}, Field: ${row.sapFieldName || 'N/A'}`;

    try {
      const response = await fetch(`${import.meta.env.VITE_API_URL}/chat`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ query })
      });

      const data = await response.json();

      if (data && data.answer) {
        setConsoleMessages(prev => [...prev, {
          type: 'success',
          message: `✅ Suggestion Generated Successfully`
        }, {
          type: 'response',
          message: `💡 AI Response: ${data.answer}`
        }]);
      } else {
        setConsoleMessages(prev => [...prev, {
          type: 'warning',
          message: `⚠️ No suggestion received from API`
        }]);
      }

      setSelectedRow(null);
    } catch (error) {
      setConsoleMessages(prev => [...prev, {
        type: 'error',
        message: `❌ Error: ${error.message}`
      }]);
      setSelectedRow(null);
    }
  };

  const clearConsole = () => {
    setConsoleMessages([]);
  };

  return (
    <div className="h-full flex flex-col bg-gray-50">
      {/* Main Content - Three Blocks */}
      <div className="flex-1 flex gap-4 p-4 overflow-hidden">

        {/* Source Block - Narrower */}
        <div className="w-1/4 bg-white shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-blue-50 border-b font-medium text-blue-800 flex items-center">
            <div className="w-3 h-3 bg-blue-500 rounded-full mr-2"></div>
            Source Data
          </div>
          <div className="overflow-y-auto" style={{ maxHeight: "calc(100vh - 160px)" }}>
            {sourceData.length > 0 ? (
              <table className="w-full text-sm">
                <thead className="bg-blue-50 sticky top-0">
                  <tr>
                    <th className="p-2 text-left font-medium text-blue-800 border-b text-xs">Entity Type</th>
                    <th className="p-2 text-left font-medium text-blue-800 border-b text-xs">Type of Data</th>
                  </tr>
                </thead>
                <tbody>
                  {sourceData.map((row, idx) => {
                    const values = Object.values(row);
                    return (
                      <tr
                        key={idx}
                        className={`transition-colors ${idx % 2 === 0 ? 'bg-white' : 'bg-gray-50'}`}
                      >
                        <td className="p-2 border-b text-gray-800 text-xs">
                          {values[0] || 'N/A'}
                        </td>
                        <td className="p-2 border-b text-gray-800 text-xs">
                          {values[1] || 'N/A'}
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            ) : (
              <div className="flex items-center justify-center h-32 text-gray-500 text-sm">
                No source data available
              </div>
            )}
          </div>
        </div>

        {/* Target Block - Wider */}
        <div className="flex-1 bg-white shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-green-50 border-b font-medium text-green-800 flex items-center">
            <div className="w-3 h-3 bg-green-500 rounded-full mr-2"></div>
            Target Mapping
          </div>
          <div className="overflow-y-auto" style={{ maxHeight: "calc(100vh - 160px)" }}>
            {targetData.length > 0 ? (
              <table className="w-full text-sm">
                <thead className="bg-green-50 sticky top-0">
                  <tr>
                    <th className="p-3 text-left font-medium text-green-800 border-b">AP Suite Name</th>
                    <th className="p-3 text-left font-medium text-green-800 border-b">SAP Table</th>
                    <th className="p-3 text-left font-medium text-green-800 border-b">SAP Field</th>
                    <th className="p-3 text-left font-medium text-green-800 border-b">API Name</th>
                    <th className="p-3 text-left font-medium text-green-800 border-b">Endpoint</th>
                    <th className="p-3 text-center font-medium text-green-800 border-b w-16">Action</th>
                  </tr>
                </thead>
                <tbody>
                  {targetData.map((row, idx) => (
                    <tr
                      key={idx}
                      className={`transition-colors ${selectedRow === idx ? 'bg-green-100' : idx % 2 === 0 ? 'bg-white' : 'bg-gray-50'}`}
                    >
                      <td className="p-3 border-b text-gray-800 font-medium">
                        {row.apsuiteName || 'N/A'}
                      </td>
                      <td className="p-3 border-b text-gray-800">
                        <input
                          type="text"
                          value={row.sapTableName || ''}
                          onChange={(e) => handleTableFieldChange(idx, 'sapTableName', e.target.value)}
                          className='w-full p-2 border border-gray-300 rounded focus:outline-none focus:ring-2 focus:ring-green-500 focus:border-transparent'
                          placeholder="SAP Table"
                        />
                      </td>
                      <td className="p-3 border-b text-gray-800">
                        <input
                          type="text"
                          value={row.sapFieldName || ''}
                          onChange={(e) => handleTableFieldChange(idx, 'sapFieldName', e.target.value)}
                          className='w-full p-2 border border-gray-300 rounded focus:outline-none focus:ring-2 focus:ring-green-500 focus:border-transparent'
                          placeholder="SAP Field"
                        />
                      </td>
                      <td className="p-3 border-b text-gray-800">
                        {row.apiName || 'N/A'}
                      </td>
                      <td className="p-3 border-b text-gray-800">
                        {row.endpoint || 'N/A'}
                      </td>
                      <td className="p-3 border-b text-center">
                        <button
                          onClick={() => handleSuggestion(row, idx)}
                          disabled={selectedRow === idx}
                          className={`p-2 rounded-full transition-colors ${selectedRow === idx
                            ? 'bg-gray-300 text-gray-500 cursor-not-allowed'
                            : 'bg-green-600 text-white hover:bg-green-700'
                            }`}
                          title="Get AI Suggestion"
                        >
                          {selectedRow === idx ? (
                            <Loader className="animate-spin" size={16} />
                          ) : (
                            <Brain size={16} />
                          )}
                        </button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            ) : (
              <div className="flex items-center justify-center h-32 text-gray-500">
                No target data available
              </div>
            )}
          </div>
        </div>

        {/* Console Block */}
        <div className="w-1/3 bg-gray-800 shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-gray-800 text-white font-medium flex items-center justify-between">
            <div className="flex items-center">
              <div className="w-3 h-3 bg-gray-400 rounded-full mr-2"></div>
              AI Console
            </div>
            <button
              onClick={clearConsole}
              className="text-xs px-2 py-1 bg-gray-600 hover:bg-gray-500 rounded"
            >
              Clear
            </button>
          </div>
          <div className="p-4 bg-gray-900 text-green-400 font-mono text-xs overflow-y-auto" style={{ maxHeight: "calc(100vh - 160px)" }}>
            {consoleMessages.length > 0 ? (
              <div className="space-y-3">
                {consoleMessages.map((msg, idx) => (
                  <div key={idx} className={`${msg.type === 'error' ? 'text-red-400' :
                    msg.type === 'success' ? 'text-green-400' :
                      msg.type === 'warning' ? 'text-yellow-400' :
                        msg.type === 'response' ? 'text-cyan-300 bg-gray-800 p-3 rounded border-l-4 border-cyan-400' :
                          'text-blue-400'
                    }`}>
                    {msg.type === 'response' ? (
                      <div>
                        <div className="font-bold text-cyan-400 mb-2">AI Mapping Results:</div>
                        <div className="text-gray-100 font-sans text-sm leading-relaxed whitespace-pre-wrap">
                          {msg.message.replace('💡 AI Response: ', '')}
                        </div>
                      </div>
                    ) : (
                      <div>
                        <span className="text-gray-500">[{new Date().toLocaleTimeString()}]</span> {msg.message}
                      </div>
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-gray-500">
                <div className="mb-2">🤖 AI Console Ready</div>
                <div className="text-xs">Upload Excel → Do Mapping → View Results</div>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Footer */}
      <div className="p-4 border-t bg-gray-100 flex items-center justify-between">
        <div className="flex items-center">
          <label htmlFor="file-upload" className={`cursor-pointer px-4 py-2 rounded border shadow text-white mr-4 ${isLoading ? "bg-gray-400 cursor-not-allowed" : "bg-blue-600 hover:bg-blue-700"}`}>
            {isLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                Processing
              </div>
            ) : "Upload Excel File"}
            <input
              id="file-upload"
              type="file"
              accept=".csv, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, application/vnd.ms-excel"
              onChange={handleFileUpload}
              className="hidden"
              disabled={isLoading}
            />
          </label>

          <button
            onClick={handleDoMapping}
            disabled={isMappingLoading || sourceData.length === 0}
            className={`px-6 py-2 rounded border shadow text-white font-medium ${
              isMappingLoading || sourceData.length === 0
                ? "bg-gray-400 cursor-not-allowed"
                : "bg-purple-600 hover:bg-purple-700"
            }`}
          >
            {isMappingLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                Mapping...
              </div>
            ) : (
              <div className="flex items-center">
                <Zap className="mr-2" size={16} />
                Do Mapping
              </div>
            )}
          </button>
        </div>

        <span className="text-sm text-gray-600">
          {sourceData.length > 0 ? `${sourceData.length} source records` : "No file selected"}
          {targetData.length > 0 && ` | ${targetData.length} mapped`}
        </span>
      </div>
    </div>
  );
};

export default ApSuite;