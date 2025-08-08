import { ArrowRight, Brain, Database, Edit, Loader, Plus, Save, X, Zap } from 'lucide-react';
import { useState } from 'react';
import * as XLSX from 'xlsx';

const ApSuite = () => {
  const [sourceData, setSourceData] = useState([]);
  const [targetData, setTargetData] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isMappingLoading, setIsMappingLoading] = useState(false);
  const [consoleMessages, setConsoleMessages] = useState([]);
  const [selectedRow, setSelectedRow] = useState(null);
  const [showAddForm, setShowAddForm] = useState(false);
  const [editingRows, setEditingRows] = useState(new Set());
  const [newRowData, setNewRowData] = useState({
    entityType: '',
    typeOfData: '',
    apsuiteName: '',
    sapTableName: '',
    sapFieldName: '',
    apiName: '',
    endpoint: ''
  });

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
      await simulateAISteps();

      const payload = sourceData.map(row => ({
        entityType: Object.values(row)[0] || '',
        typeOfData: Object.values(row)[1] || '',
        apsuiteName: Object.values(row)[2] || ''
      }));

      setConsoleMessages(prev => [...prev, {
        type: 'info',
        message: `📤 Sending ${payload.length} records for mapping...`
      }]);

      const response = await fetch(`${import.meta.env.VITE_API_URL}/apsuite-data/search`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          mappingData: payload
        })
      });

      const data = await response.json();

      // Create matched target data maintaining the same order as source data
      const matchedTargetData = sourceData.map((sourceRow, index) => {
        const sourceValues = Object.values(sourceRow);
        const sourceEntityType = sourceValues[0] || '';
        const sourceTypeOfData = sourceValues[1] || '';
        const sourceApsuiteName = sourceValues[2] || '';

        // Find matching target data
        const matchedTarget = data.find(targetRow => 
          targetRow.entityType === sourceEntityType &&
          targetRow.typeOfData === sourceTypeOfData &&
          targetRow.apsuiteName === sourceApsuiteName
        );

        return matchedTarget || {
          entityType: sourceEntityType,
          typeOfData: sourceTypeOfData,
          apsuiteName: sourceApsuiteName,
          sapTableName: '',
          sapFieldName: '',
          apiName: '',
          endpoint: ''
        };
      });

      setTargetData(matchedTargetData);
      
      setConsoleMessages(prev => [...prev, {
        type: 'success',
        message: `🎉 Successfully mapped ${matchedTargetData.length} records!`
      }, {
        type: 'response',
        message: `Mapping Results:\n${matchedTargetData.map((item, idx) => 
          `${idx + 1}. ${item.apsuiteName} → ${item.sapTableName || 'N/A'}.${item.sapFieldName || 'N/A'}`
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

  const handleSaveRow = async (rowIndex) => {
    const rowData = targetData[rowIndex];
    
    setConsoleMessages(prev => [...prev, {
      type: 'info',
      message: `💾 Saving row ${rowIndex + 1}...`
    }]);

    try {
      const response = await fetch(`${import.meta.env.VITE_API_URL}/apsuite-data/update`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(rowData)
      });

      if (response.ok) {
        setEditingRows(prev => {
          const newSet = new Set(prev);
          newSet.delete(rowIndex);
          return newSet;
        });
        
        setConsoleMessages(prev => [...prev, {
          type: 'success',
          message: `✅ Row ${rowIndex + 1} saved successfully!`
        }]);
      } else {
        throw new Error('Failed to save row');
      }
    } catch (error) {
      setConsoleMessages(prev => [...prev, {
        type: 'error',
        message: `❌ Error saving row: ${error.message}`
      }]);
    }
  };

  const handleAddNewRow = async () => {
    setConsoleMessages(prev => [...prev, {
      type: 'info',
      message: `➕ Adding new row...`
    }]);

    try {
      const response = await fetch(`${import.meta.env.VITE_API_URL}/apsuite-data/create`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(newRowData)
      });

      if (response.ok) {
        setTargetData(prev => [...prev, { ...newRowData }]);
        setSourceData(prev => [...prev, {
          [Object.keys(prev[0] || {})[0] || 'entityType']: newRowData.entityType,
          [Object.keys(prev[0] || {})[1] || 'typeOfData']: newRowData.typeOfData,
          [Object.keys(prev[0] || {})[2] || 'apsuiteName']: newRowData.apsuiteName
        }]);
        setNewRowData({
          entityType: '',
          typeOfData: '',
          apsuiteName: '',
          sapTableName: '',
          sapFieldName: '',
          apiName: '',
          endpoint: ''
        });
        setShowAddForm(false);
        
        setConsoleMessages(prev => [...prev, {
          type: 'success',
          message: `✅ New row added successfully!`
        }]);
      } else {
        throw new Error('Failed to add new row');
      }
    } catch (error) {
      setConsoleMessages(prev => [...prev, {
        type: 'error',
        message: `❌ Error adding row: ${error.message}`
      }]);
    }
  };

  const toggleEdit = (rowIndex) => {
    setEditingRows(prev => {
      const newSet = new Set(prev);
      if (newSet.has(rowIndex)) {
        newSet.delete(rowIndex);
      } else {
        newSet.add(rowIndex);
      }
      return newSet;
    });
  };

  const clearConsole = () => {
    setConsoleMessages([]);
  };

  return (
    <div className="h-full flex flex-col bg-gray-50">
      {/* Main Content - Two Blocks: Mapping Table + Console */}
      <div className="flex-1 flex gap-4 p-4 overflow-hidden">

        {/* Mapping Block - Takes most of the space */}
        <div className="flex-1 bg-white shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-gradient-to-r from-blue-50 to-green-50 border-b font-medium text-gray-800 flex items-center justify-between">
            <div className="flex items-center">
              <div className="w-3 h-3 bg-blue-500 rounded-full mr-2"></div>
              Source to Target Mapping
              <ArrowRight size={16} className="mx-2 text-gray-500" />
              <div className="w-3 h-3 bg-green-500 rounded-full"></div>
            </div>
            <button
              onClick={() => setShowAddForm(!showAddForm)}
              className="px-3 py-1 bg-blue-600 text-white rounded hover:bg-blue-700 flex items-center text-sm"
            >
              <Plus size={14} className="mr-1" />
              Add Row
            </button>
          </div>
          
          {/* Add Form */}
          {showAddForm && (
            <div className="p-4 bg-blue-25 border-b">
              <h4 className="text-sm font-medium text-gray-700 mb-3">Add New Mapping</h4>
              <div className="grid grid-cols-2 gap-4">
                <div className="space-y-2">
                  <label className="text-xs font-medium text-gray-600">Source Data</label>
                  <div className="space-y-2">
                    <input
                      type="text"
                      placeholder="Entity Type"
                      value={newRowData.entityType}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, entityType: e.target.value }))}
                      className="w-full p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500 text-sm"
                    />
                    <input
                      type="text"
                      placeholder="Type of Data"
                      value={newRowData.typeOfData}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, typeOfData: e.target.value }))}
                      className="w-full p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500 text-sm"
                    />
                    <input
                      type="text"
                      placeholder="AP Suite Name"
                      value={newRowData.apsuiteName}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, apsuiteName: e.target.value }))}
                      className="w-full p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-blue-500 text-sm"
                    />
                  </div>
                </div>
                <div className="space-y-2">
                  <label className="text-xs font-medium text-gray-600">Target Mapping</label>
                  <div className="grid grid-cols-2 gap-2">
                    <input
                      type="text"
                      placeholder="SAP Table"
                      value={newRowData.sapTableName}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, sapTableName: e.target.value }))}
                      className="p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-sm"
                    />
                    <input
                      type="text"
                      placeholder="SAP Field"
                      value={newRowData.sapFieldName}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, sapFieldName: e.target.value }))}
                      className="p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-sm"
                    />
                    <input
                      type="text"
                      placeholder="API Name"
                      value={newRowData.apiName}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, apiName: e.target.value }))}
                      className="p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-sm"
                    />
                    <input
                      type="text"
                      placeholder="Endpoint"
                      value={newRowData.endpoint}
                      onChange={(e) => setNewRowData(prev => ({ ...prev, endpoint: e.target.value }))}
                      className="p-2 border border-gray-300 rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-sm"
                    />
                  </div>
                </div>
              </div>
              <div className="flex gap-2 mt-4">
                <button
                  onClick={handleAddNewRow}
                  className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 flex items-center text-sm"
                >
                  <Save size={14} className="mr-1" />
                  Save Mapping
                </button>
                <button
                  onClick={() => setShowAddForm(false)}
                  className="px-4 py-2 bg-gray-500 text-white rounded hover:bg-gray-600 flex items-center text-sm"
                >
                  <X size={14} className="mr-1" />
                  Cancel
                </button>
              </div>
            </div>
          )}
          
          <div className="overflow-y-auto" style={{ maxHeight: "calc(100vh - 180px)" }}>
            {sourceData.length > 0 ? (
              <table className="w-full text-sm">
                <thead className="bg-gray-50 sticky top-0">
                  <tr>
                    <th colSpan="3" className="p-3 text-center font-medium text-blue-800 border-b bg-blue-50">
                      Source Data
                    </th>
                    <th className="p-3 w-8 border-b bg-gray-100"></th>
                    <th colSpan="4" className="p-3 text-center font-medium text-green-800 border-b bg-green-50">
                      Target Mapping
                    </th>
                    <th className="p-3 text-center font-medium text-gray-800 border-b bg-gray-50">Actions</th>
                  </tr>
                  <tr className="bg-gray-100">
                    <th className="p-2 text-left font-medium text-blue-700 border-b text-xs">Entity Type</th>
                    <th className="p-2 text-left font-medium text-blue-700 border-b text-xs">Type of Data</th>
                    <th className="p-2 text-left font-medium text-blue-700 border-b text-xs">AP Suite Name</th>
                    <th className="p-2 w-8 border-b">
                      <ArrowRight size={14} className="text-gray-400 mx-auto" />
                    </th>
                    <th className="p-2 text-left font-medium text-green-700 border-b text-xs">SAP Table</th>
                    <th className="p-2 text-left font-medium text-green-700 border-b text-xs">SAP Field</th>
                    <th className="p-2 text-left font-medium text-green-700 border-b text-xs">API Name</th>
                    <th className="p-2 text-left font-medium text-green-700 border-b text-xs">Endpoint</th>
                    <th className="p-2 text-center font-medium text-gray-700 border-b text-xs w-24">Actions</th>
                  </tr>
                </thead>
                <tbody>
                  {sourceData.map((sourceRow, idx) => {
                    const sourceValues = Object.values(sourceRow);
                    const targetRow = targetData[idx] || {};
                    const isEditing = editingRows.has(idx);
                    
                    return (
                      <tr
                        key={idx}
                        className={`transition-colors ${selectedRow === idx ? 'bg-purple-50 border-l-4 border-l-purple-400' : idx % 2 === 0 ? 'bg-white' : 'bg-gray-50'}`}
                      >
                        {/* Source Data */}
                        <td className="p-2 border-b text-gray-800 text-xs bg-blue-25">
                          {sourceValues[0] || 'N/A'}
                        </td>
                        <td className="p-2 border-b text-gray-800 text-xs bg-blue-25">
                          {sourceValues[1] || 'N/A'}
                        </td>
                        <td className="p-2 border-b text-gray-800 text-xs bg-blue-25 font-medium">
                          {sourceValues[2] || 'N/A'}
                        </td>
                        
                        {/* Arrow */}
                        <td className="p-2 border-b text-center bg-gray-50">
                          <ArrowRight size={12} className="text-gray-400 mx-auto" />
                        </td>
                        
                        {/* Target Data */}
                        <td className="p-2 border-b bg-green-25">
                          <input
                            type="text"
                            value={targetRow.sapTableName || ''}
                            onChange={(e) => handleTableFieldChange(idx, 'sapTableName', e.target.value)}
                            className={`w-full p-1 border rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-xs ${
                              isEditing ? 'border-green-500 bg-green-50' : 'border-gray-300 bg-white'
                            }`}
                            placeholder="SAP Table"
                            disabled={!isEditing}
                          />
                        </td>
                        <td className="p-2 border-b bg-green-25">
                          <input
                            type="text"
                            value={targetRow.sapFieldName || ''}
                            onChange={(e) => handleTableFieldChange(idx, 'sapFieldName', e.target.value)}
                            className={`w-full p-1 border rounded focus:outline-none focus:ring-1 focus:ring-green-500 text-xs ${
                              isEditing ? 'border-green-500 bg-green-50' : 'border-gray-300 bg-white'
                            }`}
                            placeholder="SAP Field"
                            disabled={!isEditing}
                          />
                        </td>
                        <td className="p-2 border-b text-gray-700 text-xs bg-green-25">
                          <div className="truncate" title={targetRow.apiName || 'N/A'}>
                            {targetRow.apiName || 'N/A'}
                          </div>
                        </td>
                        <td className="p-2 border-b text-gray-700 text-xs bg-green-25">
                          <div className="truncate" title={targetRow.endpoint || 'N/A'}>
                            {targetRow.endpoint || 'N/A'}
                          </div>
                        </td>
                        
                        {/* Actions */}
                        <td className="p-2 border-b text-center">
                          <div className="flex items-center justify-center gap-1">
                            <button
                              onClick={() => toggleEdit(idx)}
                              className="p-1 rounded hover:bg-gray-200 transition-colors"
                              title={isEditing ? "Cancel Edit" : "Edit Row"}
                            >
                              {isEditing ? <X size={12} className="text-red-500" /> : <Edit size={12} className="text-blue-500" />}
                            </button>
                            
                            {isEditing && (
                              <button
                                onClick={() => handleSaveRow(idx)}
                                className="p-1 rounded hover:bg-green-100 transition-colors"
                                title="Save Changes"
                              >
                                <Save size={12} className="text-green-600" />
                              </button>
                            )}
                            
                            <button
                              onClick={() => handleSuggestion(targetRow, idx)}
                              disabled={selectedRow === idx}
                              className={`p-1 rounded transition-colors ${
                                selectedRow === idx
                                  ? 'text-gray-400 cursor-not-allowed'
                                  : 'hover:bg-purple-100 text-purple-600'
                              }`}
                              title="Get AI Suggestion"
                            >
                              {selectedRow === idx ? (
                                <Loader className="animate-spin" size={12} />
                              ) : (
                                <Brain size={12} />
                              )}
                            </button>
                          </div>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            ) : (
              <div className="flex flex-col items-center justify-center h-64 text-gray-500">
                <Database size={48} className="mb-4 text-gray-300" />
                <div className="text-lg font-medium mb-2">No Data Available</div>
                <div className="text-sm">Upload an Excel file to start mapping</div>
              </div>
            )}
          </div>
        </div>

        {/* Console Block */}
        <div className="w-80 bg-gray-800 shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-gray-800 text-white font-medium flex items-center justify-between">
            <div className="flex items-center">
              <div className="w-3 h-3 bg-green-400 rounded-full mr-2 animate-pulse"></div>
              AI Console
            </div>
            <button
              onClick={clearConsole}
              className="text-xs px-2 py-1 bg-gray-600 hover:bg-gray-500 rounded transition-colors"
            >
              Clear
            </button>
          </div>
          <div className="p-4 bg-gray-900 text-green-400 font-mono text-xs overflow-y-auto" style={{ maxHeight: "calc(100vh - 180px)" }}>
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
                <div className="text-xs mb-4">Upload Excel → Do Mapping → View Results</div>
                <div className="text-xs text-gray-600">
                  Features:
                  <div className="mt-1 space-y-1">
                    <div>• AI-powered field mapping</div>
                    <div>• Real-time suggestions</div>
                    <div>• Batch processing</div>
                    <div>• Live editing</div>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Footer */}
      <div className="p-4 border-t bg-gradient-to-r from-gray-100 to-gray-200 flex items-center justify-between">
        <div className="flex items-center">
          <label htmlFor="file-upload" className={`cursor-pointer px-4 py-2 rounded border shadow text-white mr-4 transition-colors ${isLoading ? "bg-gray-400 cursor-not-allowed" : "bg-blue-600 hover:bg-blue-700"}`}>
            {isLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                Processing Excel...
              </div>
            ) : (
              <div className="flex items-center">
                <Database className="mr-2" size={16} />
                Upload Excel File
              </div>
            )}
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
            className={`px-6 py-2 rounded border shadow text-white font-medium transition-colors ${
              isMappingLoading || sourceData.length === 0
                ? "bg-gray-400 cursor-not-allowed"
                : "bg-purple-600 hover:bg-purple-700"
            }`}
          >
            {isMappingLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                AI Mapping...
              </div>
            ) : (
              <div className="flex items-center">
                <Zap className="mr-2" size={16} />
                Do AI Mapping
              </div>
            )}
          </button>
        </div>

        <div className="flex items-center space-x-4">
          <span className="text-sm text-gray-600">
            {sourceData.length > 0 ? `${sourceData.length} source records` : "No file selected"}
            {targetData.length > 0 && ` | ${targetData.length} mapped`}
          </span>
          <div className="flex items-center space-x-2 text-xs text-gray-500">
            <div className="flex items-center">
              <div className="w-2 h-2 bg-blue-500 rounded-full mr-1"></div>
              Source
            </div>
            <div className="flex items-center">
              <div className="w-2 h-2 bg-green-500 rounded-full mr-1"></div>
              Target
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ApSuite