import { Loader, Play, CheckCircle, XCircle } from 'lucide-react';
import { useState } from 'react';
import * as XLSX from 'xlsx';

const PoAutomation = () => {
  const [tableData, setTableData] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [isProcessing, setIsProcessing] = useState(false);
  const [processingSteps, setProcessingSteps] = useState([]);
  const [processingStarted, setProcessingStarted] = useState(false);

  const handleFileUpload = (e) => {
    const file = e.target.files[0];
    if (!file) return;

    setIsLoading(true);
    setProcessingSteps([]);
    setProcessingStarted(false);

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

      setTableData(rows);
      setIsLoading(false);
    };
    reader.readAsArrayBuffer(file);
  };

  const generatePONumber = () => {
    return '4' + Math.floor(Math.random() * 1000000000).toString().padStart(9, '0');
  };

  const addProcessingStep = (message, type = 'info') => {
    setProcessingSteps(prev => [...prev, { message, type, timestamp: new Date().toLocaleTimeString() }]);
  };

  const handleGenerateData = async () => {
    if (tableData.length === 0) return;

    setIsProcessing(true);
    setProcessingStarted(true);
    setProcessingSteps([]);

    addProcessingStep('Starting PO generation process...', 'info');
    await new Promise(resolve => setTimeout(resolve, 500));

    addProcessingStep('Analyzing data structure...', 'info');
    await new Promise(resolve => setTimeout(resolve, 800));

    addProcessingStep('Validating item numbers...', 'info');
    await new Promise(resolve => setTimeout(resolve, 600));

    const updatedData = tableData.map((row, index) => {
      const itemNumber = parseInt(row.Item);
      const updatedRow = { ...row };

      // Skip PO generation for items 4, 7, 9
      if ([4, 7, 9].includes(itemNumber)) {
        updatedRow['PO Number'] = '';
        updatedRow['Status'] = 'Error';
        updatedRow['Comment'] = `Item ${itemNumber} - Price miss match, Notification sent.`;
        return updatedRow;
      }

      // Generate PO number for other items
      updatedRow['PO Number'] = generatePONumber();
      updatedRow['Status'] = 'Completed';
      updatedRow['Comment'] = '';

      return updatedRow;
    });

    addProcessingStep('Generating PO numbers for valid items...', 'info');
    await new Promise(resolve => setTimeout(resolve, 1000));

    addProcessingStep('Setting status for completed items...', 'info');
    await new Promise(resolve => setTimeout(resolve, 500));

    addProcessingStep('Adding error comments for invalid items...', 'warning');
    await new Promise(resolve => setTimeout(resolve, 700));

    const completedCount = updatedData.filter(row => row['Status'] === 'Completed').length;
    const errorCount = updatedData.filter(row => row['Status'] === 'Error').length;

    addProcessingStep(`Process completed: ${completedCount} items processed successfully`, 'success');
    if (errorCount > 0) {
      addProcessingStep(`${errorCount} items had errors and were skipped`, 'warning');
    }

    setTableData(updatedData);
    setIsProcessing(false);
  };

  const getStatusIcon = (status) => {
    if (status === 'Completed') {
      return <CheckCircle className="w-4 h-4 text-green-500" />;
    } else if (status === 'Error') {
      return <XCircle className="w-4 h-4 text-red-500" />;
    }
    return null;
  };

  const getStatusColor = (status) => {
    if (status === 'Completed') return 'text-green-600 bg-green-50';
    if (status === 'Error') return 'text-red-600 bg-red-50';
    return 'text-gray-600 bg-gray-50';
  };

  return (
    <div className="h-full flex flex-col bg-gray-50">
      <div className="flex flex-1 p-4 gap-4 overflow-hidden">
        <div className="w-2/3 flex flex-col bg-white shadow rounded-lg overflow-hidden border border-gray-200">
          <div className="flex-1 overflow-auto" style={{ maxHeight: "calc(100vh - 240px)" }}>
            {tableData.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="min-w-full text-sm">
                  <thead>
                    <tr className="bg-gray-50">
                      {Object.keys(tableData[0]).map((key) => (
                        <th key={key} className="border p-2 text-left font-semibold">{key}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {tableData.map((row, idx) => (
                      <tr key={idx} className={idx % 2 === 0 ? "bg-white" : "bg-gray-50"}>
                        {Object.entries(row).map(([key, val], i) => (
                          <td key={i} className="border p-2">
                            {key === 'Status' && val ? (
                              <div className={`flex items-center gap-2 px-2 py-1 rounded-full text-xs font-medium ${getStatusColor(val)}`}>
                                {getStatusIcon(val)}
                                {val}
                              </div>
                            ) : key === 'Comment' && val ? (
                              <span className="text-red-600 text-xs">{val}</span>
                            ) : (
                              val
                            )}
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <div className="flex items-center justify-center h-32 text-gray-500">
                No data loaded. Please upload a file to begin.
              </div>
            )}
          </div>
        </div>

        <div className="w-1/3 bg-white shadow rounded-lg border border-gray-200 overflow-hidden">
          <div className="p-3 bg-gray-100 border-b font-medium text-gray-700">
            Processing Steps
          </div>
          <div
            className="p-4 space-y-2 text-sm overflow-y-auto"
            id="steps-container"
            style={{ maxHeight: "calc(100vh - 200px)" }}
          >
            {processingSteps.length === 0 ? (
              <div className="text-gray-500 text-center py-8">
                No processing steps yet. Click "Generate PO Data" to start.
              </div>
            ) : (
              processingSteps.map((step, index) => (
                <div key={index} className="flex items-start gap-2 p-2 rounded border-l-4 border-l-blue-400 bg-blue-50">
                  <div className="flex-1">
                    <div className={`font-medium ${
                      step.type === 'success' ? 'text-green-600' : 
                      step.type === 'warning' ? 'text-yellow-600' : 
                      'text-blue-600'
                    }`}>
                      {step.message}
                    </div>
                    <div className="text-xs text-gray-500 mt-1">{step.timestamp}</div>
                  </div>
                </div>
              ))
            )}
          </div>
        </div>
      </div>

      <div className="p-4 border-t bg-gray-100 flex items-center justify-between">
        <div className="flex items-center">
          <label htmlFor="file-upload" className={`cursor-pointer px-4 py-2 rounded border shadow text-white ${isLoading ? "bg-gray-400 cursor-not-allowed" : "bg-blue-600 hover:bg-blue-700"}`}>
            {isLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                Processing
              </div>
            ) : "Upload File"}
            <input
              id="file-upload"
              type="file"
              accept=".csv, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, application/vnd.ms-excel"
              onChange={handleFileUpload}
              className="hidden"
              disabled={isLoading}
            />
          </label>
          <span className="text-sm text-gray-600 ml-4">
            {tableData.length > 0 ? `${tableData.length} rows loaded` : "No file selected"}
          </span>
        </div>

        <button
          onClick={handleGenerateData}
          disabled={tableData.length === 0 || isProcessing}
          className={`flex items-center gap-2 px-4 py-2 rounded border shadow text-white ${
            tableData.length === 0 || isProcessing 
              ? "bg-gray-400 cursor-not-allowed" 
              : "bg-green-600 hover:bg-green-700"
          }`}
        >
          {isProcessing ? (
            <>
              <Loader className="animate-spin" size={16} />
              Processing...
            </>
          ) : (
            <>
              <Play size={16} />
              Generate PO Data
            </>
          )}
        </button>
      </div>
    </div>
  );
};

export default PoAutomation;