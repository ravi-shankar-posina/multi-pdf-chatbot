import { CheckCircle, Loader, Play, XCircle } from 'lucide-react';
import { useState } from 'react';

const PoAutomation = () => {
  const [tableData, setTableData] = useState([
    {
      "PR Number": 10000028,
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 100,
      "Purchase Order Quantity": 0,
      "Total Value": 1000.00,
      "Assigned Supplier": "",
      "Delivery Date": "18-12-2025",
      "Plant": 1710
    },
    {
      "PR Number": "10000061",
      "Material": "MATPRIMA-",
      "Product Group": "L002",
      "Quantity": 14,
      "Purchase Order Quantity": 0,
      "Total Value": 245.00,
      "Assigned Supplier": "",
      "Delivery Date": "09-11-2025",
      "Plant": 1710
    },
    {
      "PR Number": "10000091",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 56,
      "Purchase Order Quantity": 56,
      "Total Value": 56.00,
      "Assigned Supplier": "",
      "Delivery Date": "30-12-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000112",
      "Material": "MATPRIMA-(ALM)",
      "Product Group": "L002",
      "Quantity": 11.000,
      "Purchase Order Quantity": 0.000,
      "Total Value": 11.00,
      "Assigned Supplier": "",
      "Delivery Date": "27-11-2025",
      "Plant": 6000
    },
    {
      "PR Number": "10000113",
      "Material": "MATPRIMA-f",
      "Product Group": "ALM",
      "Quantity": 23.000,
      "Purchase Order Quantity": 0.000,
      "Total Value": 23.00,
      "Assigned Supplier": "",
      "Delivery Date": "27-12-2025",
      "Plant": 6000
    },
    {
      "PR Number": 10000114,
      "Material": "MATPRIMA-f",
      "Product Group": "ALM",
      "Quantity": 11.000,
      "Purchase Order Quantity": 0.000,
      "Total Value": 11.00,
      "Assigned Supplier": "",
      "Delivery Date": "27-11-2025",
      "Plant": 6000
    },
    {
      "PR Number": "10000160",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 13,
      "Purchase Order Quantity": 13,
      "Total Value": 26.00,
      "Assigned Supplier": "",
      "Delivery Date": "08-12-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000160",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 45.000,
      "Purchase Order Quantity": 45.000,
      "Total Value": 45.00,
      "Assigned Supplier": "",
      "Delivery Date": "08-12-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000160",
      "Material": "MATPRIMA3",
      "Product Group": "L001",
      "Quantity": 23.000,
      "Purchase Order Quantity": 0.000,
      "Total Value": 46.00,
      "Assigned Supplier": "",
      "Delivery Date": "08-12-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000166",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 50.000,
      "Purchase Order Quantity": 50.000,
      "Total Value": 50.00,
      "Assigned Supplier": "",
      "Delivery Date": "05-11-2025",
      "Plant": 1720
    },
    {
      "PR Number": 10000168,
      "Material": "PRODTERM",
      "Product Group": "L004",
      "Quantity": 2,
      "Purchase Order Quantity": 0,
      "Total Value": 198.00,
      "Assigned Supplier": "",
      "Delivery Date": "04-11-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000177",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 14.000,
      "Purchase Order Quantity": 14.000,
      "Total Value": 42.00,
      "Assigned Supplier": "",
      "Delivery Date": "04-11-2025",
      "Plant": 1720
    },
    {
      "PR Number": "10000177",
      "Material": "MATPRIMA3",
      "Product Group": "L002",
      "Quantity": 38,
      "Purchase Order Quantity": 38,
      "Total Value": 190.00,
      "Assigned Supplier": "",
      "Delivery Date": "05-11-2025",
      "Plant": 1720
    }
  ]);
  const [isProcessing, setIsProcessing] = useState(false);
  const [processingSteps, setProcessingSteps] = useState([]);
  const [processingStarted, setProcessingStarted] = useState(false);
  const [currentlyProcessingIndex, setCurrentlyProcessingIndex] = useState(-1);
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
    setCurrentlyProcessingIndex(-1);

    addProcessingStep('Starting PO generation process...', 'info');
    await new Promise(resolve => setTimeout(resolve, 800));

    addProcessingStep('Analyzing data structure...', 'info');
    await new Promise(resolve => setTimeout(resolve, 1000));

    addProcessingStep('Validating item numbers...', 'info');
    await new Promise(resolve => setTimeout(resolve, 1200));

    const updatedData = [...tableData];

    // Process each row with individual time intervals
    for (let index = 0; index < updatedData.length; index++) {
      const row = updatedData[index];
      const PR_Number = row['PR Number'];

      setCurrentlyProcessingIndex(index);
      addProcessingStep(`Processing item ${index + 1} of ${updatedData.length}: PR ${PR_Number}`, 'info');
      await new Promise(resolve => setTimeout(resolve, 600));

      // Skip PO generation for items with specific PR numbers
      if ([10000028, 10000168, 10000114].includes(PR_Number)) {
        updatedData[index] = {
          ...row,
          'PO Number': '',
          'Status': 'Error',
          'Comment': `Item ${PR_Number} - Price mismatch, Notification sent.`
        };
        addProcessingStep(`Item ${PR_Number} - Error detected, skipping PO generation`, 'warning');
        await new Promise(resolve => setTimeout(resolve, 400));
      } else {
        // Generate PO number for valid items
        const poNumber = generatePONumber();
        updatedData[index] = {
          ...row,
          'PO Number': poNumber,
          'Status': 'Completed',
          'Comment': ''
        };
        addProcessingStep(`Item ${PR_Number} - PO ${poNumber} generated successfully`, 'success');
        await new Promise(resolve => setTimeout(resolve, 500));
      }

      // Update table data progressively
      setTableData([...updatedData]);
      await new Promise(resolve => setTimeout(resolve, 300));
    }

    setCurrentlyProcessingIndex(-1);
    addProcessingStep('Finalizing process...', 'info');
    await new Promise(resolve => setTimeout(resolve, 800));

    const completedCount = updatedData.filter(row => row['Status'] === 'Completed').length;
    const errorCount = updatedData.filter(row => row['Status'] === 'Error').length;

    addProcessingStep(`Process completed: ${completedCount} items processed successfully`, 'success');
    if (errorCount > 0) {
      addProcessingStep(`${errorCount} items had errors and were skipped`, 'warning');
    }

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

  const isRowBeingProcessed = (index) => {
    return currentlyProcessingIndex === index;
  };

  return (
    <div className="h-full flex flex-col bg-gray-50">
      <div className="flex flex-1 p-4 gap-4 overflow-hidden">
        <div className="w-2/3 flex flex-col bg-white shadow rounded-lg overflow-hidden border border-gray-200">
          <div className="p-3 bg-gray-100 border-b flex justify-between items-center">
            <h2 className="font-medium text-gray-700">Purchase Order Data</h2>

          </div>

          <div className="flex-1 overflow-auto" style={{ maxHeight: "calc(100vh - 300px)" }}>
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
                      <tr
                        key={idx}
                        className={`
                          ${idx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                          ${isRowBeingProcessed(idx) ? "ring-2 ring-blue-400 bg-blue-50" : ""}
                        `}
                      >
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
                No data loaded. Please upload a file or add rows to begin.
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
                <div key={index} className={`flex items-start gap-2 p-2 rounded border-l-4 ${step.type === 'success' ? 'border-l-green-400 bg-green-50' :
                  step.type === 'warning' ? 'border-l-yellow-400 bg-yellow-50' :
                    'border-l-blue-400 bg-blue-50'
                  }`}>
                  <div className="flex-1">
                    <div className={`font-medium ${step.type === 'success' ? 'text-green-600' :
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
        <div className="text-sm text-gray-600">
          Total Items: {tableData.length} |
          Completed: {tableData.filter(row => row['Status'] === 'Completed').length} |
          Errors: {tableData.filter(row => row['Status'] === 'Error').length}
        </div>
        <button
          onClick={handleGenerateData}
          disabled={tableData.length === 0 || isProcessing}
          className={`flex items-center gap-2 px-4 py-2 rounded border shadow text-white ${tableData.length === 0 || isProcessing
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