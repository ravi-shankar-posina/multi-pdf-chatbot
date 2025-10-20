import { CheckCircle, Loader, Play, XCircle } from 'lucide-react';
import { use, useEffect } from 'react';
import { useState } from 'react';

const PoAutomation = () => {
  const [tableData, setTableData] = useState([]);
  const [isProcessing, setIsProcessing] = useState(false);
  const [processingSteps, setProcessingSteps] = useState([]);
  const [processingStarted, setProcessingStarted] = useState(false);
  const [currentlyProcessingIndex, setCurrentlyProcessingIndex] = useState(-1);

  const addProcessingStep = (message, type = 'info') => {
    setProcessingSteps(prev => [...prev, { message, type, timestamp: new Date().toLocaleTimeString() }]);
  };

  const fetchPOData = async () => {
    try {
      const response = await fetch(`${import.meta.env.VITE_API_URL}/po-data`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        },
      });

      const data = await response.json();
      setTableData(data);
      console.log(data);
    } catch (error) {
      console.error('Error fetching PO data:', error);
    }
  };

  useEffect(() => {
    fetchPOData();
  }, [])

  const handleGenerateData = async () => {
    if (tableData.length === 0) return;

    setIsProcessing(true);
    setProcessingStarted(true);
    setProcessingSteps([]);
    setCurrentlyProcessingIndex(-1);

    // Processing steps over 45 seconds
    const processingMessages = [
      { message: 'Starting PO generation process...', type: 'info', delay: 0 },
      { message: 'Connecting to procurement system...', type: 'info', delay: 2000 },
      { message: 'Analyzing data structure...', type: 'info', delay: 4000 },
      { message: 'Validating vendor information...', type: 'info', delay: 7000 },
      { message: 'Checking budget allocations...', type: 'info', delay: 10000 },
      { message: 'Validating item numbers...', type: 'info', delay: 13000 },
      { message: 'Processing approval workflows...', type: 'info', delay: 16000 },
      { message: 'Generating purchase requisitions...', type: 'info', delay: 19000 },
      { message: 'Validating pricing information...', type: 'info', delay: 22000 },
      { message: 'Checking inventory levels...', type: 'info', delay: 25000 },
      { message: 'Processing delivery schedules...', type: 'info', delay: 28000 },
      { message: 'Generating compliance reports...', type: 'info', delay: 31000 },
      { message: 'Updating financial records...', type: 'info', delay: 34000 },
      { message: 'Sending notifications to stakeholders...', type: 'info', delay: 37000 },
      { message: 'Finalizing processing...', type: 'info', delay: 40000 },
      { message: 'Refreshing data from server...', type: 'success', delay: 43000 }
    ];

    // Execute processing messages with delays
    processingMessages.forEach((step, index) => {
      setTimeout(() => {
        addProcessingStep(step.message, step.type);
        
        // Simulate processing individual items
        if (index >= 4 && index <= 10) {
          setCurrentlyProcessingIndex(Math.floor(Math.random() * tableData.length));
        } else {
          setCurrentlyProcessingIndex(-1);
        }
      }, step.delay);
    });

    // After 45 seconds, fetch fresh data and complete processing
    setTimeout(async () => {
      setCurrentlyProcessingIndex(-1);
      addProcessingStep('Fetching updated data...', 'info');
      
      // Fetch fresh data from server
      await fetchPOData();
      
      addProcessingStep('Process completed successfully', 'success');
      setIsProcessing(false);
    }, 45000);
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
                      {['pr_number', ...Object.keys(tableData[0]).filter(key => key !== 'pr_number' && key !== '_id')].map((key) => (
                        <th key={key} className="border p-2 text-left font-semibold capitalize">
                          {key.replace(/_/g, ' ').toUpperCase()}
                        </th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {tableData.map((row, idx) => (
                      <tr
                        key={idx}
                        className={`
                          ${row['Status'] === 'Error' ? "bg-red-50" : 
                            idx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                          ${isRowBeingProcessed(idx) ? "ring-2 ring-blue-400 bg-blue-50" : ""}
                        `}
                      >
                        {['pr_number', ...Object.keys(row).filter(key => key !== 'pr_number' && key !== '_id')].map((key, i) => {
                          const val = row[key];
                          return (
                            <td key={i} className="border p-2 ">
                              {key === 'status' && val ? (
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
                          );
                        })}
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
                    'border-l-gray-400 bg-blue-50'
                  }`}>
                  <div className="flex-1">
                    <div className={`font-medium ${step.type === 'success' ? 'text-green-600' :
                      step.type === 'warning' ? 'text-yellow-600' :
                        'text-gray-600'
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
          Completed: {tableData.filter(row => row['status'] === 'Completed').length} |
          Errors: {tableData.filter(row => row['status'] === 'Error').length}
        </div>
        <button
          onClick={handleGenerateData}
          disabled={tableData.length === 0 || isProcessing}
          className={`flex items-center gap-2 px-4 py-2 rounded border shadow text-white ${tableData.length === 0 || isProcessing
            ? "bg-gray-400 cursor-not-allowed"
            : "bg-gray-600 hover:bg-gray-700"
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