import React, { useState, useEffect } from 'react';
import { FaPaperPlane, FaRobot } from 'react-icons/fa';

const SupportAgent = () => {
  const [query, setQuery] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [hasInteracted, setHasInteracted] = useState(false);

  // Each section's loading and visibility state
  const [loadingStates, setLoadingStates] = useState({
    mainCategoryML: { visible: false, loading: false },
    mainCategoryNLP: { visible: false, loading: false },
    previousTicket: { visible: false, loading: false },
    mlSubCategory: { visible: false, loading: false },
    nlpSubCategory: { visible: false, loading: false },
    response: { visible: false, loading: false }
  });

  // Mock IDOC table data
  const idocTableData = [
    { idoc_number: "1000001", date_status_error: "01-04-2025", time_status_error: "10:05:23", status_counter: "1", status_code: "53", status_text: "IDOC Successfully Posted", routine_function_mode: "ZFUNC01", order_number: "450001", sales_org: "1000", sales_area: "1", division: "10", amount: "500.75" },
    { idoc_number: "1000002", date_status_error: "02-04-2025", time_status_error: "11:15:45", status_counter: "2", status_code: "51", status_text: "Application document not posted", routine_function_mode: "ZFUNC02", order_number: "450002", sales_org: "2000", sales_area: "2", division: "20", amount: "2600" },
    { idoc_number: "1000003", date_status_error: "03-04-2025", time_status_error: "09:30:12", status_counter: "1", status_code: "53", status_text: "IDOC Successfully Posted", routine_function_mode: "ZFUNC03", order_number: "450003", sales_org: "1000", sales_area: "3", division: "30", amount: "320.5" },
    { idoc_number: "1000004", date_status_error: "04-04-2025", time_status_error: "14:22:57", status_counter: "3", status_code: "53", status_text: "IDOC Successfully Posted", routine_function_mode: "ZFUNC04", order_number: "450004", sales_org: "3000", sales_area: "4", division: "80", amount: "4789.2" },
    { idoc_number: "1000005", date_status_error: "05-04-2025", time_status_error: "08:47:30", status_counter: "1", status_code: "51", status_text: "Application document not posted", routine_function_mode: "ZFUNC05", order_number: "450005", sales_org: "1000", sales_area: "2", division: "10", amount: "999.99" }
  ];

  // Mock sync results
  const syncResults = [
    {
      idocNumber: "1000001",
      updates: [
        { field: "Order Number", value: "450001", status: "matched" },
        { field: "Sales Organization", value: "1000", status: "matched" },
        { field: "Sales Area", value: "01", status: "matched" },
        { field: "Division", value: "10", status: "matched" },
        { field: "Amount", value: "500.75", status: "updated", oldValue: "1500.75" }
      ]
    },
    {
      idocNumber: "1000003",
      updates: [
        { field: "Order Number", value: "450003", status: "updated", oldValue: "4500038" },
        { field: "Sales Organization", value: "1000", status: "matched" },
        { field: "Sales Area", value: "03", status: "matched" },
        { field: "Division", value: "30", status: "matched" },
        { field: "Amount", value: "320.5", status: "matched" }
      ]
    },
    {
      idocNumber: "1000004",
      updates: [
        { field: "Order Number", value: "450004", status: "matched" },
        { field: "Sales Organization", value: "3000", status: "matched" },
        { field: "Sales Area", value: "04", status: "updated", oldValue: "01" },
        { field: "Division", value: "80", status: "updated", oldValue: "40" },
        { field: "Amount", value: "4789.2", status: "matched" }
      ]
    }
  ];

  // Mock response data for the IDOC issue
  const mockResponseData = {
    mainCategoryML: {
      title: "Main Category: ML Triage Result",
      runningMessage: "Running: main_ml_triage_agent_predict_tool(incident_description=['psdata idoc sales order issues'])",
      result: "ML Predicted Main Category: Idoc Issue"
    },
    mainCategoryNLP: {
      title: "Main Category NLP Triage Result",
      runningMessage: "Running: main_nlp_triage_agent_predict_tool(issue_description=psdata idoc sales order issues)",
      result: "NLP Predicted Main Category: Idoc Issue"
    },
    previousTicket: {
      title: "Previous Ticket Analysis",
      runningMessage: "Running: ticket_analysis_rag_agent(query=psdata idoc sales order issues)",
      incidents: [
        {
          incidentNumber: "Not specified",
          errorMessage: "Purchase order still contains faulty items",
          sapDetails: {
            sapId: "MEPO",
            sapModule: "Addon",
            systemClient: "SP1CLNT1/100"
          }
        },
        {
          incidentNumber: "Not specified",
          errorMessage: "Status \"Obsolete\" of material 5002858 does not allow external procurement",
          sapDetails: {
            sapId: "ME",
            sapModule: "Addon",
            systemClient: "SP1CLNT1/100"
          }
        },
        {
          incidentNumber: "Not specified",
          errorMessage: "Tax jurisdiction code not allowed for tax calculation schema TAXUSJ",
          sapDetails: {
            sapId: "06",
            sapModule: "Addon",
            systemClient: "SP1CLNT1/100"
          }
        }
      ],
      analysis: [
        "Faulty Items: Ensure that all items in the purchase order are valid and correctly entered. This may include checking for entry errors or missing data.",
        "Material Status: Verify that the status of the material allows for external procurement. If a material is marked as \"Obsolete\", it might need to be updated or replaced with a valid material.",
        "Tax Jurisdiction: Validate the tax jurisdiction code setup in the system to ensure compliance with the tax calculation schema being used.",
        "General Troubleshooting: Always verify the related configurations in SAP for each identified module (e.g., Addon). Make sure the module and screen settings align with the operational requirements of the sales order."
      ],
      conclusion: "These tickets suggest cross-verifying the current setup and data accuracy for IDOCs related to sales orders, focusing on areas like data entry and configuration settings for procurement and taxation."
    },
    mlSubCategory: {
      title: "ML Sub Category ML Triage Result",
      runningMessage: "Running: ml_triage_agent_predict_tool(incident_description=['psdata idoc sales order issues'])",
      result: "ML Sub Category ML Triage Result: Incorrect Entry"
    },
    nlpSubCategory: {
      title: "NLP Sub Category NLP Triage Result",
      runningMessage: "Running: nlp_triage_agent_predict_tool(issue_description=psdata idoc sales order issues)",
      result: "NLP Sub Category NLP Triage Result: Application Document Not Posted"
    },
    finalResponse: {
      idocAgentTitle: "IDOC Incorrect Entry Agent Result",
      masterDataTitle: "Master Data Table: idoc_status",
      idocTableData: idocTableData,
      syncTitle: "Running:",
      syncCommand: "sync_idoc_with_sales(user_input=psdata idoc sales order issues)",
      syncResults: syncResults,
      updatedTableTitle: "Updated Master Data Table: idoc_status"
    }
  };

  // Response data state
  const [responseData, setResponseData] = useState({});

  const handleSend = () => {
    if (!query.trim()) return;

    setIsLoading(true);
    setHasInteracted(true);
    setResponseData({ query: query });

    // Start the sequential loading process
    startSequentialLoading();

    // Clear input
    setQuery("");
  };

  const startSequentialLoading = () => {
    // Reset all loading states and visibility
    setLoadingStates({
      mainCategoryML: { visible: false, loading: false },
      mainCategoryNLP: { visible: false, loading: false },
      previousTicket: { visible: false, loading: false },
      mlSubCategory: { visible: false, loading: false },
      nlpSubCategory: { visible: false, loading: false },
      response: { visible: false, loading: false }
    });

    const timeout = 2500;  // Base timeout for each section
    const headingDelay = 900;  // Delay before showing next heading

    // Sequential order of components
    const sequence = ['mainCategoryML', 'mainCategoryNLP', 'previousTicket', 'mlSubCategory', 'nlpSubCategory', 'response'];

    // First make mainCategoryML heading visible
    setTimeout(() => {
      setLoadingStates(prev => ({
        ...prev,
        mainCategoryML: { visible: true, loading: true }
      }));

      // Execute the sequence
      executeSequence(0);
    }, 500);

    // Function to execute the sequence of components
    const executeSequence = (index) => {
      if (index >= sequence.length) {
        setIsLoading(false);
        return;
      }

      const currentItem = sequence[index];
      const nextItem = sequence[index + 1];

      // Process current item content
      setTimeout(() => {
        // Update the content of the current item
        setResponseData(prev => ({
          ...prev,
          [currentItem]: mockResponseData[currentItem]
        }));

        // Mark current item as loaded
        setLoadingStates(prev => ({
          ...prev,
          [currentItem]: { visible: true, loading: false }
        }));

        // Make next item visible if there is one
        if (nextItem) {
          setTimeout(() => {
            setLoadingStates(prev => ({
              ...prev,
              [nextItem]: { visible: true, loading: true }
            }));

            // Process next item
            executeSequence(index + 1);
          }, headingDelay);
        } else {
          setIsLoading(false);
          // Set final response
          setResponseData(prev => ({
            ...prev,
            finalResponse: mockResponseData.finalResponse
          }));
        }
      }, timeout);
    };
  };

  // Get loading messages for each section
  const getLoadingMessage = (section) => {
    const messages = {
      mainCategoryML: mockResponseData.mainCategoryML.runningMessage,
      mainCategoryNLP: mockResponseData.mainCategoryNLP.runningMessage,
      previousTicket: mockResponseData.previousTicket.runningMessage,
      mlSubCategory: mockResponseData.mlSubCategory.runningMessage,
      nlpSubCategory: mockResponseData.nlpSubCategory.runningMessage,
      response: "Generating response..."
    };
    return messages[section] || `Loading ${section}...`;
  };

  // Left panel components (ML categories)
  const LeftComponents = () => {
    return (
      <div className="flex flex-col h-full">
        <div className="flex flex-col h-full space-y-6">
          {/* Main Category ML Section */}
          {loadingStates.mainCategoryML.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">{mockResponseData.mainCategoryML.title}</h2>
              {loadingStates.mainCategoryML.loading ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">{getLoadingMessage('mainCategoryML')}</p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.mainCategoryML ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <p>{responseData.mainCategoryML.result}</p>
                </div>
              ) : (
                <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                </div>
              )}
            </div>
          )}

          {/* Main Category NLP Section */}
          {loadingStates.mainCategoryNLP.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">{mockResponseData.mainCategoryNLP.title}</h2>
              {loadingStates.mainCategoryNLP.loading ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">{getLoadingMessage('mainCategoryNLP')}</p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.mainCategoryNLP ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <p>{responseData.mainCategoryNLP.result}</p>
                </div>
              ) : (
                <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                </div>
              )}
            </div>
          )}

          {/* ML Sub Category Section */}
          {loadingStates.mlSubCategory.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">{mockResponseData.mlSubCategory.title}</h2>
              {loadingStates.mlSubCategory.loading ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">{getLoadingMessage('mlSubCategory')}</p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.mlSubCategory ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <p>{responseData.mlSubCategory.result}</p>
                </div>
              ) : (
                <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  };

  // Right panel components (NLP categories and previous tickets)
  const RightComponents = () => {
    return (
      <div className="flex flex-col h-full">
        <div className="flex flex-col h-full space-y-6">
          {/* Previous Ticket Analysis Section */}
          {loadingStates.previousTicket.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">{mockResponseData.previousTicket.title}</h2>
              {loadingStates.previousTicket.loading ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">{getLoadingMessage('previousTicket')}</p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.previousTicket ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  {responseData.previousTicket.incidents.map((incident, idx) => (
                    <div key={idx} className="mb-4 pb-3 border-b border-gray-200">
                      <p className="font-semibold">Similar Incident Found:</p>
                      <p>Incident Number: {incident.incidentNumber}</p>
                      <p>Error Message: {incident.errorMessage}</p>
                      <p className="font-semibold mt-1">SAP Details:</p>
                      <p>SAP Id: {incident.sapDetails.sapId}</p>
                      <p>SAP Module: {incident.sapDetails.sapModule}</p>
                      <p>System/Client: {incident.sapDetails.systemClient}</p>
                    </div>
                  ))}
                  <div className="mt-4">
                    <p className="font-semibold">Previous Ticket Analysis:</p>
                    {responseData.previousTicket.analysis.map((item, idx) => (
                      <p key={idx} className="mt-2">{item}</p>
                    ))}
                    <p className="mt-3">{responseData.previousTicket.conclusion}</p>
                  </div>
                </div>
              ) : (
                <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-3/4 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-2/3 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-4/5"></div>
                </div>
              )}
            </div>
          )}

          {/* NLP Sub Category Section */}
          {loadingStates.nlpSubCategory.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">{mockResponseData.nlpSubCategory.title}</h2>
              {loadingStates.nlpSubCategory.loading ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">{getLoadingMessage('nlpSubCategory')}</p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.nlpSubCategory ? (
                <div className="bg-gray-100 p-4 rounded-lg">
                  <p>{responseData.nlpSubCategory.result}</p>
                </div>
              ) : (
                <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  };

  // Create responsive table component
  const ResponsiveTable = ({ data, title }) => {
    return (
      <div className="mb-6 overflow-x-auto">
        <h3 className="font-bold text-lg mb-2">{title}</h3>
        <div className="border border-gray-200 rounded-lg">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                {Object.keys(data[0]).map((header, idx) => (
                  <th key={idx} className="px-3 py-2 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    {header.replace(/_/g, ' ')}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {data.map((row, rowIdx) => (
                <tr key={rowIdx} className={rowIdx % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                  {Object.values(row).map((cell, cellIdx) => (
                    <td key={cellIdx} className="px-3 py-2 text-xs text-gray-500">
                      {cell}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    );
  };

  // Sync results component
  const SyncResultsComponent = ({ syncResults }) => {
    return (
      <div className="mb-6">
        <div className="space-y-4">
          <p className="font-medium">The following mismatches were found between the IDOC data and the sales data, and they have been updated:</p>
          {syncResults.map((result, idx) => (
            <div key={idx} className="pl-2 border-l-2 border-indigo-400">
              <p className="font-bold">{idx + 1}. IDOC Number: {result.idocNumber}</p>
              <ul className="pl-4 mt-1 space-y-1">
                {result.updates.map((update, updateIdx) => (
                  <li key={updateIdx} className="text-sm">
                    <span className="font-medium">{update.field}</span>:
                    <span className="ml-1">`{update.value}`</span>
                    {update.status === "updated" ?
                      <span className="text-amber-600 ml-1">(was updated to match from `{update.oldValue}`)</span> :
                      <span className="text-green-600 ml-1">(matched)</span>}
                  </li>
                ))}
              </ul>
            </div>
          ))}
          <p className="text-green-700 font-medium">These discrepancies have been successfully reconciled to ensure data consistency between the IDOC and sales records.</p>
        </div>
      </div>
    );
  };

  return (
    <div className="flex flex-col h-full bg-gray-50">
      <div className="flex flex-grow overflow-hidden">
        {/* Left Panel - Only visible after interaction */}
        {hasInteracted && (
          <div className="hidden md:block w-1/4 bg-white border-r border-gray-200 overflow-hidden">
            <div className="p-4 h-full">
              <LeftComponents />
            </div>
          </div>
        )}

        {/* Main Content Area - Full width initially, then center column after interaction */}
        <div className={`flex-grow flex flex-col ${hasInteracted ? 'md:w-2/4' : 'w-full'}`}>
          {/* Chat Messages Area */}
          <div className="flex-grow overflow-y-auto p-4">
            {!hasInteracted ? (
              <div className="flex flex-col items-center justify-center h-full text-center p-6">
                {/* Improved initial UI with better positioning */}
                <div className="max-w-md w-full mx-auto flex flex-col items-center">
                  <h2 className="text-2xl font-bold text-indigo-600 mb-3">SAP Support Assistant</h2>
                  <p className="text-gray-600 mb-8">How can I help you with your SAP issues today?</p>

                  {/* Input field positioned in the middle of the screen */}
                  <div className="w-full flex items-center gap-2 bg-white rounded-full shadow-md border border-indigo-100 p-1 pl-4 transition-all hover:shadow-lg">
                    <input
                      type="text"
                      value={query}
                      onChange={(e) => setQuery(e.target.value)}
                      className="flex-1 p-3 focus:outline-none bg-transparent"
                      placeholder="Describe your SAP issue..."
                      onKeyDown={(e) => e.key === "Enter" && handleSend()}
                    />
                    <button
                      onClick={handleSend}
                      disabled={!query.trim()}
                      className={`p-3 rounded-full text-white transition-all ${query.trim() ? "bg-indigo-600 hover:bg-indigo-700" : "bg-indigo-300 cursor-not-allowed"}`}
                    >
                      <FaPaperPlane />
                    </button>
                  </div>
                </div>
              </div>
            ) : (
              <div className="max-w-3xl mx-auto w-full">
                {responseData.query && (
                  <div className="mb-6 flex justify-end">
                    <div className="bg-indigo-500 text-white p-4 rounded-lg rounded-tr-none max-w-md">
                      <p>{responseData.query}</p>
                    </div>
                  </div>
                )}

                {loadingStates.response.visible ? (
                  loadingStates.response.loading ? (
                    <div className="mb-6">
                      <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm max-w-full">
                        <div className="flex items-center space-x-2">
                          <p className="text-sm text-gray-500">{getLoadingMessage('response')}</p>
                          <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                        </div>
                      </div>
                    </div>
                  ) : responseData.finalResponse ? (
                    <div className="mb-6 overflow-auto ">
                      <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm">
                        {/* New Table Response */}
                        <h2 className="text-xl font-bold mb-3">{responseData.finalResponse.idocAgentTitle}</h2>
                        <h3 className="text-lg font-semibold mb-2">{responseData.finalResponse.masterDataTitle}</h3>

                        {/* IDOC Table */}
                        <ResponsiveTable
                          data={responseData.finalResponse.idocTableData}
                          title=""
                        />

                        {/* Running Command */}
                        <p className="font-semibold mb-1">{responseData.finalResponse.syncTitle}</p>
                        <p className="mb-4 font-mono text-sm bg-gray-50 p-2 rounded">{responseData.finalResponse.syncCommand}</p>

                        {/* Sync Results */}
                        <SyncResultsComponent syncResults={responseData.finalResponse.syncResults} />

                        {/* Updated Table */}
                        <h3 className="text-lg font-semibold mb-2">{responseData.finalResponse.updatedTableTitle}</h3>
                        <ResponsiveTable
                          data={responseData.finalResponse.idocTableData}
                          title=""
                          className="overflow-x-auto"
                        />
                      </div>
                    </div>
                  ) : null
                ) : isLoading && !Object.values(loadingStates).some(state => state.visible) ? (
                  <div className="mb-6">
                    <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm max-w-md">
                      <div className="flex items-center space-x-2">
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "0ms" }}></div>
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "150ms" }}></div>
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "300ms" }}></div>
                      </div>
                    </div>
                  </div>
                ) : null}
              </div>
            )}
          </div>

          {/* Input Area - Only shown after interaction or if not on initial screen */}
          {hasInteracted && (
            <div className="p-4 bg-white shadow-lg border-t border-indigo-100">
              <div className="max-w-4xl mx-auto flex items-center gap-2 bg-white rounded-full shadow-sm border border-indigo-100 p-1 pl-4">
                <input
                  type="text"
                  value={query}
                  onChange={(e) => setQuery(e.target.value)}
                  className="flex-1 p-2 focus:outline-none bg-transparent"
                  placeholder="Type your message..."
                  onKeyDown={(e) => e.key === "Enter" && handleSend()}
                />
                <button
                  onClick={handleSend}
                  disabled={!query.trim()}
                  className={`p-3 rounded-full text-white transition-all ${query.trim() ? "bg-indigo-600 hover:bg-indigo-700" : "bg-indigo-300 cursor-not-allowed"}`}
                >
                  <FaPaperPlane />
                </button>
              </div>
            </div>
          )}
        </div>
        {/* Right Panel - Only visible after interaction */}
        {hasInteracted && (
          <div className="hidden md:block w-1/4 bg-white border-l border-gray-200 overflow-hidden">
            <div className="p-4 h-full">
              <RightComponents />
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default SupportAgent;  