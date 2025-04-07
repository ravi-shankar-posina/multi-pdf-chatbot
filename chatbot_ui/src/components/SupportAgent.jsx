import React, { useState, useEffect } from 'react';
import { FaPaperPlane, FaRobot } from 'react-icons/fa';
import { categoryResponses } from './responseData';

const SupportAgent = () => {
  const [query, setQuery] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [hasInteracted, setHasInteracted] = useState(false);
  const [category, setCategory] = useState(null);

  // State for each section's loading status
  const [loadingStates, setLoadingStates] = useState({
    triageAgent: false,
    nextAgent: false,
    analysis: false,
    suggestions: false,
    externalLinks: false,
    response: false
  });

  // Single XML response data
  const [xmlResponseData, setXmlResponseData] = useState(null);

  // Parsed response data for rendering
  const [responseData, setResponseData] = useState({
    query: "",
    response: "",
    triageAgent: null,
    nextAgent: null,
    analysis: null,
    suggestions: null,
    externalLinks: null
  });

  // Parse XML data to our internal format
  const parseXmlToResponseData = (xmlString) => {
    // This is a simple parser - in production you would use a proper XML parser
    const parser = new DOMParser();
    const xmlDoc = parser.parseFromString(xmlString, "text/xml");

    return {
      query: xmlDoc.querySelector("query")?.textContent || "",
      response: xmlDoc.querySelector("response")?.textContent || "",
      triageAgent: {
        mlPrediction: xmlDoc.querySelector("triageAgent mlPrediction")?.textContent || "",
        nlpPrediction: xmlDoc.querySelector("triageAgent nlpPrediction")?.textContent || "",
        reasoning: xmlDoc.querySelector("triageAgent reasoning")?.textContent || "",
        category: xmlDoc.querySelector("triageAgent category")?.textContent || "",
        subcategory: xmlDoc.querySelector("triageAgent subcategory")?.textContent || ""
      },
      nextAgent: {
        recommendedAgent: xmlDoc.querySelector("nextAgent recommendedAgent")?.textContent || "",
        availability: xmlDoc.querySelector("nextAgent availability")?.textContent || "",
        nextStep: xmlDoc.querySelector("nextAgent nextStep")?.textContent || ""
      },
      analysis: {
        sentiment: xmlDoc.querySelector("analysis sentiment")?.textContent || "",
        priority: xmlDoc.querySelector("analysis priority")?.textContent || "",
        category: xmlDoc.querySelector("analysis category")?.textContent || "",
        keywords: Array.from(xmlDoc.querySelectorAll("analysis keywords keyword")).map(k => k.textContent)
      },
      suggestions: Array.from(xmlDoc.querySelectorAll("suggestions suggestion")).map(s => s.textContent),
      externalLinks: Array.from(xmlDoc.querySelectorAll("externalLinks link")).map(l => ({
        title: l.getAttribute("title") || "",
        url: l.getAttribute("url") || ""
      }))
    };
  };

  // Determine query category based on keywords
  const determineCategory = (queryText) => {
    queryText = queryText.toLowerCase();

    if (queryText.includes('access') || queryText.includes('login') || queryText.includes("can't get in")) {
      return 'access';
    } else if (queryText.includes('authorize') || queryText.includes('permission') || queryText.includes('admin')) {
      return 'authorize';
    } else if (queryText.includes('forgot') || queryText.includes('reset') || queryText.includes('password')) {
      return 'forget';
    }

    // Default to password reset if can't determine
    return 'forget';
  };

  const handleSend = () => {
    if (!query.trim()) return;

    setIsLoading(true);
    setHasInteracted(true);

    // Determine the category of the query
    const determinedCategory = determineCategory(query);
    setCategory(determinedCategory);

    // Store the query
    setResponseData(prev => ({
      ...prev,
      query: query
    }));

    // Start the sequential loading process with the determined category
    startSequentialLoading(determinedCategory);

    // Clear input
    setQuery("");
  };

  const startSequentialLoading = (queryCategory) => {
    // Get the appropriate response and loading messages for the category
    const categoryData = categoryResponses[queryCategory];
    const mockXmlResponse = categoryData.xmlResponse;
    const loadingMessages = categoryData.loadingMessages;

    // Store the XML response
    setXmlResponseData(mockXmlResponse);

    // Reset all loading states
    setLoadingStates({
      triageAgent: true,
      nextAgent: false,
      analysis: false,
      suggestions: false,
      externalLinks: false,
      response: false
    });

    // Use shorter timeout for each step
    const timeout = 1500;

    // Triage Agent (first)
    setTimeout(() => {
      const parsedData = parseXmlToResponseData(mockXmlResponse);
      setResponseData(prev => ({
        ...prev,
        triageAgent: parsedData.triageAgent
      }));

      setLoadingStates(prev => ({
        ...prev,
        triageAgent: false,
        nextAgent: true
      }));

      // Next Agent (second)
      setTimeout(() => {
        setResponseData(prev => ({
          ...prev,
          nextAgent: parsedData.nextAgent
        }));

        setLoadingStates(prev => ({
          ...prev,
          nextAgent: false,
          analysis: true
        }));

        // Analysis (third)
        setTimeout(() => {
          setResponseData(prev => ({
            ...prev,
            analysis: parsedData.analysis
          }));

          setLoadingStates(prev => ({
            ...prev,
            analysis: false,
            suggestions: true
          }));

          // Suggestions (fourth)
          setTimeout(() => {
            setResponseData(prev => ({
              ...prev,
              suggestions: parsedData.suggestions
            }));

            setLoadingStates(prev => ({
              ...prev,
              suggestions: false,
              externalLinks: true
            }));

            // External Links (fifth)
            setTimeout(() => {
              setResponseData(prev => ({
                ...prev,
                externalLinks: parsedData.externalLinks
              }));

              setLoadingStates(prev => ({
                ...prev,
                externalLinks: false,
                response: true
              }));

              // Main Response (last)
              setTimeout(() => {
                setResponseData(prev => ({
                  ...prev,
                  response: parsedData.response
                }));

                setLoadingStates(prev => ({
                  ...prev,
                  response: false
                }));

                setIsLoading(false);
              }, timeout);
            }, timeout);
          }, timeout);
        }, timeout);
      }, timeout);
    }, timeout);
  };

  // Get the current category's loading messages, or use defaults
  const getLoadingMessage = (section) => {
    if (!category) return "";
    return categoryResponses[category].loadingMessages[section] || `Loading ${section}...`;
  };

  // LeftComponents extraction
  const LeftComponents = () => {
    return (
      <div className="flex flex-col h-full">
        {/* Left Panel Container with height management */}
        <div className="flex flex-col h-full space-y-6">
          {/* Triage Agent Section - with flex-1 to share space and overflow-auto for scrolling */}
          <div className="flex-1 min-h-0 overflow-auto">
            <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">Triage Agent</h2>
            {loadingStates.triageAgent ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="flex items-center space-x-2">
                  <p className="text-sm text-gray-500">{getLoadingMessage('triageAgent')}</p>
                  <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                </div>
              </div>
            ) : responseData.triageAgent ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="mb-3">
                  <p className="font-semibold">ML Prediction:</p>
                  <p className="text-indigo-600">{responseData.triageAgent.mlPrediction}</p>
                </div>
                <div className="mb-3">
                  <p className="font-semibold">NLP Prediction:</p>
                  <p className="text-indigo-600">{responseData.triageAgent.nlpPrediction}</p>
                </div>
                <div className="mb-3">
                  <p className="font-semibold">Category:</p>
                  <p>{responseData.triageAgent.category}</p>
                </div>
                <div>
                  <p className="font-semibold">Subcategory:</p>
                  <p>{responseData.triageAgent.subcategory}</p>
                </div>
              </div>
            ) : (
              <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-1/3 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-3/4"></div>
              </div>
            )}
          </div>

          {/* Next Agent Section - with flex-1 to share space and overflow-auto for scrolling */}
          <div className="flex-1 min-h-0 overflow-auto">
            <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">NextAgent</h2>
            {loadingStates.nextAgent ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="flex items-center space-x-2">
                  <p className="text-sm text-gray-500">{getLoadingMessage('nextAgent')}</p>
                  <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                </div>
              </div>
            ) : responseData.nextAgent ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="mb-3">
                  <p className="font-semibold">Recommended Agent:</p>
                  <p className="text-sm">{responseData.nextAgent.recommendedAgent}</p>
                </div>
                <div className="mb-3">
                  <p className="font-semibold">Availability:</p>
                  <p className="text-green-600">{responseData.nextAgent.availability}</p>
                </div>
                <div>
                  <p className="font-semibold">Next Step:</p>
                  <p className="text-sm">{responseData.nextAgent.nextStep}</p>
                </div>
              </div>
            ) : (
              <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-1/3 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-2/3"></div>
              </div>
            )}
          </div>
        </div>
      </div>
    );
  };

  // RightComponents extraction
  const RightComponents = () => {
    return (
      <div className="flex flex-col h-full">
        {/* Right Panel Container with height management */}
        <div className="flex flex-col h-full space-y-4">
          {/* Analysis Section - with flex-1 to share space and overflow-auto for scrolling */}
          <div className="flex-1 min-h-0 overflow-auto">
            <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">Analysis</h2>
            {loadingStates.analysis ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="flex items-center space-x-2">
                  <p className="text-sm text-gray-500">{getLoadingMessage('analysis')}</p>
                  <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                </div>
              </div>
            ) : responseData.analysis ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="mb-3">
                  <p className="font-semibold">Sentiment:</p>
                  <p className="capitalize">{responseData.analysis.sentiment}</p>
                </div>
                <div className="mb-3">
                  <p className="font-semibold">Priority:</p>
                  <p className="capitalize">{responseData.analysis.priority}</p>
                </div>
                <div className="mb-3">
                  <p className="font-semibold">Category:</p>
                  <p className="capitalize">{responseData.analysis.category}</p>
                </div>
                <div>
                  <p className="font-semibold">Keywords:</p>
                  <div className="flex flex-wrap gap-2 mt-1">
                    {responseData.analysis.keywords.map((keyword, index) => (
                      <span key={index} className="bg-blue-100 px-2 py-1 rounded-full text-sm">
                        {keyword}
                      </span>
                    ))}
                  </div>
                </div>
              </div>
            ) : (
              <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-1/3 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-3/4 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-2/3"></div>
              </div>
            )}
          </div>

          {/* Suggestions Section - with flex-1 to share space and overflow-auto for scrolling */}
          <div className="flex-1 min-h-0 overflow-auto">
            <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">Suggestions</h2>
            {loadingStates.suggestions ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="flex items-center space-x-2">
                  <p className="text-sm text-gray-500">{getLoadingMessage('suggestions')}</p>
                  <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                </div>
              </div>
            ) : responseData.suggestions ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <ul className="list-disc pl-5">
                  {responseData.suggestions.map((suggestion, index) => (
                    <li key={index} className="mb-2">{suggestion}</li>
                  ))}
                </ul>
              </div>
            ) : (
              <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-3/4 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-2/3 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-4/5"></div>
              </div>
            )}
          </div>

          {/* External Links Section - with flex-1 to share space and overflow-auto for scrolling */}
          <div className="flex-1 min-h-0 overflow-auto">
            <h2 className="text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2">Web Search Results</h2>
            {loadingStates.externalLinks ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <div className="flex items-center space-x-2">
                  <p className="text-sm text-gray-500">{getLoadingMessage('externalLinks')}</p>
                  <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                </div>
              </div>
            ) : responseData.externalLinks ? (
              <div className="bg-gray-100 p-4 rounded-lg">
                <ul className="space-y-2">
                  {responseData.externalLinks.map((link, index) => (
                    <li key={index}>
                      <a
                        href={link.url}
                        className="text-blue-600 hover:underline flex items-center"
                        target="_blank"
                        rel="noopener noreferrer"
                      >
                        <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"></path>
                        </svg>
                        {link.title}
                      </a>
                    </li>
                  ))}
                </ul>
              </div>
            ) : (
              <div className="bg-gray-100 p-4 rounded-lg animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-2/3 mb-3"></div>
                <div className="h-4 bg-gray-200 rounded w-1/2"></div>
              </div>
            )}
          </div>
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
                  <h2 className="text-2xl font-bold text-indigo-600 mb-3">Support Assistant</h2>
                  <p className="text-gray-600 mb-8">How can I help you today?</p>

                  {/* Input field positioned in the middle of the screen */}
                  <div className="w-full flex items-center gap-2 bg-white rounded-full shadow-md border border-indigo-100 p-1 pl-4 transition-all hover:shadow-lg">
                    <input
                      type="text"
                      value={query}
                      onChange={(e) => setQuery(e.target.value)}
                      className="flex-1 p-3 focus:outline-none bg-transparent"
                      placeholder="Ask me anything..."
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

                {loadingStates.response ? (
                  <div className="mb-6">
                    <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm max-w-md">
                      <div className="flex items-center space-x-2">
                        <p className="text-sm text-gray-500">{getLoadingMessage('response')}</p>
                        <div className="w-4 h-4 border-2 border-gray-300 border-t-indigo-500 rounded-full animate-spin"></div>
                      </div>
                    </div>
                  </div>
                ) : responseData.response ? (
                  <div className="mb-6">
                    <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm max-w-md">
                      <p>{responseData.response}</p>
                    </div>
                  </div>
                ) : isLoading && (
                  <div className="mb-6">
                    <div className="bg-white border border-gray-200 p-4 rounded-lg shadow-sm max-w-md">
                      <div className="flex items-center space-x-2">
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "0ms" }}></div>
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "150ms" }}></div>
                        <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: "300ms" }}></div>
                      </div>
                    </div>
                  </div>
                )}
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
                  className={`p-3 rounded-full text-white transition-all ${query.trim() ? "bg-indigo-600 hover:bg-indigo-700" : "bg-indigo-300 cursor-not-allowed"
                    }`}
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