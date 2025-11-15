import { useState } from "react";
import { FaPaperPlane, FaPlus, FaMicrophone, FaArrowUp } from "react-icons/fa";
import logo from "../assets/image.png";
import AccessManagement from "../pages/Accessmanagement";
import { categoryResponses } from "./responseData";

const PasswordAgent = () => {
  const [query, setQuery] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [hasInteracted, setHasInteracted] = useState(false);
  const [category, setCategory] = useState(null);

  // Modified loading states to include heading visibility
  const [loadingStates, setLoadingStates] = useState({
    triageAgent: { visible: false, loading: false },
    nextAgent: { visible: false, loading: false },
    analysis: { visible: false, loading: false },
    suggestions: { visible: false, loading: false },
    externalLinks: { visible: false, loading: false },
    response: { visible: false, loading: false },
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
    externalLinks: null,
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
        mlPrediction:
          xmlDoc.querySelector("triageAgent mlPrediction")?.textContent || "",
        nlpPrediction:
          xmlDoc.querySelector("triageAgent nlpPrediction")?.textContent || "",
        reasoning:
          xmlDoc.querySelector("triageAgent reasoning")?.textContent || "",
        category:
          xmlDoc.querySelector("triageAgent category")?.textContent || "",
        subcategory:
          xmlDoc.querySelector("triageAgent subcategory")?.textContent || "",
      },
      nextAgent: {
        recommendedAgent:
          xmlDoc.querySelector("nextAgent recommendedAgent")?.textContent || "",
        availability:
          xmlDoc.querySelector("nextAgent availability")?.textContent || "",
        nextStep: xmlDoc.querySelector("nextAgent nextStep")?.textContent || "",
      },
      analysis: {
        sentiment:
          xmlDoc.querySelector("analysis sentiment")?.textContent || "",
        priority: xmlDoc.querySelector("analysis priority")?.textContent || "",
        category: xmlDoc.querySelector("analysis category")?.textContent || "",
        keywords: Array.from(
          xmlDoc.querySelectorAll("analysis keywords keyword")
        ).map((k) => k.textContent),
      },
      suggestions: Array.from(
        xmlDoc.querySelectorAll("suggestions suggestion")
      ).map((s) => s.textContent),
      externalLinks: Array.from(
        xmlDoc.querySelectorAll("externalLinks link")
      ).map((l) => ({
        title: l.getAttribute("title") || "",
        url: l.getAttribute("url") || "",
      })),
    };
  };

  // Determine query category based on keywords
  const determineCategory = (queryText) => {
    queryText = queryText.toLowerCase();

    if (
      queryText.includes("access") ||
      queryText.includes("login") ||
      queryText.includes("can't get in")
    ) {
      return "access";
    } else if (
      queryText.includes("authorize") ||
      queryText.includes("permission") ||
      queryText.includes("admin")
    ) {
      return "authorize";
    } else if (
      queryText.includes("forgot") ||
      queryText.includes("reset") ||
      queryText.includes("password")
    ) {
      return "forget";
    }

    // Default to password reset if can't determine
    return "forget";
  };

  const handleSend = () => {
    if (!query.trim()) return;

    setIsLoading(true);
    setHasInteracted(true);

    // Determine the category of the query
    const determinedCategory = determineCategory(query);
    setCategory(determinedCategory);

    // Store the query
    setResponseData((prev) => ({
      ...prev,
      query: query,
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

    // Reset all loading states and visibility
    setLoadingStates({
      triageAgent: { visible: false, loading: false },
      nextAgent: { visible: false, loading: false },
      analysis: { visible: false, loading: false },
      suggestions: { visible: false, loading: false },
      externalLinks: { visible: false, loading: false },
      response: { visible: false, loading: false },
    });

    const parsedData = parseXmlToResponseData(mockXmlResponse);
    const timeout = 2500;
    const headingDelay = 900; // Delay before showing next heading

    // Sequential order of components
    const sequence = [
      "triageAgent",
      "nextAgent",
      "analysis",
      "suggestions",
      "externalLinks",
      "response",
    ];

    // First make Triage Agent heading visible
    setTimeout(() => {
      setLoadingStates((prev) => ({
        ...prev,
        triageAgent: { visible: true, loading: true },
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
        setResponseData((prev) => ({
          ...prev,
          [currentItem]: parsedData[currentItem],
        }));

        // Mark current item as loaded
        setLoadingStates((prev) => ({
          ...prev,
          [currentItem]: { visible: true, loading: false },
        }));

        // Make next item visible if there is one
        if (nextItem) {
          setTimeout(() => {
            setLoadingStates((prev) => ({
              ...prev,
              [nextItem]: { visible: true, loading: true },
            }));

            // Process next item
            executeSequence(index + 1);
          }, headingDelay);
        } else {
          setIsLoading(false);
        }
      }, timeout);
    };
  };

  // Get the current category's loading messages, or use defaults
  const getLoadingMessage = (section) => {
    if (!category) return "";
    return (
      categoryResponses[category].loadingMessages[section] ||
      `Loading ${section}...`
    );
  };

  // LeftComponents extraction with sequential visibility
  const LeftComponents = () => {
    return (
      <div className="flex flex-col h-full">
        {/* Left Panel Container with height management */}
        <div className="flex flex-col h-full space-y-6">
          {/* Triage Agent Section - Only shown when visible */}
          {loadingStates.triageAgent.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-lg sm:text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2 text-gray-900">
                Triage Agent
              </h2>
              {loadingStates.triageAgent.loading ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">
                      {getLoadingMessage("triageAgent")}
                    </p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-black rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.triageAgent ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="mb-3">
                    <p className="font-semibold text-gray-900">
                      ML Prediction:
                    </p>
                    <p className="text-gray-700">
                      {responseData.triageAgent.mlPrediction}
                    </p>
                  </div>
                  <div className="mb-3">
                    <p className="font-semibold text-gray-900">
                      NLP Prediction:
                    </p>
                    <p className="text-gray-700">
                      {responseData.triageAgent.nlpPrediction}
                    </p>
                  </div>
                  <div className="mb-3">
                    <p className="font-semibold text-gray-900">Category:</p>
                    <p className="text-gray-700">
                      {responseData.triageAgent.category}
                    </p>
                  </div>
                  <div>
                    <p className="font-semibold text-gray-900">Subcategory:</p>
                    <p className="text-gray-700">
                      {responseData.triageAgent.subcategory}
                    </p>
                  </div>
                </div>
              ) : (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-1/3 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-3/4"></div>
                </div>
              )}
            </div>
          )}

          {/* Next Agent Section - Only shown when visible */}
          {loadingStates.nextAgent.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-lg sm:text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2 text-gray-900">
                NextAgent
              </h2>
              {loadingStates.nextAgent.loading ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">
                      {getLoadingMessage("nextAgent")}
                    </p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-black rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.nextAgent ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="mb-3">
                    <p className="font-semibold text-gray-900">
                      Recommended Agent:
                    </p>
                    <p className="text-sm text-gray-700">
                      {responseData.nextAgent.recommendedAgent}
                    </p>
                  </div>
                  <div className="mb-3">
                    <p className="font-semibold text-gray-900">Availability:</p>
                    <p className="text-green-600">
                      {responseData.nextAgent.availability}
                    </p>
                  </div>
                  <div>
                    <p className="font-semibold text-gray-900">Next Step:</p>
                    <p className="text-sm text-gray-700">
                      {responseData.nextAgent.nextStep}
                    </p>
                  </div>
                </div>
              ) : (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-1/2 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-1/3 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-2/3"></div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  };

  // RightComponents extraction with sequential visibility
  const RightComponents = () => {
    return (
      <div className="flex flex-col h-full">
        {/* Right Panel Container with height management */}
        <div className="flex flex-col h-full space-y-4">
          {/* Suggestions Section - Only shown when visible */}
          {loadingStates.suggestions.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-lg sm:text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2 text-gray-900">
                Suggestions
              </h2>
              {loadingStates.suggestions.loading ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">
                      {getLoadingMessage("suggestions")}
                    </p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-black rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.suggestions ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <ul className="list-disc pl-5 space-y-2">
                    {responseData.suggestions.map((suggestion, index) => (
                      <li key={index} className="text-gray-700">
                        {suggestion}
                      </li>
                    ))}
                  </ul>
                </div>
              ) : (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-3/4 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-2/3 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-4/5"></div>
                </div>
              )}
            </div>
          )}

          {/* External Links Section - Only shown when visible */}
          {loadingStates.externalLinks.visible && (
            <div className="flex-1 min-h-0 overflow-auto">
              <h2 className="text-lg sm:text-xl font-bold mb-4 sticky top-0 bg-white z-10 py-2 text-gray-900">
                Web Search Results
              </h2>
              {loadingStates.externalLinks.loading ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <div className="flex items-center space-x-2">
                    <p className="text-sm text-gray-500">
                      {getLoadingMessage("externalLinks")}
                    </p>
                    <div className="w-4 h-4 border-2 border-gray-300 border-t-black rounded-full animate-spin"></div>
                  </div>
                </div>
              ) : responseData.externalLinks ? (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl">
                  <ul className="space-y-2">
                    {responseData.externalLinks.map((link, index) => (
                      <li key={index}>
                        <a
                          href={link.url}
                          className="text-black hover:text-gray-600 hover:underline flex items-center transition-colors duration-200"
                          target="_blank"
                          rel="noopener noreferrer"
                        >
                          <svg
                            className="w-4 h-4 mr-2 flex-shrink-0"
                            fill="none"
                            stroke="currentColor"
                            viewBox="0 0 24 24"
                            xmlns="http://www.w3.org/2000/svg"
                          >
                            <path
                              strokeLinecap="round"
                              strokeLinejoin="round"
                              strokeWidth="2"
                              d="M10 6H6a2 2 0 00-2 2v10a2 2 0 002 2h10a2 2 0 002-2v-4M14 4h6m0 0v6m0-6L10 14"
                            ></path>
                          </svg>
                          <span className="text-sm">{link.title}</span>
                        </a>
                      </li>
                    ))}
                  </ul>
                </div>
              ) : (
                <div className="bg-gray-50 border border-gray-200 p-4 rounded-xl animate-pulse">
                  <div className="h-4 bg-gray-200 rounded w-2/3 mb-3"></div>
                  <div className="h-4 bg-gray-200 rounded w-1/2"></div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  };

  return (
    <div className="flex flex-col h-full bg-white">
      <div className="flex flex-grow overflow-hidden">
        {/* Left Panel - Only visible after interaction */}
        {hasInteracted && (
          <div className="hidden lg:block w-1/4 bg-white border-r border-gray-200 overflow-hidden">
            <div className="p-4 h-full">
              <LeftComponents />
            </div>
          </div>
        )}

        {/* Main Content Area - Full width initially, then center column after interaction */}
        <div
          className={`flex-grow flex flex-col ${
            hasInteracted ? "lg:w-2/4" : "w-full"
          }`}
        >
          {/* Chat Messages Area */}
          <div className="flex-grow overflow-y-auto p-4 sm:p-6">
            {!hasInteracted ? (
              <div className="flex flex-col items-center justify-center h-full text-center px-4">
                {/* Initial UI with logo and branding */}
                <div className="max-w-lg w-full mx-auto flex flex-col items-center">
                  {/* Logo and Title Section */}
                  <div className="flex flex-col sm:flex-row items-center justify-center mb-6 sm:mb-8">
                    <img
                      src={logo}
                      alt="Logo"
                      className="w-16 h-16 sm:w-20 sm:h-20 mb-4 sm:mb-0 sm:mr-4"
                    />
                    <div className="flex items-center">
                      <h2 className="text-2xl sm:text-3xl lg:text-4xl font-bold text-gray-800">
                        Password Agent
                      </h2>
                      <span className="ml-2 bg-blue-300 text-gray-800 text-sm font-semibold px-2 py-1 rounded-md">
                        Plus
                      </span>
                    </div>
                  </div>

                  <p className="text-gray-600 text-base sm:text-lg mb-8 sm:mb-12">
                    How can I help you today?
                  </p>

                  {/* Input field positioned in the middle of the screen */}
                  <div className="w-full flex items-center gap-3 bg-gray-50 rounded-3xl border-2 border-gray-300 px-4 py-3 focus-within:border-gray-400 focus-within:shadow-lg transition-all hover:shadow-xl">
                    {/* Plus icon */}
                    <button
                      className="p-2.5 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                      title="Add attachment"
                      type="button"
                    >
                      <FaPlus className="text-lg" />
                    </button>

                    {/* Input */}
                    <input
                      type="text"
                      value={query}
                      onChange={(e) => setQuery(e.target.value)}
                      className="flex-1 px-2 py-3 focus:outline-none bg-transparent text-base sm:text-lg placeholder-gray-500"
                      placeholder="Ask me anything..."
                      onKeyDown={(e) => e.key === "Enter" && handleSend()}
                    />

                    {/* Voice icon */}
                    <button
                      className="p-2.5 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                      title="Voice input"
                      type="button"
                    >
                      <FaMicrophone className="text-lg" />
                    </button>

                    {/* Send button */}
                    <button
                      onClick={handleSend}
                      disabled={!query.trim()}
                      className={`p-3 rounded-full transition-all flex-shrink-0 ${
                        query.trim()
                          ? "bg-black hover:bg-gray-800 text-white transform hover:scale-[1.02] active:scale-[0.98]"
                          : "bg-gray-300 text-gray-500 cursor-not-allowed"
                      }`}
                    >
                      <FaArrowUp className="text-base" />
                    </button>
                  </div>
                </div>
              </div>
            ) : (
              <div className="max-w-4xl mx-auto w-full">
                {responseData.query && (
                  <div className="mb-6 flex justify-end">
                    <div className="bg-black text-white p-4 rounded-2xl rounded-tr-none max-w-[85%] sm:max-w-md shadow-sm">
                      <p className="text-sm sm:text-base">
                        {responseData.query}
                      </p>
                    </div>
                  </div>
                )}

                {loadingStates.response.visible ? (
                  loadingStates.response.loading ? (
                    <div className="mb-6">
                      <div className="bg-gray-50 border border-gray-200 p-4 rounded-2xl shadow-sm max-w-[85%] sm:max-w-md">
                        <div className="flex items-center space-x-2">
                          <p className="text-sm text-gray-500">
                            {getLoadingMessage("response")}
                          </p>
                          <div className="w-4 h-4 border-2 border-gray-300 border-t-black rounded-full animate-spin"></div>
                        </div>
                      </div>
                    </div>
                  ) : responseData.response ? (
                    <div className="mb-6">
                      <div className="bg-gray-50 border border-gray-200 p-4 rounded-2xl shadow-sm max-w-[85%] sm:max-w-md mb-4">
                        <p className="text-sm sm:text-base text-gray-900">
                          {responseData.response}
                        </p>
                      </div>
                      <AccessManagement />
                    </div>
                  ) : null
                ) : isLoading &&
                  !Object.values(loadingStates).some(
                    (state) => state.visible
                  ) ? (
                  <div className="mb-6">
                    <div className="bg-gray-50 border border-gray-200 p-4 rounded-2xl shadow-sm max-w-md">
                      <div className="flex items-center space-x-2">
                        <div
                          className="w-2 h-2 bg-gray-400 rounded-full animate-bounce"
                          style={{ animationDelay: "0ms" }}
                        ></div>
                        <div
                          className="w-2 h-2 bg-gray-400 rounded-full animate-bounce"
                          style={{ animationDelay: "150ms" }}
                        ></div>
                        <div
                          className="w-2 h-2 bg-gray-400 rounded-full animate-bounce"
                          style={{ animationDelay: "300ms" }}
                        ></div>
                      </div>
                    </div>
                  </div>
                ) : null}
              </div>
            )}
          </div>

          {/* Input Area - Only shown after interaction */}
          {hasInteracted && (
            <div className="p-4 sm:p-6 bg-white border-t border-gray-200">
              <div className="max-w-4xl mx-auto flex items-center gap-3 bg-gray-50 rounded-3xl border-2 border-gray-300 px-4 py-3 focus-within:border-gray-400 focus-within:shadow-sm transition-all">
                {/* Plus icon */}
                <button
                  className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                  title="Add attachment"
                  type="button"
                >
                  <FaPlus className="text-base" />
                </button>

                {/* Input */}
                <input
                  type="text"
                  value={query}
                  onChange={(e) => setQuery(e.target.value)}
                  className="flex-1 px-2 py-2 focus:outline-none bg-transparent text-gray-900 placeholder-gray-500"
                  placeholder="Type your message..."
                  onKeyDown={(e) => e.key === "Enter" && handleSend()}
                />

                {/* Voice icon */}
                <button
                  className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                  title="Voice input"
                  type="button"
                >
                  <FaMicrophone className="text-base" />
                </button>

                {/* Send button */}
                <button
                  onClick={handleSend}
                  disabled={!query.trim()}
                  className={`p-2.5 rounded-full transition-all flex-shrink-0 ${
                    query.trim()
                      ? "bg-black hover:bg-gray-800 text-white transform hover:scale-[1.02] active:scale-[0.98]"
                      : "bg-gray-300 text-gray-500 cursor-not-allowed"
                  }`}
                >
                  <FaArrowUp className="text-sm" />
                </button>
              </div>
            </div>
          )}
        </div>

        {/* Right Panel - Only visible after interaction */}
        {hasInteracted && (
          <div className="hidden lg:block w-1/4 bg-white border-l border-gray-200 overflow-hidden">
            <div className="p-4 h-full">
              <RightComponents />
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default PasswordAgent;
