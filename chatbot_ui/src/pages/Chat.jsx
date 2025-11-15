import React, { useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import logo from "../assets/image.png";
import { FaArrowUp, FaPlus, FaMicrophone } from "react-icons/fa";

const Chat = ({ api }) => {
  const [inputMessage, setInputMessage] = useState("");
  const [currentQuestion, setCurrentQuestion] = useState(null);
  const [currentResponse, setCurrentResponse] = useState(null);
  const [selectedOption, setSelectedOption] = useState(api);
  const [isLoading, setIsLoading] = useState(false);
  const [showInitialInput, setShowInitialInput] = useState(true);
  const [source, setSource] = useState();
  const [content, setContent] = useState([]);
  const [links, setLinks] = useState([]);
  const [images, setImages] = useState([]);
  const [showSatisfactionQuestion, setShowSatisfactionQuestion] =
    useState(false);
  const [thankYouMessage, setThankYouMessage] = useState("");
  const [additionalResponse, setAdditionalResponse] = useState("");

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    setCurrentQuestion(inputMessage);
    setShowInitialInput(false);
    setInputMessage("");
    setCurrentResponse(" ");
    setContent([]);
    setLinks([]);
    setImages([]);
    setIsLoading(true);
    setShowSatisfactionQuestion(false);
    setThankYouMessage("");
    setAdditionalResponse("");

    try {
      const response = await fetch(
        `${import.meta.env.VITE_API_URL}/${selectedOption}`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ query: inputMessage }),
        }
      );

      if (!response.ok) {
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }

      const text = await response.text();
      const data = JSON.parse(text);
      const sources = data.sources || [];

      if (sources.length > 0) {
        const updatedContent = sources.map(
          (source) => source.page_content || " "
        );
        setContent(updatedContent);
      } else {
        setContent([]);
      }

      setCurrentResponse(data.answer || " ");
      setLinks(data.links || []);
      setShowSatisfactionQuestion(true);
    } catch (error) {
      console.error("Error fetching data:", error);
      setCurrentResponse("Error fetching response.");
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="flex h-full bg-white relative break-words">
      <div className="flex-1 flex flex-col relative">
        {isLoading && (
          <div className="absolute inset-0 bg-gray-100 bg-opacity-75 flex justify-center items-center z-10">
            <div className="animate-spin rounded-full h-8 w-8 sm:h-12 sm:w-12 border-b-4 border-black"></div>
          </div>
        )}

        <div className="flex-1 overflow-y-auto bg-white">
          <div className="h-full flex flex-col">
            <div className="flex-1 flex flex-col">
              {showInitialInput ? (
                <div className="flex-1 flex items-center justify-center px-4 py-8">
                  <div className="w-full max-w-4xl mx-auto">
                    {/* Logo and Grise Section */}
                    <div className="flex flex-col sm:flex-row items-center justify-center mb-8 sm:mb-12">
                      {/* Logo */}
                      <div className="mb-4 sm:mb-0 sm:mr-6">
                        <img 
                          src={logo} 
                          alt="Logo" 
                          className="w-16 h-16 sm:w-20 sm:h-20 lg:w-24 lg:h-24"
                        />
                      </div>
                      
                      {/* Grise Text with Plus Badge */}
                      <div className="flex items-center">
                        <h1 className="text-2xl sm:text-3xl lg:text-4xl font-bold text-gray-800 mr-3">
                          G-RISE
                        </h1>
                        <span className="bg-blue-200 text-gray-800 text-sm sm:text-sm lg:text-sm font-semibold px-4 py-1 rounded-md border-2 ">
                          Plus
                        </span>
                      </div>
                    </div>

                    {/* Input Section */}
                    <div className="relative w-full max-w-2xl mx-auto">
                      <div className="relative flex items-center gap-3 bg-gray-50 rounded-3xl border-2 border-gray-300 px-4 py-3 focus-within:border-gray-400 focus-within:shadow-lg transition-all duration-200 hover:shadow-xl">
                        {/* Plus icon for attachments */}
                        <button
                          className="p-2.5 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                          title="Add attachment"
                          type="button"
                        >
                          <FaPlus className="text-lg" />
                        </button>

                        {/* Text input */}
                        <input
                          type="text"
                          value={inputMessage}
                          onChange={(e) => setInputMessage(e.target.value)}
                          placeholder="Type your question..."
                          className="flex-1 px-2 py-3 focus:outline-none bg-transparent text-base sm:text-lg text-black placeholder-gray-500"
                          onKeyPress={(e) => {
                            if (e.key === "Enter") {
                              e.preventDefault();
                              handleSendMessage();
                            }
                          }}
                        />

                        {/* Voice icon */}
                        <button
                          className="p-2.5 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                          title="Voice input"
                          type="button"
                        >
                          <FaMicrophone className="text-lg" />
                        </button>

                        {/* Send Button */}
                        <button
                          onClick={handleSendMessage}
                          disabled={inputMessage.trim() === ""}
                          className={`p-3 rounded-full transition-all flex-shrink-0 ${
                            inputMessage.trim()
                              ? "bg-black hover:bg-gray-800 text-white"
                              : "bg-gray-300 text-gray-500 cursor-not-allowed"
                          }`}
                        >
                          <FaArrowUp className="text-base" />
                        </button>
                      </div>
                    </div>
                  </div>
                </div>
              ) : (
                <div className="flex-1 flex flex-col p-4 sm:p-6 lg:p-8 overflow-y-auto max-h-[calc(100vh-120px)]">
                  <div className="max-w-6xl w-full mx-auto">
                    {currentQuestion && (
                      <div className="mb-4 sm:mb-6">
                        <div className="text-black font-bold text-lg sm:text-xl lg:text-2xl whitespace-normal">
                          <h1>{currentQuestion}</h1>
                        </div>
                      </div>
                    )}
                    {additionalResponse && (
                      <div className="mb-4 sm:mb-6">
                        <h1 className="py-2 font-bold text-base sm:text-lg">
                          Response generated by LLM
                        </h1>
                        <div className="prose prose-sm sm:prose-base max-w-none">
                          <ReactMarkdown
                            className="markdown-body"
                            remarkPlugins={[remarkGfm]}
                          >
                            {additionalResponse}
                          </ReactMarkdown>
                        </div>
                      </div>
                    )}
                    {currentResponse && (
                      <div className="mb-4 sm:mb-6">
                        {selectedOption === "analyze" ? (
                          <div
                            className="prose prose-sm sm:prose-base max-w-none"
                            dangerouslySetInnerHTML={{
                              __html: currentResponse,
                            }}
                          />
                        ) : (
                          <div className="prose prose-sm sm:prose-base max-w-none">
                            <ReactMarkdown
                              className="markdown-body"
                              remarkPlugins={[remarkGfm]}
                            >
                              {currentResponse}
                            </ReactMarkdown>
                          </div>
                        )}
                      </div>
                    )}
                    {images.length > 0 && (
                      <div className="mb-4 sm:mb-6 flex flex-col space-y-4">
                        {images.map((image, index) => (
                          <img
                            key={index}
                            src={image}
                            alt={`Fetched ${index}`}
                            className="max-w-full h-auto rounded-lg shadow-md"
                          />
                        ))}
                      </div>
                    )}

                    {content.length > 0 && (
                      <div className="mt-4 sm:mt-6">
                        <h2 className="font-bold text-base sm:text-lg mb-3">
                          Related Information:
                        </h2>
                        <ul className="list-disc pl-5 space-y-2 text-sm sm:text-base">
                          {content.map((item, index) => (
                            <li key={index}>
                              {item.replace(/^prompt:\s*/, "")}
                            </li>
                          ))}
                        </ul>
                      </div>
                    )}
                    {links.length > 0 && (
                      <div className="mt-4 sm:mt-6">
                        <h2 className="font-bold text-base sm:text-lg mb-3">
                          Related Links:
                        </h2>
                        <ul className="list-disc ml-5 mt-2 space-y-2">
                          {links.map((link, index) => (
                            <li key={index}>
                              <a
                                href={link}
                                target="_blank"
                                rel="noopener noreferrer"
                                className="text-blue-500 underline hover:text-blue-700 text-sm sm:text-base break-all"
                              >
                                {link}
                              </a>
                            </li>
                          ))}
                        </ul>
                      </div>
                    )}
                  </div>
                </div>
              )}
              {!showInitialInput && (
                <div className="p-3 sm:p-4 bg-white border-t border-gray-200">
                  <div className="max-w-6xl mx-auto">
                    <div className="relative flex items-center gap-3 bg-gray-50 rounded-3xl border-2 border-gray-300 px-4 py-3 focus-within:border-gray-400 focus-within:shadow-sm transition-all duration-200">
                      {/* Plus icon for attachments */}
                      <button
                        className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                        title="Add attachment"
                        type="button"
                      >
                        <FaPlus className="text-base" />
                      </button>

                      {/* Text input */}
                      <input
                        type="text"
                        value={inputMessage}
                        onChange={(e) => setInputMessage(e.target.value)}
                        placeholder="Type your question..."
                        className="flex-1 px-2 py-2 focus:outline-none bg-transparent text-sm sm:text-base text-black placeholder-gray-500"
                        onKeyPress={(e) => {
                          if (e.key === "Enter") {
                            e.preventDefault();
                            handleSendMessage();
                          }
                        }}
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
                        onClick={handleSendMessage}
                        disabled={inputMessage.trim() === ""}
                        className={`p-2.5 rounded-full transition-all flex-shrink-0 ${
                          inputMessage.trim()
                            ? "bg-black hover:bg-gray-800 text-white"
                            : "bg-gray-300 text-gray-500 cursor-not-allowed"
                        }`}
                      >
                        <FaArrowUp className="text-sm" />
                      </button>
                    </div>
                  </div>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Chat;