import React, { useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import logo from "../assets/image.png";
import { FaArrowAltCircleUp, FaArrowUp, FaLongArrowAltUp } from "react-icons/fa";

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

      // const img = data.images || [];
      // if (img.length > 0) {
      //   setImages(img.map((image) => `data:image/png;base64,${image}`));
      // } else {
      //   setImages([]);
      // }

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
                          G-Rise
                        </h1>
                        <span className="bg-blue-200 text-gray-800 text-sm sm:text-sm lg:text-sm font-semibold px-4 py-1 rounded-md border-2 ">
                          Plus
                        </span>
                      </div>
                    </div>

                    {/* Input Section */}
                    <div className="relative w-full max-w-2xl mx-auto">
                      <div className="relative">
                        <input
                          type="text"
                          value={inputMessage}
                          onChange={(e) => setInputMessage(e.target.value)}
                          placeholder="Type your question..."
                          className="w-full h-14 sm:h-16 lg:h-20 border-2 border-gray-300 rounded-2xl px-6 pr-16 py-4 text-base sm:text-lg focus:outline-none focus:ring-2 focus:ring-black focus:border-transparent text-black shadow-lg transition-all duration-200 hover:shadow-xl"
                          onKeyPress={(e) => {
                            if (e.key === "Enter") {
                              e.preventDefault();
                              handleSendMessage();
                            }
                          }}
                        />
                        {/* Send Button */}
                        <button
                          onClick={handleSendMessage}
                          disabled={inputMessage.trim() === ""}
                          className="absolute right-3 top-1/2 transform -translate-y-1/2 p-2.5 sm:p-3 bg-black text-white rounded-full hover:bg-gray-800 focus:outline-none focus:ring-2 focus:ring-black disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                        >
                        <FaArrowUp  className="text-xl"/>
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
                <div className="p-3 sm:p-4 bg-gray-50 border-t border-gray-200">
                  <div className="max-w-6xl mx-auto">
                    <div className="relative">
                      <input
                        type="text"
                        value={inputMessage}
                        onChange={(e) => setInputMessage(e.target.value)}
                        placeholder="Type your question..."
                        className="w-full h-12 sm:h-14 border-2 border-gray-300 rounded-xl px-4 pr-14 py-2 text-sm sm:text-base focus:outline-none focus:ring-2 focus:ring-black focus:border-transparent text-black transition-all duration-200"
                        onKeyPress={(e) => {
                          if (e.key === "Enter") {
                            e.preventDefault();
                            handleSendMessage();
                          }
                        }}
                      />
                      <button
                        onClick={handleSendMessage}
                        disabled={inputMessage.trim() === ""}
                        className="absolute right-2 top-1/2 transform -translate-y-1/2 p-2 sm:p-2.5 bg-black text-white rounded-lg hover:bg-gray-800 focus:outline-none focus:ring-2 focus:ring-black disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
                      >
                        <svg 
                          width="16" 
                          height="16" 
                          viewBox="0 0 24 24" 
                          fill="currentColor"
                        >
                          <path d="M12 4l-1.41 1.41L16.17 11H4v2h12.17l-5.58 5.59L12 20l8-8z"/>
                        </svg>
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