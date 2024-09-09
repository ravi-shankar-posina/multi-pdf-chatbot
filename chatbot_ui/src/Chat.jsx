import React, { useState } from "react";
import {
  FaCode,
  FaFilePdf,
  FaHeadset,
  FaUser,
} from "react-icons/fa";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import chatbotIntro from "./assets/ai.png";

const options = [
  { label: "Support Help", api: "csv/query", icon: <FaHeadset /> },
  { label: "Pdf Reader", api: "pdf/query", icon: <FaFilePdf /> },
  { label: "ABAP Code Genarator", api: "pdf/query", icon: <FaCode /> },
];

const Chat = () => {
  const [inputMessage, setInputMessage] = useState("");
  const [currentQuestion, setCurrentQuestion] = useState(null);
  const [currentResponse, setCurrentResponse] = useState(null);
  const [selectedOption, setSelectedOption] = useState("csv/query");
  const [selectedLabel, setSelectedLabel] = useState("Support Help");
  const [isLoading, setIsLoading] = useState(false);
  const [showInitialInput, setShowInitialInput] = useState(true);
  const [source, setSource] = useState();
  const [content, setContent] = useState("");
  const [links, setLinks] = useState([]);

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    setCurrentQuestion(inputMessage);
    setShowInitialInput(false);
    setInputMessage("");
    setCurrentResponse(" ");
    setContent(" ");
    setLinks([]);
    setIsLoading(true);

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
        setContent(sources[0].content || " ");
      } else {
        setContent(" ");
      }

      setCurrentResponse(data.answer || " ");
      setLinks(data.link_info || []);
    } catch (error) {
      console.error("Error fetching data:", error);
      setCurrentResponse("Error fetching response.");
    } finally {
      setIsLoading(false);
    }
  };

  const handleOptionClick = (optionApi, optionLabel) => {
    setSelectedOption(optionApi);
    setSelectedLabel(optionLabel);
    setInputMessage("");
    setCurrentQuestion(null);
    setCurrentResponse(null);
    setShowInitialInput(true);
    if (optionLabel === "ABAP Code Genarator") {
      setSource("");
    }
  };

  return (
    <div className="flex h-screen bg-white relative break-words">
      <div className="w-56 bg-gray-100">
        <img src={chatbotIntro} alt="Chatbot Intro" className="h-34 mr-2" />
        <div className="text-black p-10 hidden md:block">
          <ul className="space-y-4">
            {options.map((option, index) => (
              <li
                key={index}
                onClick={() => handleOptionClick(option.api, option.label)}
                className={`flex items-center text-sm font-bold cursor-pointer p-2 rounded-lg transition duration-300 ${
                  selectedLabel === option.label ? "bg-gray-300" : ""
                } hover:bg-gray-200`}
              >
                <div className="mr-2">{option.icon}</div>
                {option.label}
              </li>
            ))}
          </ul>
        </div>
      </div>
      <div className="flex-1 flex flex-col relative">
        {isLoading && (
          <div className="absolute inset-0 bg-gray-100 bg-opacity-75 flex justify-center items-center z-10">
            <div className="animate-spin rounded-full h-12 w-12 border-b-4 border-green-700"></div>
          </div>
        )}
        <header className="bg-white border-b border-gray-200 p-2 flex justify-between items-center">
          <div className="text-green-900 text-xl font-bold">
            {selectedLabel}
          </div>
          <div className="text-green-900 text-3xl">
            <FaUser />
          </div>
        </header>
        <div className="flex-1 p-8 overflow-y-auto bg-white flex flex-col max-w-full">
          {showInitialInput ? (
            <div className="flex-1 flex items-center justify-center">
              <div className="w-full px-2">
                <input
                  type="text"
                  value={inputMessage}
                  onChange={(e) => setInputMessage(e.target.value)}
                  placeholder="Type your question..."
                  className="w-[90%] ml-20 h-24 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
                  onKeyPress={(e) => {
                    if (e.key === "Enter") {
                      e.preventDefault();
                      handleSendMessage();
                    }
                  }}
                />
              </div>
            </div>
          ) : (
            <div className="bg-white rounded-lg p-3 w-full break-words max-w-full">
              {currentQuestion && (
                <div className="w-full mb-2">
                  <div className="text-black font-bold text-xl whitespace-normal">
                    <h1>{currentQuestion}</h1>
                  </div>
                </div>
              )}
              {currentResponse && (
                <div className="mb-2 overflow-x-auto">
                  <ReactMarkdown
                    className="markdown-body"
                    remarkPlugins={[remarkGfm]}
                  >
                    {currentResponse}
                  </ReactMarkdown>
                </div>
              )}
              {content && (
                <>
                  <p>{content}</p>
                </>
              )}
              {links.length > 0 && (
                <div className="mt-4">
                  <h2 className="font-bold text-lg">Related Information:</h2>
                  <ul className="list-disc ml-5 mt-2">
                    {links.map((link, index) => (
                      <li key={index}>
                        <a
                          href={link.link}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-blue-500 underline"
                        >
                          {link.text}
                        </a>
                      </li>
                    ))}
                  </ul>
                </div>
              )}
              {/* {source} */}
            </div>
          )}
        </div>
        {!showInitialInput && (
          <div className="p-2 bg-gray-50 border-t border-gray-200">
            <div className="flex">
              <input
                type="text"
                value={inputMessage}
                onChange={(e) => setInputMessage(e.target.value)}
                placeholder="Type your question..."
                className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
                onKeyPress={(e) => {
                  if (e.key === "Enter") {
                    e.preventDefault();
                    handleSendMessage();
                  }
                }}
              />
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default Chat;
