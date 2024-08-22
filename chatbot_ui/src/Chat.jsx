import React, { useState, useEffect, useRef } from "react";
import { FaArrowUp, FaFilePdf, FaHeadset, FaUser } from "react-icons/fa";
import chatbotIntro from "./assets/ai.png";

const options = [
  { label: "Support Help", api: "csvchat", icon: <FaHeadset /> },
  { label: "Pdf Reader", api: "pdfchat", icon: <FaFilePdf /> },
];

const Chat = () => {
  const [inputMessage, setInputMessage] = useState("");
  const [currentQuestion, setCurrentQuestion] = useState(null);
  const [currentResponse, setCurrentResponse] = useState(null);
  const [selectedOption, setSelectedOption] = useState("csvchat");
  const [selectedLabel, setSelectedLabel] = useState("Support Help");
  const [isLoading, setIsLoading] = useState(false);
  const [showInitialInput, setShowInitialInput] = useState(true);
  const chatEndRef = useRef(null);

  useEffect(() => {
    chatEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [currentResponse]);

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    setCurrentQuestion(inputMessage);
    setShowInitialInput(false);
    setInputMessage("");
    setCurrentResponse(" ");
    setIsLoading(true);

    try {
      const response = await fetch(
        `${import.meta.env.VITE_API_URL}/${selectedOption}`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ prompt: inputMessage }),
        }
      );

      if (!response.ok) {
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }

      const text = await response.text();
      const data = JSON.parse(text);

      setCurrentResponse(data.answer || "No response available.");
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
  };

  return (
    <div className="flex h-screen bg-white relative">
      {/* Sidebar */}
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

      {/* Main chat area */}
      <div className="flex-1 flex flex-col relative">
        {/* Loader overlay */}
        {isLoading && (
          <div className="absolute inset-0 bg-gray-100 bg-opacity-75 flex justify-center items-center z-10">
            <div className="animate-spin rounded-full h-12 w-12 border-b-4 border-green-700"></div>
          </div>
        )}

        {/* Header */}
        <header className="bg-white border-b border-gray-200 p-2 flex justify-between items-center">
          <div className="text-green-900 text-xl font-bold">{selectedLabel}</div>
          <div className="text-green-900 text-3xl">
            <FaUser />
          </div>
        </header>

        {/* Chat content */}
        <div className="flex-1 p-2 overflow-y-auto bg-gray-50 flex flex-col">
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
            <>
              {currentQuestion && (
                <div className="w-full mb-2">
                  <div className="p-3 rounded-lg bg-green-700 text-white">
                    {currentQuestion}
                  </div>
                </div>
              )}
              {currentResponse && (
                <div className="w-full mb-2">
                  <div className="p-3 rounded-lg bg-gray-200 text-black">
                    {currentResponse}
                  </div>
                </div>
              )}
            </>
          )}
          <div ref={chatEndRef} />
        </div>

        {/* Input area */}
        {!showInitialInput && (
          <div className="p-2 bg-white border-t border-gray-200">
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
              <button
                onClick={handleSendMessage}
                disabled={isLoading}
                className={`ml-2 px-6 py-2 rounded-lg bg-green-700 text-white ${
                  isLoading
                    ? "opacity-50 cursor-not-allowed"
                    : "hover:bg-green-900"
                } focus:outline-none focus:ring-2 focus:ring-green-700`}
              >
                {isLoading ? (
                  <div className="animate-spin rounded-full h-5 w-5 border-b-2 border-white"></div>
                ) : (
                  <FaArrowUp />
                )}
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default Chat;
