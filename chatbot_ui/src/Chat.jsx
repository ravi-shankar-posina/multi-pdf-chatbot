import React, { useState, useRef, useEffect } from "react";
import { FiUser } from "react-icons/fi";
import { BsArrowUpLeft } from "react-icons/bs";
import { RiRobot2Line } from "react-icons/ri";
import {
  FaUserCircle,
  FaHeadset,
  FaFilePdf,
  FaDatabase,
  FaArrowUp,
} from "react-icons/fa";
import chatbotIntro from "./assets/ai.png";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

const options = [
  { label: "Support Help", api: "csvchat", icon: <FaHeadset /> },
  { label: "Pdf Reader", api: "pdfchat", icon: <FaFilePdf /> },
  { label: "Query SAP", api: "query-sap", icon: <FaDatabase /> },
];

const quotes = [
  "Tell me about SAP sales order.",
  "Innovate with SAP technology.",
  "Empower your enterprise with SAP solutions.",
  "Transform your business with intelligent ERP.",
  "SAP: Driving digital transformation.",
];

const Chat = () => {
  const [messages, setMessages] = useState([]);
  const [inputMessage, setInputMessage] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const messagesEndRef = useRef(null);
  const [selectedOption, setSelectedOption] = useState("");
  const [model, setModel] = useState("OpenAI");
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [firstInteraction, setFirstInteraction] = useState(true);

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(scrollToBottom, [messages]);

  const handleSendMessage = async (optionApi) => {
    console.log(optionApi);
    setShowSuggestions(false);
    if (inputMessage.trim() === "") return;

    setMessages([...messages, { text: inputMessage, sender: "user" }]);
    setInputMessage("");
    setIsLoading(true);
    setShowSuggestions(false);
    setFirstInteraction(false);

    try {
      const response = await fetch(
        `${import.meta.env.VITE_API_URL}/${optionApi}`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ prompt: inputMessage }),
        }
      );

      if (!response.ok) {
        console.error(`Network response was not ok: ${response.statusText}`);
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }

      const data = await response.text();

      console.log("API Response:", data);

      setMessages((prevMessages) => [
        ...prevMessages,
        {
          text: data,
          sender: "ai",
        },
      ]);
    } catch (error) {
      console.error("Error fetching data:", error);
      setMessages((prevMessages) => [
        ...prevMessages,
        { text: "Error fetching response.", sender: "ai" },
      ]);
    } finally {
      setIsLoading(false);
    }
  };

  const handleModelChange = async (selectedModel) => {
    setShowSuggestions(false);
    setModel(selectedModel);
    try {
      const response = await fetch(
        `${import.meta.env.VITE_API_URL}/change_model`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({ model: selectedModel }),
        }
      );

      if (!response.ok) {
        console.error(`Network response was not ok: ${response.statusText}`);
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }

      console.log(`Model changed to ${selectedModel}`);
    } catch (error) {
      console.error("Error changing model:", error);
    }
  };

  return (
    <div className="flex h-screen bg-white">
      <div className="w-56 bg-gray-100">
        <div>
          <img src={chatbotIntro} alt="SAP Logo" className="h-28 mr-2" />
        </div>
        <div className="text-black p-10 hidden md:block">
          <ul className="space-y-4">
            {options.map((option, index) => (
              <li
                onClick={() => {
                  handleSendMessage(option.api);
                  setSelectedOption(option.api); // Set selectedOption to option.api
                }}
                key={index}
                className={`flex items-center text-sm font-bold cursor-pointer p-2 rounded-lg transition duration-300 ${
                  selectedOption === option.api ? "bg-gray-300" : "" // Compare with option.api
                } hover:bg-gray-200`}
              >
                <div className="mr-2">{option.icon}</div>
                {option.label}
              </li>
            ))}
          </ul>
        </div>
      </div>
      <div className="flex-1 flex flex-col ">
        <header className="bg-white border-2 rounded-lg border-gray-200 m-1 p-2 flex justify-end items-center">
          <div className="text-green-900 text-3xl">
            <FaUserCircle className="h-10 w-10" />
          </div>
        </header>

        <div className="flex-1 overflow-y-auto p-4">
          {messages.map((message, index) => (
            <div
              key={index}
              className={`flex ${
                message.sender === "user" ? "justify-end" : "justify-start"
              } mb-4`}
            >
              <div
                className={`flex items-end ${
                  message.sender === "user" ? "flex-row" : "flex-row-reverse"
                }`}
              >
                <div
                  className={`${
                    message.sender === "user"
                      ? "bg-gray-500 text-white"
                      : "bg-white"
                  } rounded-lg p-3 shadow-md max-w-7xl lg:max-w-7xl`}
                >
                  {message.sender === "ai" && message.source && (
                    <p>
                      source :{" "}
                      <a
                        href={message.source}
                        target="_blank"
                        rel="noopener noreferrer"
                        className="text-blue-700"
                      >
                        {message.source}
                      </a>
                    </p>
                  )}
                  <ReactMarkdown
                    className="markdown-body"
                    remarkPlugins={[remarkGfm]}
                  >
                    {message.text}
                  </ReactMarkdown>
                </div>
                <div
                  className={`${
                    message.sender === "user" ? "ml-2" : "mr-2"
                  } text-2xl`}
                >
                  {message.sender === "user" ? <FiUser /> : <RiRobot2Line />}
                </div>
              </div>
            </div>
          ))}
          {isLoading && (
            <div className="flex justify-center mt-2">
              <div className="animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-green-700"></div>
              <h1 className="ml-4">Loading...</h1>
            </div>
          )}
          <div ref={messagesEndRef} />
        </div>
        <div
          className={`${
            messages.length === 0
              ? "absolute inset-x-0 bottom-1/2 transform translate-x-[60%] w-1/2 translate-y-1/2 border-2 border-gray-600 rounded-xl shadow-xl h-24  items-center justify-center"
              : ""
          } bg-white border-t border-gray-200 p-4`}
        >
          <div className="flex items-center w-full ">
            <input
              type="text"
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              placeholder="Type your message..."
              className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
              onKeyPress={(e) =>
                e.key === "Enter" && handleSendMessage(selectedOption)
              }
              onFocus={() => {
                if (firstInteraction) {
                  setShowSuggestions(true);
                }
              }}
            />
            <div className="text-green-900 text-sm ml-4">
              <select
                value={model}
                onChange={(e) => handleModelChange(e.target.value)}
                className="border border-gray-300 rounded-lg px-2 py-1 focus:outline-none focus:ring-2 focus:ring-green-900 text-black"
              >
                <option value="openai">OpenAI</option>
                <option value="gemini">Gemini</option>
              </select>
            </div>
            <button
              onClick={() => handleSendMessage(selectedOption)}
              disabled={isLoading}
              className={`ml-4 px-4 py-2 rounded-lg bg-green-700 text-white ${
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
          {showSuggestions && (
            <div className="mt-4 w-full bg-white  rounded-2xl p-4">
              <ul className="space-y-2">
                {quotes.map((quote, index) => (
                  <li
                    key={index}
                    onClick={() => {
                      setInputMessage(quote);
                      setShowSuggestions(false);
                    }}
                    className="cursor-pointer text-black flex justify-between hover:bg-slate-200 p-2 rounded-lg"
                  >
                    {quote}

                    <BsArrowUpLeft />
                  </li>
                ))}
              </ul>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default Chat;
