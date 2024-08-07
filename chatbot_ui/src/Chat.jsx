import React, { useState, useRef, useEffect } from "react";
import { FiUser } from "react-icons/fi";
import { BsArrowUpLeft } from "react-icons/bs";
import { RiRobot2Line } from "react-icons/ri";
import {
  FaUserCircle,
  FaHeadset,
  FaFilePdf,
  FaArrowUp,
  FaDatabase,
} from "react-icons/fa";
import chatbotIntro from "./assets/ai.png";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import jsonData from './csvjson.json'

const options = [
  { label: "Support Help", api: "csvchat", icon: <FaHeadset /> },
  { label: "Pdf Reader", api: "pdfchat", icon: <FaFilePdf /> },
  { label: "Abap Code Generator", api: "pdfchat", icon: <FaDatabase /> },
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
  const [selectedOption, setSelectedOption] = useState("csvchat");
  const [selectedLabel, setSelectedLabel] = useState("Support Help");
  const [model, setModel] = useState("OpenAI");
  const [models, setModels] = useState([]);
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [firstInteraction, setFirstInteraction] = useState(true);
  const [chatHistories, setChatHistories] = useState({
    csvchat: [],
    pdfchat: [],
    "query-sap": [],
  });
  const [supportData, setSupportData] = useState({});

  useEffect(() => {
    const fetchModels = async () => {
      try {
        const res = await fetch(`${import.meta.env.VITE_API_URL}/get_models`, {
          method: "GET",
          headers: {
            "Content-Type": "application/json",
          },
        });
        if (!res.ok) {
          throw new Error(`HTTP error! Status: ${res.status}`);
        }
        const data = await res.json();
        setModels(data.models);
      } catch (error) {
        console.error("Error fetching models:", error);
      }
    };

    const fetchSupportData = async () => {
      try {   
        setSupportData(jsonData);
      } catch (error) {
        console.error("Error fetching support data:", error);
      }
    };
    

    fetchModels();
    fetchSupportData();
  }, []);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    const updatedHistory = [
      ...chatHistories[selectedOption],
      { text: inputMessage, sender: "user" },
    ];
    setChatHistories((prevHistories) => ({
      ...prevHistories,
      [selectedOption]: updatedHistory,
    }));

    setMessages(updatedHistory);
    setInputMessage("");
    setIsLoading(true);
    setFirstInteraction(false);

    if (selectedLabel === "Support Help") {
      const matchedData = supportData.find((item) =>
        item.prompt.toLowerCase().includes(inputMessage.trim().toLowerCase())
      );
      const response = matchedData
        ? matchedData.response
        : "Sorry, I don't have information on that topic.";
      setChatHistories((prevHistories) => ({
        ...prevHistories,
        [selectedOption]: [...updatedHistory, { text: response, sender: "ai" }],
      }));
      setMessages((prevMessages) => [
        ...prevMessages,
        { text: response, sender: "ai" },
      ]);
      setIsLoading(false);
    } else {
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
          throw new Error(
            `Network response was not ok: ${response.statusText}`
          );
        }

        const text = await response.text();
        const data = JSON.parse(text);
        const answer = JSON.parse(data.answer);

        setChatHistories((prevHistories) => ({
          ...prevHistories,
          [selectedOption]: [
            ...updatedHistory,
            {
              text: answer.answer,
              source: answer.source === "llm" ? "llm" : data.source,
              sender: "ai",
            },
          ],
        }));

        setMessages((prevMessages) => [
          ...prevMessages,
          {
            text: answer.answer,
            source: answer.source === "llm" ? "llm" : data.source,
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
    }
  };

  const handleOptionClick = (optionApi, optionLabel) => {
    setSelectedOption(optionApi);
    setSelectedLabel(optionLabel);
    setMessages(chatHistories[optionApi]);
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
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }
    } catch (error) {
      console.error("Error changing model:", error);
    }
  };

  return (
    <div className="flex h-screen bg-white">
      <div className="w-56 bg-gray-100">
        <img src={chatbotIntro} alt="SAP Logo" className="h-34 mr-2" />
        <div className="text-black p-10 hidden md:block">
          <ul className="space-y-4">
            {options.map((option, index) => (
              <li
                onClick={() => handleOptionClick(option.api, option.label)}
                key={index}
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
      <div className="flex-1 flex flex-col">
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
                  {message.sender === "ai" && message.source !== "llm" && (
                    <p>source: {message.source}</p>
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
              ? "absolute inset-x-0 bottom-1/2 transform translate-x-[60%] w-1/2 translate-y-1/2 border-2 border-gray-600 rounded-xl shadow-xl h-24 items-center justify-center"
              : ""
          } bg-white border-t border-gray-200 p-4`}
        >
          <div className="flex items-center w-full">
            <input
              type="text"
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              placeholder="Type your message..."
              className="flex-1 border border-gray-300 rounded-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-green-700 text-black"
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  e.preventDefault();
                  handleSendMessage();
                }
              }}
              onFocus={() => {
                if (firstInteraction && selectedLabel !== "Support Help") {
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
                {models.map((option, index) => (
                  <option key={index} value={option.value}>
                    {option.name}
                  </option>
                ))}
              </select>
            </div>

            <button
              onClick={handleSendMessage}
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
          {showSuggestions && selectedLabel !== "Support Help" && (
            <div className="mt-4 w-full bg-white rounded-2xl p-4">
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
