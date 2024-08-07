// ChatComponent.jsx
import React, { useState, useRef, useEffect } from "react";
import { FiUser } from "react-icons/fi";
import { BsArrowUpLeft } from "react-icons/bs";
import { RiRobot2Line } from "react-icons/ri";
import { FaUserCircle, FaArrowUp } from "react-icons/fa";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

const quotes = [
  "Tell me about SAP sales order.",
  "Innovate with SAP technology.",
  "Empower your enterprise with SAP solutions.",
  "Transform your business with intelligent ERP.",
  "SAP: Driving digital transformation.",
];

const ChatComponent = ({ api, label }) => {
  const [messages, setMessages] = useState([]);
  const [inputMessage, setInputMessage] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const messagesEndRef = useRef(null);
  const [model, setModel] = useState("OpenAI");
  const [models, setModels] = useState([]);
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [firstInteraction, setFirstInteraction] = useState(true);

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

    fetchModels();
  }, []);

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    setMessages((prevMessages) => [
      ...prevMessages,
      { text: inputMessage, sender: "user" },
    ]);
    setInputMessage("");
    setIsLoading(true);
    setFirstInteraction(false);

    try {
      const response = await fetch(
        `${import.meta.env.VITE_API_URL}/${api}`,
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
      const answer = JSON.parse(data.answer);

      setMessages((prevMessages) => [
        ...prevMessages,
        { text: answer.answer, source: (answer.source === "llm") ? 'llm' : data.source, sender: "ai" },
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
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }
    } catch (error) {
      console.error("Error changing model:", error);
    }
  };

  return (
    <div className="flex-1 flex flex-col">
      <header className="bg-white border-2 rounded-lg border-gray-200 m-1 p-2 flex justify-between items-center">
        <h1 className="text-xl font-bold">{label}</h1>
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
                  <p>
                    source: {message.source}
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
      <div className="bg-white border-t border-gray-200 p-4">
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
        {showSuggestions && (
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
  );
};

export default ChatComponent;