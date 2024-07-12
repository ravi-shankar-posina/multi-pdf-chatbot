import React, { useState, useRef, useEffect } from "react";
import { FiSend, FiUser } from "react-icons/fi";
import { RiRobot2Line } from "react-icons/ri";
import { FaUserCircle } from "react-icons/fa";
import logo from "./assets/logo.jpeg";
import chatbotIntro from "./assets/ai.png";
import bg from "./assets/bg.avif";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import "./markdown.css";

const ChatInterface = () => {
  const [messages, setMessages] = useState([]);
  const [inputMessage, setInputMessage] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const messagesEndRef = useRef(null);

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  useEffect(scrollToBottom, [messages]);

  const handleSendMessage = async () => {
    if (inputMessage.trim() === "") return;

    setMessages([...messages, { text: inputMessage, sender: "user" }]);
    setInputMessage("");
    setIsLoading(true); // Set loading state to true when sending message

    try {
      const response = await fetch("http://localhost:8501/chat", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ prompt: inputMessage }),
      });

      if (!response.ok) {
        throw new Error("Network response was not ok");
      }

      const data = await response.json();
      console.log(data.sources[0].metadata.source)


      setMessages((prevMessages) => [
        ...prevMessages,
        { text: data.answer, sender: "ai", source:data.sources[0].metadata.source },
      ]);
    } catch (error) {
      console.error("Error fetching data:", error);
      setMessages((prevMessages) => [
        ...prevMessages,
        { text: "Error fetching response.", sender: "ai" },
      ]);
    } finally {
      setIsLoading(false); // Set loading state to false once response is received
    }
  };

  const quotes = [
    "Tell me about SAP sales order.",
    "Innovate with SAP technology.",
    "Empower your enterprise with SAP solutions.",
    "Transform your business with intelligent ERP.",
    "SAP: Driving digital transformation.",
  ];

  return (
    <div
      className="flex h-screen bg-gray-100"
      style={{
        backgroundImage: `url(${bg})`,
        backgroundSize: "cover",
        backgroundPosition: "center",
      }}
    >
      <div className="w-80 bg-white text-black p-6 hidden md:block">
        <div className=" ">
          <img src={logo} alt="SAP Logo" className="h-10 m-2" />
        </div>
        <img src={chatbotIntro} alt="SAP Logo" className="h-34 mr-2" />
        <h2 className="text-xl font-bold mb-4">Recommended Inputs</h2>
        <ul className="space-y-4">
          {quotes.map((quote, index) => (
            <li
              onClick={() => setInputMessage(quote)}
              key={index}
              className="text-md italic hover:not-italic cursor-pointer"
            >
              {quote}
            </li>
          ))}
        </ul>
      </div>
      <div className="flex-1 flex flex-col">
        <header className="bg-white p-4 flex justify-end">
          <div className=" text-pink-700 text-3xl mr-4 ">
            <FaUserCircle className="h-10 w-10" />
          </div>
        </header>
        <div className="flex-1 overflow-y-auto p-4">
          <div className="flex items-center mb-4">
            <img
              src={chatbotIntro}
              alt="AI Assistant"
              className="w-12 h-12 mr-3"
            />
            <div className="bg-white rounded-lg p-3 shadow-md">
              <p>Hello! I'm your TAIL assistant. How can I help you today?</p>
            </div>
          </div>
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
                      ? "bg-blue-500 text-white"
                      : "bg-white"
                  } rounded-lg p-3 shadow-md max-w-7xl lg:max-w-7xl`}
                >
                  {message.sender === "ai" && message.source && (
                    <p >
                      source :
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
              <div className="animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-purple-500"></div>
              <h1 className="ml-4">Loading...</h1>
            </div>
          )}
          <div ref={messagesEndRef} />
        </div>
        <div className="bg-white border-t border-gray-200 p-4">
          <div className="flex items-center">
            <input
              type="text"
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              placeholder="Type your message..."
              className="flex-1 border border-gray-300 rounded-l-lg px-4 py-2 focus:outline-none focus:ring-2 focus:ring-pink-500 text-black"
              onKeyPress={(e) => e.key === "Enter" && handleSendMessage()}
            />
            <button
              onClick={handleSendMessage}
              disabled={isLoading}
              className={`bg-pink-500 text-white rounded-r-lg px-4 py-2 ${
                isLoading
                  ? "opacity-50 cursor-not-allowed"
                  : "hover:bg-pink-900"
              } focus:outline-none focus:ring-2 focus:ring-pink-500`}
            >
              {isLoading ? (
                <div className="animate-spin rounded-full h-5 w-5 border-b-2 border-white"></div>
              ) : (
                <FiSend />
              )}
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ChatInterface;
