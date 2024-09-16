import React, { useState } from "react";
import { FaCode, FaFilePdf, FaHeadset, FaUser } from "react-icons/fa";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import chatbotIntro from "./assets/ai.png";

const options = [
  { label: "How To?", api: "csv/query", icon: <FaHeadset /> },
  { label: "Incident", api: "csv/agent_query", icon: <FaCode /> },
  { label: "Best Practices", api: "pdf/query", icon: <FaFilePdf /> },
  { label: "ABAP Code Generator", api: "query", icon: <FaCode /> },  
];

const Chat = () => {
  const [inputMessage, setInputMessage] = useState("");
  const [currentQuestion, setCurrentQuestion] = useState(null);
  const [currentResponse, setCurrentResponse] = useState(null);
  const [selectedOption, setSelectedOption] = useState("csv/query");
  const [selectedLabel, setSelectedLabel] = useState("How To?");
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

  const handleOptionClick = (optionApi, optionLabel) => {
    setSelectedOption(optionApi);
    setSelectedLabel(optionLabel);
    setInputMessage("");
    setCurrentQuestion(null);
    setCurrentResponse(null);
    setShowInitialInput(true);
    setShowSatisfactionQuestion(false);
    setThankYouMessage("");
    setAdditionalResponse("");
    if (optionLabel === "ABAP Code Generator") {
      setSource("");
    }
  };

  const handleYesClick = () => {
    setThankYouMessage(
      "Thank you! If you have further questions, feel free to ask."
    );
    setShowSatisfactionQuestion(false);
  };

  const handleNoClick = async () => {
    setIsLoading(true);
    setShowSatisfactionQuestion(false);
    setThankYouMessage("");

    try {
      const response = await fetch(`${import.meta.env.VITE_API_URL}/query`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ query: currentQuestion }),
      });

      if (!response.ok) {
        throw new Error(`Network response was not ok: ${response.statusText}`);
      }

      const text = await response.text();
      const data = JSON.parse(text);
      setAdditionalResponse(data.answer || " ");
      setCurrentResponse(" ");
      setLinks([])
      setContent([])
    } catch (error) {
      console.error("Error fetching data:", error);
      setAdditionalResponse("Error fetching additional response.");
    } finally {
      setIsLoading(false);
      setShowSatisfactionQuestion(true); // Display the satisfaction question again after handling 'No'
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
              {additionalResponse && (
                <div className="mb-2 overflow-x-auto">
                  <h1 className="py-2 font-bold ">
                    Response genarated by LLM
                  </h1>
                  <ReactMarkdown
                    className="markdown-body"
                    remarkPlugins={[remarkGfm]}
                  >
                    {additionalResponse}
                  </ReactMarkdown>
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
              {images.length > 0 && (
                <div className="mb-2 flex flex-col">
                  {images.map((image, index) => (
                    <img
                      key={index}
                      src={image}
                      alt={`Fetched ${index}`}
                      className="max-w-full h-auto rounded-lg"
                    />
                  ))}
                </div>
              )}

              {content.length > 0 && (
                <div className="mt-4">
                  <h2 className="font-bold text-lg">Related Information:</h2>
                  <ul className="list-disc pl-5">
                    {content.map((item, index) => (
                      <li key={index}>{item.replace(/^prompt:\s*/, "")}</li>
                    ))}
                  </ul>
                </div>
              )}
              {links.length > 0 && (
                <div className="mt-4">
                  <h2 className="font-bold text-lg">Related Links:</h2>
                  <ul className="list-disc ml-5 mt-2">
                    {links.map((link, index) => (
                      <li key={index}>
                        <a
                          href={link}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-blue-500 underline"
                        >
                          {link}
                        </a>
                      </li>
                    ))}
                  </ul>
                </div>
              )}
              {/* {thankYouMessage && (
                <div className="mt-4 text-center text-green-600 font-semibold">
                  {thankYouMessage}
                </div>
              )}
              {showSatisfactionQuestion && !thankYouMessage && (
                <div className="flex  mt-4 space-x-4">
                  <h1 className="py-2 font-semibold">
                    Are You satisifies with this Response...?
                  </h1>
                  <button
                    onClick={handleYesClick}
                    className="bg-green-500 text-white px-4 py-2 rounded hover:bg-green-600"
                  >
                    Yes
                  </button>
                  <button
                    onClick={handleNoClick}
                    className="bg-red-500 text-white px-4 py-2 rounded hover:bg-red-600"
                  >
                    No
                  </button>
                </div>
              )} */}
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
