import React, { useState } from "react";
import axios from "axios";
import ReactMarkdown from "react-markdown";
import { FaSpinner, FaPaperPlane } from "react-icons/fa";

const SapTestCase = () => {
  const [file, setFile] = useState(null);
  const [query, setQuery] = useState("");
  const [messages, setMessages] = useState([]);
  const [fileUploadLoading, setFileUploadLoading] = useState(false);
  const [queryLoading, setQueryLoading] = useState(false);
  const [error, setError] = useState("");
  const [fileProcessed, setFileProcessed] = useState(false);
  const [uploadMessage, setUploadMessage] = useState("");

  const messagesEndRef = React.useRef(null);
  const fileInputRef = React.useRef(null);

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  };

  React.useEffect(() => {
    scrollToBottom();
  }, [messages]);

  const handleFileChange = (e) => {
    const selectedFile = e.target.files[0];
    if (selectedFile) {
      setFile(selectedFile);
      setFileProcessed(false);
      setMessages([]);
      setError("");
      setUploadMessage("");
    }
  };

  const handleFileUpload = async () => {
    if (!file) {
      setError("Please upload a file first.");
      return;
    }
    setFileUploadLoading(true);
    setError("");
    setUploadMessage("");

    const formData = new FormData();
    formData.append("files", file);

    try {
      const res = await axios.post(
        "https://sapchatapi.rspos.dev/upload",
        formData,
        {
          headers: {
            "Content-Type": "multipart/form-data",
          },
        }
      );
      setUploadMessage(res.data.message);
      setFileProcessed(true);
      setMessages([{ type: "system", content: res.data.message }]);
    } catch (err) {
      setError(
        err.response ? err.response.data.error : "Error uploading file."
      );
    }

    setFileUploadLoading(false);
  };

  const handleQuerySubmit = async (e) => {
    setQuery("");
    e.preventDefault();
    if (!fileProcessed) {
      setError("Please upload and process a file first.");
      return;
    }

    if (!query.trim()) {
      setError("Please enter a query.");
      return;
    }

    setQueryLoading(true);
    setError("");
    setMessages((prev) => [...prev, { type: "user", content: query }]);

    try {
      const res = await axios.post(
        "https://sapchatapi.rspos.dev/query",
        { query },
        {
          headers: {
            "Content-Type": "application/json",
          },
        }
      );
      setMessages((prev) => [
        ...prev,
        { type: "bot", content: res.data.test_cases.join("\n") },
      ]);
    } catch (err) {
      setError(
        err.response ? err.response.data.error : "Error generating test cases."
      );
    }

    setQueryLoading(false);
  };

  return (
    <div className={`h-full flex flex-col ${!fileProcessed ? 'items-center justify-center' : ''} bg-gray-50`}>
      <div 
        className={`${
          fileProcessed 
            ? 'w-full p-4' 
            : 'w-auto p-8'
        } transition-all duration-300`}
      >
        <div className={`rounded-lg shadow-md bg-white ${fileProcessed ? 'p-4' : 'p-8'}`}>
          <div className="flex flex-col items-center space-y-4">
            <div className="flex justify-center  items-center w-1/2">
              <div className="relative flex-grow p-2">
                <input  
                  type="file"
                  ref={fileInputRef}
                  accept=".pdf,.txt,.docx"
                  onChange={handleFileChange}
                  className="absolute inset-0 w-full h-full opacity-0 cursor-pointer"
                />
                <div className="p-3 border border-gray-300 bg-gray-100 rounded-lg shadow-sm text-gray-700 truncate min-w-[200px]">
                  {file ? file.name : "Choose a file..."}
                </div>
              </div>
              <button
                onClick={handleFileUpload}
                disabled={fileUploadLoading || !file}
                className={`flex items-center justify-center px-6 py-3 rounded-lg text-white transition-all duration-200 w-full sm:w-auto ${
                  fileUploadLoading || !file
                    ? "bg-gray-400 cursor-not-allowed"
                    : "bg-blue-600 hover:bg-blue-700"
                }`}
              >
                {fileUploadLoading ? (
                  <FaSpinner className="animate-spin mr-2" />
                ) : (
                  "Upload"
                )}
              </button>
            </div>
            {error && <p className="text-red-500 text-sm">{error}</p>}
          </div>
        </div>
      </div>

      {fileProcessed && (
        <div className="flex-1 flex flex-col p-4">
          <div className="flex-1 flex flex-col bg-gray-100 rounded-lg shadow-md overflow-hidden">
            <div className="flex-1 overflow-y-auto p-4" style={{ maxHeight: '60vh' }}>
              {messages.map((message, index) => (
                <div
                  key={index}
                  className={`mb-4 ${message.type === "user" ? "text-right" : "text-left"}`}
                >
                  <div
                    className={`inline-block p-3 rounded-lg shadow-md ${
                      message.type === "user"
                        ? "bg-blue-500 text-white"
                        : "bg-gray-200 text-black"
                    }`}
                  >
                    {message.type === "system" ? (
                      <p className="text-sm italic">{message.content}</p>
                    ) : (
                      <ReactMarkdown>{message.content}</ReactMarkdown>
                    )}
                  </div>
                </div>
              ))}
              <div ref={messagesEndRef} />
            </div>
            <div className="bg-white border-t border-gray-300">
              <form onSubmit={handleQuerySubmit} className="p-4">
                <div className="flex items-center">
                  <input
                    type="text"
                    placeholder="Type your query here..."
                    value={query}
                    onChange={(e) => setQuery(e.target.value)}
                    className="flex-grow p-2 rounded-l-lg bg-gray-50 border border-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-500"
                  />
                  <button
                    type="submit"
                    disabled={queryLoading}
                    className={`flex items-center justify-center p-2 rounded-r-lg ${
                      queryLoading
                        ? "bg-gray-400 cursor-not-allowed"
                        : "bg-blue-600 hover:bg-blue-700 transition"
                    } text-white`}
                  >
                    {queryLoading ? (
                      <FaSpinner className="animate-spin" />
                    ) : (
                      <FaPaperPlane />
                    )}
                  </button>
                </div>
                {error && <p className="text-red-500 mt-2">{error}</p>}
              </form>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default SapTestCase;
