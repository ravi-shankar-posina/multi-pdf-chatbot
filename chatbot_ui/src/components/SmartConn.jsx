import axios from "axios";
import { useEffect, useRef, useState } from "react";
import { FaPaperPlane, FaRobot } from "react-icons/fa";
import { FaUserLarge } from "react-icons/fa6";

const SmartConn = () => {
  const [chatHistory, setChatHistory] = useState([]);
  const [query, setQuery] = useState("");
  const [loading, setLoading] = useState(false);
  const chatContainerRef = useRef(null);

  // Auto-scroll to the latest message
  useEffect(() => {
    if (chatContainerRef.current) {
      chatContainerRef.current.scrollTop = chatContainerRef.current.scrollHeight;
    }
  }, [chatHistory]);

  const handleSend = async () => {
    if (!query.trim()) return;

    const newMessage = { role: "user", content: query };
    setChatHistory((prev) => [...prev, newMessage]);
    setQuery("");
    setLoading(true);

    try {
      const { data } = await axios.post(`${import.meta.env.VITE_API_URL}/chat`, {
        chat_history: chatHistory,
        query,
      });

      setChatHistory([...data.chat_history]);
    } catch (error) {
      console.error("Error fetching response:", error);
    } finally {
      setLoading(false);
    }
  };

  // Format message content with basic code block handling
  const formatMessageContent = (content) => {
    if (!content) return null;

    // Split by code blocks (marked by triple backticks)
    const parts = content.split(/(```[\s\S]*?```)/g);

    return parts.map((part, index) => {
      // Check if this part is a code block
      if (part.startsWith("```") && part.endsWith("```")) {
        // Extract language if specified
        const firstLineEnd = part.indexOf("\n");
        const firstLine = part.substring(3, firstLineEnd).trim();
        const codeContent = part.substring(firstLineEnd + 1, part.length - 3);
        
        return (
          <div key={index} className="code-block my-2">
            {firstLine && <div className="code-language px-3 py-1 bg-gray-700 text-gray-200 text-xs font-mono rounded-t-md">{firstLine}</div>}
            <pre className={`${firstLine ? 'rounded-b-md rounded-t-none' : 'rounded-md'} bg-gray-800 p-3 overflow-x-auto text-gray-100 font-mono text-sm`}>
              {codeContent}
            </pre>
          </div>
        );
      } 
      
      // Format regular text with basic Markdown-like formatting
      const formattedText = formatSimpleMarkdown(part);
      
      return <div key={index} className="markdown-text">{formattedText}</div>;
    });
  };

  // Simple function to handle basic markdown formatting
  const formatSimpleMarkdown = (text) => {
    if (!text) return null;
    
    // Convert line breaks to <br/> tags
    const withLineBreaks = text.split('\n').map((line, i) => (
      <span key={i}>
        {line}
        {i < text.split('\n').length - 1 && <br />}
      </span>
    ));

    return withLineBreaks;
  };

  return (
    <div className="flex flex-col h-full bg-gradient-to-br from-indigo-50 to-blue-50 text-gray-800">
      {/* Chat messages container */}
      <div 
        ref={chatContainerRef} 
        className="flex-1 overflow-y-auto p-4 space-y-4"
      >
        {chatHistory.length === 0 && (
          <div className="flex flex-col items-center justify-center h-full text-center p-6">
            <div className="text-gray-500 mb-4 text-6xl">
              <FaRobot />
            </div>
            <h2 className="text-2xl font-bold text-gray-600 mb-2">Welcome to SmartChat</h2>
            <p className="text-gray-500 max-w-md">Ask anything and get intelligent responses instantly.</p>
          </div>
        )}

        {chatHistory.map((msg, index) => (
          <div
            key={index}
            className={`flex items-start gap-2 ${msg.role === "user" ? "justify-end" : "justify-start"}`}
          >
            {msg.role !== "user" && (
              <div className="bg-indigo-600 text-white p-2 rounded-full h-8 w-8 flex items-center justify-center mt-1">
                <FaRobot className="text-xs" />
              </div>
            )}
            
            <div
              className={`max-w-[80%] p-4 rounded-2xl shadow-sm ${
                msg.role === "user"
                  ? "bg-indigo-600 text-white rounded-br-none"
                  : "bg-white text-gray-800 rounded-bl-none border border-indigo-100"
              }`}
            >
              {msg.role === "user" ? (
                msg.content
              ) : (
                formatMessageContent(msg.content)
              )}
            </div>
            
            {msg.role === "user" && (
              <div className="bg-indigo-100 text-indigo-600 p-2 rounded-full h-8 w-8 flex items-center justify-center mt-1">
                <FaUserLarge className="text-xs" />
              </div>
            )}
          </div>
        ))}
        
        {loading && (
          <div className="flex justify-start items-start gap-2">
            <div className="bg-indigo-600 text-white p-2 rounded-full h-8 w-8 flex items-center justify-center">
              <FaRobot className="text-xs" />
            </div>
            <div className="bg-white p-4 rounded-2xl rounded-bl-none shadow-sm border border-indigo-100">
              <div className="flex space-x-1">
                <div className="h-2 w-2 bg-indigo-400 rounded-full animate-bounce" style={{ animationDelay: "0ms" }}></div>
                <div className="h-2 w-2 bg-indigo-400 rounded-full animate-bounce" style={{ animationDelay: "150ms" }}></div>
                <div className="h-2 w-2 bg-indigo-400 rounded-full animate-bounce" style={{ animationDelay: "300ms" }}></div>
              </div>
            </div>
          </div>
        )}
      </div>

      {/* Chat input */}
      <div className="p-4 bg-white shadow-lg border-t border-indigo-100">
        <div className="max-w-4xl mx-auto flex items-center gap-2 bg-white rounded-full shadow-sm border border-indigo-100 p-1 pl-4">
          <input
            type="text"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            className="flex-1 p-2 focus:outline-none bg-transparent"
            placeholder="Type your message..."
            onKeyDown={(e) => e.key === "Enter" && handleSend()}
          />
          <button
            onClick={handleSend}
            disabled={!query.trim() || loading}
            className={`p-3 rounded-full text-white transition-all ${
              query.trim() && !loading
                ? "bg-gray-600 hover:bg-gray-700"
                : "bg-gray-300 cursor-not-allowed"
            }`}
          >
            <FaPaperPlane />
          </button>
        </div>
      </div>
    </div>
  );
};

export default SmartConn;