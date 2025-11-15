import axios from "axios";
import { useEffect, useRef, useState } from "react";
import { FaPaperPlane, FaRobot, FaPlus, FaMicrophone } from "react-icons/fa";
import { FaUserLarge } from "react-icons/fa6";

const SmartConn = () => {
  const [chatHistory, setChatHistory] = useState([]);
  const [query, setQuery] = useState("");
  const [loading, setLoading] = useState(false);
  const [loadingMessage, setLoadingMessage] = useState("Thinking...");
  const [shouldScrollToBottom, setShouldScrollToBottom] = useState(false);
  const chatContainerRef = useRef(null);
  const messagesEndRef = useRef(null);

  // Progressive loading messages
  useEffect(() => {
    if (!loading) {
      setLoadingMessage("Thinking...");
      return;
    }

    const messages = [
      "Thinking...",
      "Processing your request...",
      "Almost ready...",
      "Finalizing response..."
    ];

    let index = 0;
    const interval = setInterval(() => {
      index = (index + 1) % messages.length;
      setLoadingMessage(messages[index]);
    }, 2000); // Change message every 2 seconds

    return () => clearInterval(interval);
  }, [loading]);

  // Scroll to bottom only when user sends a message
  useEffect(() => {
    if (shouldScrollToBottom && messagesEndRef.current) {
      messagesEndRef.current.scrollIntoView({ behavior: "smooth" });
      setShouldScrollToBottom(false);
    }
  }, [chatHistory, shouldScrollToBottom]);

  const handleSend = async () => {
    if (!query.trim()) return;

    const newMessage = { role: "user", content: query };
    setChatHistory((prev) => [...prev, newMessage]);
    setQuery("");
    setLoading(true);
    setShouldScrollToBottom(true); // Scroll when user sends message

    try {
      const { data } = await axios.post(
        `${import.meta.env.VITE_API_URL}/chat`,
        {
          chat_history: chatHistory,
          query,
        }
      );

      setChatHistory([...data.chat_history]);
      // Don't auto-scroll after receiving response - user can read from top
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
            {firstLine && (
              <div className="code-language px-3 py-1 bg-gray-200 text-gray-700 text-xs font-mono rounded-t-md">
                {firstLine}
              </div>
            )}
            <pre
              className={`${
                firstLine ? "rounded-b-md rounded-t-none" : "rounded-md"
              } bg-gray-100 p-3 overflow-x-auto text-gray-800 font-mono text-sm border border-gray-300`}
            >
              {codeContent}
            </pre>
          </div>
        );
      }

      // Format regular text with basic Markdown-like formatting
      const formattedText = formatSimpleMarkdown(part);

      return (
        <div key={index} className="markdown-text">
          {formattedText}
        </div>
      );
    });
  };

  // Simple function to handle basic markdown formatting
  const formatSimpleMarkdown = (text) => {
    if (!text) return null;

    // Convert line breaks to <br/> tags
    const withLineBreaks = text.split("\n").map((line, i) => (
      <span key={i}>
        {line}
        {i < text.split("\n").length - 1 && <br />}
      </span>
    ));

    return withLineBreaks;
  };

  return (
    <div className="flex flex-col h-full bg-white text-gray-900">
      {/* Chat messages container */}
      <div
        ref={chatContainerRef}
        className="flex-1 overflow-y-auto p-4 space-y-4"
      >
        {chatHistory.length === 0 && (
          <div className="flex flex-col items-center justify-center h-full text-center p-6">
            <div className="text-gray-400 mb-4 text-6xl">
              <FaRobot />
            </div>
            <h2 className="text-2xl font-bold text-gray-800 mb-2">
              Welcome to SmartChat
            </h2>
            <p className="text-gray-600 max-w-md">
              Ask anything and get intelligent responses instantly.
            </p>
          </div>
        )}

        {chatHistory.map((msg, index) => (
          <div
            key={index}
            className={`flex items-start gap-3 ${
              msg.role === "user" ? "justify-end" : "justify-start"
            }`}
          >
            {msg.role !== "user" && (
              <div className="bg-gray-800 text-white p-2 rounded-full h-9 w-9 flex items-center justify-center mt-1 flex-shrink-0">
                <FaRobot className="text-sm" />
              </div>
            )}

            <div
              className={`max-w-[80%] p-4 rounded-lg ${
                msg.role === "user"
                  ? "bg-gray-100 text-gray-900 border border-gray-200"
                  : "bg-white text-gray-900 border border-gray-200"
              }`}
            >
              {msg.role === "user"
                ? msg.content
                : formatMessageContent(msg.content)}
            </div>

            {msg.role === "user" && (
              <div className="bg-gray-200 text-gray-800 p-2 rounded-full h-9 w-9 flex items-center justify-center mt-1 flex-shrink-0">
                <FaUserLarge className="text-sm" />
              </div>
            )}
          </div>
        ))}

        {loading && (
          <div className="flex justify-start items-start gap-3">
            <div className="bg-gray-800 text-white p-2 rounded-full h-9 w-9 flex items-center justify-center flex-shrink-0">
              <FaRobot className="text-sm" />
            </div>
            <div className="bg-white p-4 rounded-lg border border-gray-200">
              <div className="flex items-center gap-2 text-gray-600">
                <span className="text-sm italic animate-pulse">
                  {loadingMessage}
                </span>
              </div>
            </div>
          </div>
        )}

        <div ref={messagesEndRef} />
      </div>

      {/* Chat input */}
      <div className="p-4 bg-white border-t border-gray-200">
        <div className="max-w-4xl mx-auto">
          <div className="relative flex items-end gap-2 bg-gray-50 rounded-2xl border border-gray-300 p-3 focus-within:border-gray-400 focus-within:shadow-sm transition-all">
            {/* Plus icon for attachments - bottom left */}
            <button
              className="mb-1 p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
              title="Add attachment"
              type="button"
            >
              <FaPlus className="text-base" />
            </button>

            {/* Text input */}
            <textarea
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              className="flex-1 px-2 py-2 focus:outline-none bg-transparent resize-none max-h-32 min-h-[40px] text-gray-900 placeholder-gray-500"
              placeholder="Message SmartChat..."
              rows="1"
              onKeyDown={(e) => {
                if (e.key === "Enter" && !e.shiftKey) {
                  e.preventDefault();
                  handleSend();
                }
              }}
              style={{
                height: "auto",
                overflowY: query.split("\n").length > 3 ? "auto" : "hidden",
              }}
              onInput={(e) => {
                e.target.style.height = "auto";
                e.target.style.height =
                  Math.min(e.target.scrollHeight, 128) + "px";
              }}
            />

            {/* Right side icons container */}
            <div className="flex items-center gap-1 mb-1">
              {/* Voice icon */}
              <button
                className="p-2 text-gray-600 hover:text-gray-900 hover:bg-gray-200 rounded-full transition-all flex-shrink-0"
                title="Voice input"
                type="button"
              >
                <FaMicrophone className="text-base" />
              </button>

              {/* Send button */}
              <button
                onClick={handleSend}
                disabled={!query.trim() || loading}
                className={`p-2.5 rounded-full transition-all flex-shrink-0 ${
                  query.trim() && !loading
                    ? "bg-gray-900 hover:bg-gray-700 text-white"
                    : "bg-gray-300 text-gray-500 cursor-not-allowed"
                }`}
                title="Send message"
              >
                <FaPaperPlane className="text-sm" />
              </button>
            </div>
          </div>
          <div className="text-xs text-gray-500 mt-2 text-center">
            Press Enter to send, Shift+Enter for new line
          </div>
        </div>
      </div>
    </div>
  );
};

export default SmartConn;