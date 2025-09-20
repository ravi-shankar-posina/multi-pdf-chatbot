import React, { useState } from "react";
import logo from "../assets/image.png";

export const AccessManagement = () => {
  const validData = {
    email: "703055690@genpact.com",
    empId: "703055690",
  };

  const [formData, setFormData] = useState({
    email: "",
    empId: "",
  });

  const [currentStep, setCurrentStep] = useState(-1);
  const [isSubmitted, setIsSubmitted] = useState(false);
  const [error, setError] = useState("");
  const [selectedOption, setSelectedOption] = useState("");
  const [messages, setMessages] = useState([
    {
      text: "Hello! I am here to help you choose the option",
      sender: "bot",
    },
  ]);
  const [loading, setLoading] = useState(false);

  const questions = [
    {
      label: "Employee ID",
      name: "empId",
      placeholder: "Enter your employee ID",
      type: "text",
    },
    {
      label: "Email Address",
      name: "email",
      placeholder: "Enter your email address",
      type: "email",
    },
  ];

  const generateTicketNumber = () => {
    return Math.floor(10000000 + Math.random() * 90000000).toString();
  };

  const handleOptionClick = (option) => {
    setSelectedOption(option);
    setMessages((prevMessages) => [
      ...prevMessages,
      { text: option, sender: "user" },
      { text: "Please enter your employee ID.", sender: "bot" },
    ]);
    setCurrentStep(0);
  };

  const handleChange = (e) => {
    setFormData({
      ...formData,
      [e.target.name]: e.target.value,
    });
    setError("");
  };

  const handleSubmit = () => {
    const currentQuestion = questions[currentStep];
    const input = formData[currentQuestion.name];

    if (input !== validData[currentQuestion.name]) {
      setError(
        `Invalid ${currentQuestion.label.toLowerCase()}. Please try again.`
      );
      setMessages((prevMessages) => [
        ...prevMessages,
        {
          text: `Invalid ${currentQuestion.label.toLowerCase()}. Please try again.`,
          sender: "bot",
        },
      ]);
    } else {
      setMessages((prevMessages) => [
        ...prevMessages,
        { text: input, sender: "user" },
      ]);
      setLoading(true);

      setTimeout(() => {
        setLoading(false);

        if (currentStep < questions.length - 1) {
          setCurrentStep(currentStep + 1);
          setMessages((prevMessages) => [
            ...prevMessages,
            {
              text: `Please enter your ${questions[
                currentStep + 1
              ].label.toLowerCase()}.`,
              sender: "bot",
            },
          ]);
        } else {
          const ticketNumber = generateTicketNumber();
          setMessages((prevMessages) => [
            ...prevMessages,
            {
              text: `Success! ${selectedOption} processed successfully and forwarded to your registered email.
                Ticket created for further processing with INC${ticketNumber}.`,
              sender: "bot",
            },
          ]);
          setIsSubmitted(true);
        }
      }, 1500);
    }
  };

  const handleKeyDown = (e) => {
    if (e.key === "Enter") {
      e.preventDefault();
      if (!loading) handleSubmit();
      setFormData({
        email: "",
        empId: "",
      });
    }
  };

  const messagesEndRef = React.useRef(null);

  React.useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [messages]);

  return (
    <div className="flex flex-col h-full bg-white overflow-hidden">
      <div className="flex-1 flex flex-col">
        {/* Header Section - Only show when no option is selected */}
        {currentStep === -1 && (
          <div className="flex flex-col items-center justify-center p-6 sm:p-8 bg-gray-50 border-b border-gray-200">
            <div className="flex items-center mb-4">
              <img 
                src={logo} 
                alt="Logo" 
                className="w-10 h-10 sm:w-12 sm:h-12 mr-3" 
              />
              <div className="flex items-center">
                <h1 className="text-xl sm:text-2xl lg:text-3xl font-bold text-gray-800">
                  Password Agent
                </h1>
                <span className="ml-2 bg-blue-500 text-blue-300 text-base sm:text-lg font-semibold px-2 py-1 rounded-full">
                  +
                </span>
              </div>
            </div>
            <p className="text-gray-600 text-sm sm:text-base text-center">
              Get help with password-related issues
            </p>
          </div>
        )}

        {/* Messages Area */}
        <div className="flex-1 overflow-y-auto p-4 sm:p-6 space-y-4 bg-white">
          <div className="max-w-4xl mx-auto space-y-4">
            {messages.map((msg, index) => (
              <div
                key={index}
                className={`flex ${msg.sender === "bot" ? "justify-start" : "justify-end"}`}
              >
                <div
                  className={`${
                    msg.sender === "bot"
                      ? "bg-gray-100 text-gray-900 border border-gray-200"
                      : "bg-black text-white"
                  } p-3 sm:p-4 rounded-2xl max-w-[85%] sm:max-w-[75%] lg:max-w-2xl shadow-sm`}
                >
                  <div className="text-sm sm:text-base whitespace-pre-wrap">
                    {msg.text}
                  </div>
                </div>
              </div>
            ))}
            
            {loading && (
              <div className="flex justify-start">
                <div className="bg-gray-100 border border-gray-200 p-3 sm:p-4 rounded-2xl max-w-xs animate-pulse">
                  <div className="flex items-center space-x-2">
                    <div className="flex space-x-1">
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce"></div>
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '0.1s' }}></div>
                      <div className="w-2 h-2 bg-gray-400 rounded-full animate-bounce" style={{ animationDelay: '0.2s' }}></div>
                    </div>
                    <span className="text-gray-600 text-sm">Typing...</span>
                  </div>
                </div>
              </div>
            )}
            <div ref={messagesEndRef} />
          </div>
        </div>

        {/* Input/Options Area */}
        <div className="border-t border-gray-200 bg-gray-50">
          {currentStep === -1 ? (
            <div className="p-4 sm:p-6">
              <div className="max-w-4xl mx-auto">
                <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3 sm:gap-4">
                  {[
                    "Forget Password",
                    "Unable to login due to User locked",
                    "Reset Password",
                    "Password Expired",
                  ].map((option, index) => (
                    <button
                      key={index}
                      onClick={() => handleOptionClick(option)}
                      className="px-4 py-3 bg-black text-white rounded-xl shadow-sm hover:bg-gray-800 transition-all duration-200 transform hover:scale-[1.02] active:scale-[0.98] text-sm sm:text-base font-medium"
                    >
                      {option}
                    </button>
                  ))}
                </div>
              </div>
            </div>
          ) : (
            <div className="p-4 sm:p-6">
              <div className="max-w-4xl mx-auto">
                <form className="flex flex-col space-y-3">
                  <div className="flex flex-col sm:flex-row gap-3">
                    <input
                      type={questions[currentStep].type}
                      id={questions[currentStep].name}
                      name={questions[currentStep].name}
                      required
                      value={formData[questions[currentStep].name]}
                      onChange={handleChange}
                      onKeyDown={handleKeyDown}
                      className="flex-1 px-4 py-3 border-2 border-gray-300 rounded-xl focus:ring-2 focus:ring-black focus:border-transparent transition-all duration-200 text-gray-900 placeholder-gray-500 hover:border-gray-400"
                      placeholder={questions[currentStep].placeholder}
                    />
                    <button
                      type="button"
                      onClick={handleSubmit}
                      disabled={loading || !formData[questions[currentStep].name].trim()}
                      className={`px-6 py-3 rounded-xl font-semibold transition-all duration-200 min-w-[100px] ${
                        loading || !formData[questions[currentStep].name].trim()
                          ? "bg-gray-400 cursor-not-allowed text-white"
                          : "bg-black text-white hover:bg-gray-800 transform hover:scale-[1.02] active:scale-[0.98]"
                      }`}
                    >
                      {loading ? (
                        <div className="flex items-center justify-center">
                          <div className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
                        </div>
                      ) : (
                        "Submit"
                      )}
                    </button>
                  </div>
                  
                  {error && (
                    <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-xl text-sm">
                      {error}
                    </div>
                  )}
                  
                  <div className="text-xs sm:text-sm text-gray-500 text-center">
                    Press Enter to submit or click the Submit button
                  </div>
                </form>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default AccessManagement;