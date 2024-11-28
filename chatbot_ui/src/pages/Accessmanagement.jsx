import React, { useState } from "react";

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
      text: "Hello! How can I help you?",
      sender: "bot",
    },
  ]);
  const [loading, setLoading] = useState(false);

  const questions = [
    {
      label: "Email Address",
      name: "email",
      placeholder: "Enter your email address",
      type: "email",
    },
    {
      label: "Employee ID",
      name: "empId",
      placeholder: "Enter your employee ID",
      type: "text",
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
      { text: "Please enter your email address.", sender: "bot" },
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
              text: `Success! ${selectedOption} processed successfully and forwarded to your registered email. Ticket created for further processing with INC${ticketNumber}.`,
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
      })
    }
  };

  return (
    <div className="flex flex-col min-h-screen bg-gray-100 w-full">
      <div className="bg-white p-6 rounded-lg shadow-lg h-screen w-full mx-auto  space-y-4">
        <div className="space-y-4">
          {messages.map((msg, index) => (
            <div
              key={index}
              className={`flex ${
                msg.sender === "bot" ? "justify-start" : "justify-end"
              }`}
            >
              <div
                className={`${
                  msg.sender === "bot"
                    ? "bg-gray-300 text-black"
                    : "bg-blue-600 text-white"
                } p-3 rounded-lg max-w-xs shadow`}
              >
                {msg.text}
              </div>
            </div>
          ))}
          {loading && (
            <div className="flex justify-start">
              <div className="bg-gray-300 p-3 rounded-lg max-w-xs animate-pulse">
                Typing...
              </div>
            </div>
          )}
        </div>

        {currentStep === -1 ? (
          <div className="flex flex-wrap gap-3 justify-center mt-4">
            {[
              "Forget Password",
              "Unable to login due to User locked",
              "Reset Password",
              "Password Expired",
            ].map((option, index) => (
              <button
                key={index}
                onClick={() => handleOptionClick(option)}
                className="px-4 py-2 bg-blue-500 text-white rounded-lg shadow-md hover:bg-blue-600 transition duration-200"
              >
                {option}
              </button>
            ))}
          </div>
        ) : (
          <form className="mt-4 flex flex-col space-y-2">
            <input
              type={questions[currentStep].type}
              id={questions[currentStep].name}
              name={questions[currentStep].name}
              required
              value={formData[questions[currentStep].name]}
              onChange={handleChange}
              onKeyDown={handleKeyDown}
              className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition duration-200"
              placeholder={questions[currentStep].placeholder}
            />
            {error && <p className="text-red-500 text-sm">{error}</p>}
          </form>
        )}
      </div>
    </div>
  );
};

export default AccessManagement;
