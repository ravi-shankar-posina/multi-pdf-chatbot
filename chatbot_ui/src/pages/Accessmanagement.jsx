import React, { useState } from "react";

export const AccessManagement = () => {
  // Static values for validation
  const validData = {
    email: "703055690@genpact.com",
    name: "chittya Ranjan Bej",
    empId: "703055690",
    phone: "9000933795",
  };

  const [formData, setFormData] = useState({
    email: "",
    name: "",
    empId: "",
    phone: "",
  });

  const [currentStep, setCurrentStep] = useState(0);
  const [isSubmitted, setIsSubmitted] = useState(false);
  const [error, setError] = useState("");
  const [messages, setMessages] = useState([
    {
      text: "Hello! Let's get you registered. Please enter your email address.",
      sender: "bot",
    },
  ]);

  const questions = [
    {
      label: "Email Address",
      name: "email",
      placeholder: "Enter your email address",
      type: "email",
    },
    {
      label: "Full Name",
      name: "name",
      placeholder: "Enter your full name",
      type: "text",
    },
    {
      label: "Employee ID",
      name: "empId",
      placeholder: "Enter your employee ID",
      type: "text",
    },
    {
      label: "Phone Number",
      name: "phone",
      placeholder: "Enter your phone number",
      type: "tel",
    },
  ];

  const handleChange = (e) => {
    setFormData({
      ...formData,
      [e.target.name]: e.target.value,
    });
    setError(""); // Clear error on input change
  };

  const handleSubmit = () => {
    const currentQuestion = questions[currentStep];
    const input = formData[currentQuestion.name];

    // Validate current input
    if (input !== validData[currentQuestion.name]) {
      setError(
        `Invalid ${currentQuestion.label.toLowerCase()}. Please try again.`
      );
      setMessages([
        ...messages,
        {
          text: `Invalid ${currentQuestion.label.toLowerCase()}. Please try again.`,
          sender: "bot",
        },
      ]);
    } else {
      setMessages([...messages, { text: input, sender: "user" }]);

      if (currentStep < questions.length - 1) {
        // Move to the next step and ask the next question
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
        // All inputs are valid, show success message
        setIsSubmitted(true);
        setMessages((prevMessages) => [
          ...prevMessages,
          {
            text: "Details shared with your Email ID Successfully",
            sender: "bot",
          },
        ]);
      }
    }
  };

  const handleKeyDown = (e) => {
    if (e.key === "Enter") {
      e.preventDefault();
      handleSubmit();
    }
  };

  if (isSubmitted) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-50">
        <div className="bg-white p-8 rounded-lg shadow-lg text-center max-w-md w-full m-4">
          <div className="text-green-600 text-3xl mb-4">✓</div>
          <h2 className="text-2xl font-bold text-gray-800 mb-2">
            Details shared with your Email ID Successfully
          </h2>
        </div>
      </div>
    );
  }

  return (
    <div className="flex flex-col  min-h-screen bg-gray-50">
      <div className="bg-white p-6 rounded-lg shadow-lg h-screen  w-full m-2 space-y-4">
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
                    ? "bg-gray-200"
                    : "bg-green-600 text-white"
                } p-3 rounded-lg max-w-xs`}
              >
                {msg.text}
              </div>
            </div>
          ))}
        </div>

        <form className="mt-4 flex">
          <input
            type={questions[currentStep].type}
            id={questions[currentStep].name}
            name={questions[currentStep].name}
            required
            value={formData[questions[currentStep].name]}
            onChange={handleChange}
            onKeyDown={handleKeyDown}
            className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-green-500 focus:border-green-500 mt-2"
            placeholder={questions[currentStep].placeholder}
          />
          {error && <p className="text-red-500 text-sm mt-2">{error}</p>}
        </form>
      </div>
    </div>
  );
};

export default AccessManagement;
