import React, { useState } from 'react';

export const Accessmanagement = () => {
  const [formData, setFormData] = useState({
    name: '',
    empId: '',
    email: '',
    phone: ''
  });
  
  const [isSubmitted, setIsSubmitted] = useState(false);
  
  const handleChange = (e) => {
    setFormData({
      ...formData,
      [e.target.name]: e.target.value
    });
  };
  
  const handleSubmit = (e) => {
    e.preventDefault();
    setIsSubmitted(true);
  };
  
  if (isSubmitted) {
    return (
      <div className="flex items-center justify-center min-h-screen bg-gray-50">
        <div className="bg-white p-8 rounded-lg shadow-lg text-center max-w-md w-full m-4">
          <div className="text-green-600 text-3xl mb-4">✓</div>
          <h2 className="text-2xl font-bold text-gray-800 mb-2">Details shared with your Email ID Successfully </h2>
          {/* <p className="text-gray-600">Thank you for registering with us.</p> */}
        </div>
      </div>
    );
  }

  return (
    <div className="flex items-center justify-center min-h-screen max-h-screen bg-gray-50">
      <div className="bg-white p-6 rounded-lg shadow-lg max-w-md w-full m-2">
        <h2 className="text-2xl font-bold text-gray-800 mb-6 text-center">
          Access Management Registration
        </h2>
        
        <form onSubmit={handleSubmit} className="space-y-6">
          <div>
            <label 
              htmlFor="name" 
              className="block text-sm font-medium text-gray-700 mb-1"
            >
              Full Name
            </label>
            <input
              type="text"
              id="name"
              name="name"
              required
              value={formData.name}
              onChange={handleChange}
              className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-green-500 focus:border-green-500"
              placeholder="Enter your full name"
            />
          </div>
          <div>
            <label 
              htmlFor="name" 
              className="block text-sm font-medium text-gray-700 mb-1"
            >
              Emplooyee Id
            </label>
            <input
              type="text"
              id="name"
              name="name"
              required
              value={formData.empId}
              onChange={handleChange}
              className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-green-500 focus:border-green-500"
              placeholder="Enter your employee id"
            />
          </div>

          <div>
            <label 
              htmlFor="email" 
              className="block text-sm font-medium text-gray-700 mb-1"
            >
              Email Address
            </label>
            <input
              type="email"
              id="email"
              name="email"
              required
              value={formData.email}
              onChange={handleChange}
              className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-green-500 focus:border-green-500"
              placeholder="Enter your email address"
            />
          </div>

          <div>
            <label 
              htmlFor="phone" 
              className="block text-sm font-medium text-gray-700 mb-1"
            >
              Phone Number
            </label>
            <input
              type="tel"
              id="phone"
              name="phone"
              required
              value={formData.phone}
              onChange={handleChange}
              className="w-full px-4 py-2 border border-gray-300 rounded-md focus:ring-2 focus:ring-green-500 focus:border-green-500"
              placeholder="Enter your phone number"
            />
          </div>

          <button
            type="submit"
            className="w-full bg-green-600 text-white py-2 px-4 rounded-md hover:bg-green-700 transition duration-300 focus:outline-none focus:ring-2 focus:ring-green-500 focus:ring-offset-2"
          >
            Register
          </button>
        </form>
      </div>
    </div>
  );
};

export default Accessmanagement;