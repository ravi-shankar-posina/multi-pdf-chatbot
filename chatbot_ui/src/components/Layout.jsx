// Layout.jsx
import React from 'react';
import { Link, useLocation } from 'react-router-dom';
import { FaHeadset, FaFilePdf, FaDatabase } from "react-icons/fa";
import chatbotIntro from "../assets/ai.png";

const Layout = ({ children }) => {
  const location = useLocation();

  const isActive = (path) => {
    return location.pathname === path ? "bg-gray-300" : "";
  };

  return (
    <div className="flex h-screen bg-white">
      <div className="w-56 bg-gray-100">
        <img src={chatbotIntro} alt="SAP Logo" className="h-34 mr-2" />
        <div className="text-black p-10 hidden md:block">
          <ul className="space-y-4">
            <li>
              <Link to="/" className={`flex items-center text-sm font-bold cursor-pointer p-2 rounded-lg transition duration-300 ${isActive('/support-help')} hover:bg-gray-200`}>
                <FaHeadset className="mr-2" />
                Support Help
              </Link>
            </li>
            <li>
              <Link to="/pdf-reader" className={`flex items-center text-sm font-bold cursor-pointer p-2 rounded-lg transition duration-300 ${isActive('/pdf-reader')} hover:bg-gray-200`}>
                <FaFilePdf className="mr-2" />
                Pdf Reader
              </Link>
            </li>
            <li>
              <Link to="/abap-code-generator" className={`flex items-center text-sm font-bold cursor-pointer p-2 rounded-lg transition duration-300 ${isActive('/abap-code-generator')} hover:bg-gray-200`}>
                <FaDatabase className="mr-2" />
                Abap Code Generator
              </Link>
            </li>
          </ul>
        </div>
      </div>
      <div className="flex-1">
        {children}
      </div>
    </div>
  );
};

export default Layout;