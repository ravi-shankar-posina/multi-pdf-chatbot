import React, { useEffect } from "react";
import { FaCode, FaFilePdf, FaHeadset, FaUser } from "react-icons/fa";
import { MdAccessAlarm } from "react-icons/md";
import chatbotIntro from "../assets/ai.png";
import { TbDeviceDesktopAnalytics } from "react-icons/tb";
import { Link, Outlet, useLocation } from "react-router-dom";

const COLORS = {
  primary: "#FFA500",
  secondary: "#00AECF",
  dark: "#073161",
};

const options = [
  { label: "How To?", path: "/", api: "csv/query", icon: <FaHeadset /> },
  {
    label: "Incident Query",
    path: "/incident-query",
    api: "analyze",
    icon: <FaCode />,
  },
  {
    label: "Incident Analysis",
    path: "/incident-analysis",
    icon: <TbDeviceDesktopAnalytics />,
  },
  {
    label: "Best Practices",
    path: "/best-practices",
    api: "pdf/query",
    icon: <FaFilePdf />,
  },
  {
    label: "ABAP Code Generator",
    path: "/abap-code-generator",
    api: "query",
    icon: <FaCode />,
  },
  {
    label: "Access Management",
    path: "/access-management",
    icon: <MdAccessAlarm />,
  },
  {
    label: "Test Script Generator",
    path: "/test-script-genarator",
    icon: <FaUser />,
  },
];

const Layout = ({ onLogout }) => {
  const [selectedLabel, setSelectedLabel] = React.useState(options[0].label);
  const location = useLocation();

  useEffect(() => {
    const currentItem = options.find((item) => item.path === location.pathname);
    setSelectedLabel(currentItem ? currentItem.label : "How To?");
  }, [location.pathname]);

  return (
    <div className="flex max-h-screen min-h-screen bg-white">
      <div
        className="w-80 min-h-screen h-full rounded-r-2xl shadow-lg"
        style={{
          backgroundColor: COLORS.dark,
        }}
      >
        <div
          className="flex justify-center p-6 border-b border-opacity-20"
          style={{ borderBottomColor: COLORS.secondary }}
        >
          <img
            src={chatbotIntro}
            alt="Chatbot Intro"
            className="h-24 w-24 rounded-full object-contain border-4 bg-white"
            style={{
              borderColor: COLORS.primary,
              boxShadow: `0 0 0 4px ${COLORS.secondary}`,
            }}
          />
        </div>
        <div className="text-white pl-4 pr-4 mt-4">
          <ul className="space-y-2">
            {options.map((option, index) => (
              <li key={index} className="flex items-center">
                <Link
                  to={option.path}
                  className={`flex items-center p-3 w-full space-x-3 rounded-lg transition duration-300 group ${
                    location.pathname === option.path
                      ? "bg-white text-[#073161]"
                      : "hover:bg-white/10"
                  }`}
                  style={{
                    backgroundColor:
                      location.pathname === option.path
                        ? COLORS.primary
                        : "transparent",
                  }}
                >
                  <span
                    className={`text-lg ${
                      location.pathname === option.path
                        ? "text-[#073161]"
                        : "text-white"
                    }`}
                  >
                    {option.icon}
                  </span>
                  <span
                    className={`font-medium ${
                      location.pathname === option.path
                        ? "text-[#073161]"
                        : "text-white"
                    }`}
                  >
                    {option.label}
                  </span>
                </Link>
              </li>
            ))}
          </ul>
        </div>
      </div>
      <div className="flex flex-col w-full max-h-screen min-h-screen overflow-hidden">
        <header
          className="border-b p-4 flex justify-between items-center"
          style={{
            borderBottomColor: COLORS.secondary,
          }}
        >
          <div className="text-xl font-bold" style={{ color: COLORS.dark }}>
            {selectedLabel}
          </div>
          <div className="flex space-x-4 justify-center items-center">
            <div className="text-sky-500 text-3xl">
              <FaUser />
            </div>
            <button
              className="px-4 py-2 rounded-full transition duration-300 hover:opacity-90"
              style={{
                backgroundColor: COLORS.secondary,
                color: "white",
              }}
              onClick={onLogout}
            >
              Logout
            </button>
          </div>
        </header>
        <main className="flex-1 overflow-auto">
          <Outlet />
        </main>
      </div>
    </div>
  );
};

export default Layout;
