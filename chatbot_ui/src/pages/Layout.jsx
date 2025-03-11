import React, { useEffect, useState } from "react";
import { FaCode, FaFilePdf, FaHeadset, FaUser, FaChevronDown, FaChevronUp, FaBook, FaTools } from "react-icons/fa";
import { MdSupportAgent, MdDeveloperMode, MdPassword } from "react-icons/md";
import { TbDeviceDesktopAnalytics } from "react-icons/tb";
import chatbotIntro from "../assets/logo.png";
import { Link, Outlet, useLocation } from "react-router-dom";

const COLORS = {
  primary: "#FFA500",
  secondary: "#00AECF",
  dark: "#073161",
  labelColor: "#FFC876",
};

const navigatorSections = [
  {
    title: "Support Navigator",
    icon: <MdSupportAgent size={16} />,
    titleColor: COLORS.labelColor,
    iconColor: COLORS.labelColor,
    options: [
      { label: "How To", path: "/", icon: <FaHeadset size={14} /> },
      { label: "Incident Query", path: "/incident-query", icon: <FaCode size={14} /> },
      { label: "Incident Analysis", path: "/incident-analysis", icon: <TbDeviceDesktopAnalytics size={14} /> },
      { label: "Best Practices", path: "/best-practices", icon: <FaFilePdf size={14} /> },
      { label: "Password Management", path: "/password-management", icon: <MdPassword size={14} /> },
    ]
  },
  {
    title: "Knowledge Navigator",
    icon: <FaBook size={16} />,
    titleColor: COLORS.labelColor,
    iconColor: COLORS.labelColor,
    options: [
      {
        label: "SOP Creation", path: "/sop-creation", icon: <FaFilePdf size={14} />,
        subtitle: "Signavio + Process map "
      },
      {
        label: "KT Documents", path: "/kt-documents", icon: <FaFilePdf size={14} />,
        subtitle: "Signavio"
      },
    ]
  },
  {
    title: "Development Navigator",
    icon: <MdDeveloperMode size={16} />,
    titleColor: COLORS.labelColor,
    iconColor: COLORS.labelColor,
    options: [
      { label: "ABAP Code Generator", path: "/abap-code-generator", icon: <FaCode size={14} /> },
      { label: "Smart Insights", path: "/smart-connectors", icon: <FaTools size={14} /> },
      { label: "Test Script Generator", path: "/test-script-genarator", icon: <FaUser size={14} /> },
    ]
  }
];

const Layout = ({ onLogout }) => {
  const [selectedLabel, setSelectedLabel] = useState("");
  const [openSections, setOpenSections] = useState({});
  const location = useLocation();

  const toggleSection = (title) => {
    setOpenSections(prev => ({
      ...prev,
      [title]: !prev[title]
    }));
  };

  useEffect(() => {
    const currentPath = location.pathname;
    for (const section of navigatorSections) {
      const foundOption = section.options.find(opt => opt.path === currentPath);
      if (foundOption) {
        setSelectedLabel(foundOption.label);
        break;
      }
    }
  }, [location.pathname]);

  return (
    <div className="flex max-h-screen min-h-screen bg-white">
      <div
        className="w-80 min-h-screen h-full rounded-r-2xl shadow-lg overflow-y-auto"
        style={{
          backgroundColor: COLORS.dark,
        }}
      >
        <div
          className="flex justify-center p-4 border-b border-opacity-20"
          // style={{ borderBottomColor: COLORS.secondary }}
        >
          <img
            src={chatbotIntro}
            alt="Chatbot Intro"
            className="h-16 w-28 rounded-md object-contain bg-white"
            // style={{
            //   // borderColor: COLORS.primary,
            //   boxShadow: `0 0 0 4px ${COLORS.secondary}`,
            // }}
          />
        </div>
        <div className="text-white p-3">
          {navigatorSections.map((section, sectionIndex) => (
            <div key={sectionIndex} className="mb-2">
              <button
                onClick={() => toggleSection(section.title)}
                className="flex items-center w-full p-2 space-x-2 rounded-lg transition duration-300 hover:bg-white/10"
              >
                <span style={{ color: section.iconColor }}>
                  {section.icon}
                </span>
                <span
                  className="font-bold text-sm"
                  style={{ color: section.titleColor }}
                >
                  {section.title}
                </span>
                <span className="ml-auto text-white">
                  {openSections[section.title] ? <FaChevronUp size={12} /> : <FaChevronDown size={12} />}
                </span>
              </button>

              <div
                className={`overflow-hidden transition-all duration-300 ease-in-out pl-3 ${openSections[section.title] ? "max-h-screen opacity-100 mt-1" : "max-h-0 opacity-0"
                  }`}
              >
                <ul className="space-y-1">
                  {section.options.map((option, optIndex) => (
                    <li key={optIndex}>
                      <Link
                        to={option.path}
                        className={`flex items-center p-2 w-full rounded-lg transition duration-300 ${location.pathname === option.path
                          ? "bg-white text-[#073161]"
                          : "text-white hover:bg-white/10"
                          }`}
                        onClick={() => setSelectedLabel(option.label)}
                      >
                        <span className="mr-2">
                          {option.icon}
                        </span>
                        <div className="text-sm">
                          <div className="font-medium">{option.label}</div>
                          {option.subtitle && (
                            <div className="text-xs opacity-70 mt-0.5">
                              {option.subtitle}
                            </div>
                          )}
                        </div>
                      </Link>
                    </li>
                  ))}
                </ul>
              </div>
            </div>
          ))}
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