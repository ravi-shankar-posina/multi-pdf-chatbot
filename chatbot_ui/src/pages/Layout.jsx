import React, { useEffect, useState, useRef } from "react";
import { FaCode, FaFilePdf, FaHeadset, FaUser, FaChevronDown, FaChevronUp, FaBook, FaTools, FaChevronLeft, FaChevronRight } from "react-icons/fa";
import { MdSupportAgent, MdDeveloperMode, MdPassword } from "react-icons/md";
import { TbDeviceDesktopAnalytics } from "react-icons/tb";
import chatbotIntro from "../assets/ai.png";
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
  },
  {
    title: "Agents Navigator",
    icon: <MdSupportAgent size={16} />,
    titleColor: COLORS.labelColor,
    iconColor: COLORS.labelColor,
    options: [
      { label: "Grise Agent", path: "/grise-agent", icon: <MdSupportAgent size={14} /> },
      { label: "IDOC Agent", path: "/idoc-agent", icon: <MdSupportAgent size={14} /> },
    ]
  }
];

const Layout = ({ onLogout }) => {
  const [selectedLabel, setSelectedLabel] = useState("");
  const [openSections, setOpenSections] = useState({});
  const [hoveredSection, setHoveredSection] = useState(null);
  const [sidebarCollapsed, setSidebarCollapsed] = useState(false);
  const location = useLocation();
  const popupRef = useRef(null);

  const toggleSection = (title) => {
    if (!sidebarCollapsed) {
      setOpenSections(prev => ({
        ...prev,
        [title]: !prev[title]
      }));
    }
  };

  const toggleSidebar = () => {
    setSidebarCollapsed(prev => !prev);
    setHoveredSection(null);
  };

  // Close popup when clicking outside
  useEffect(() => {
    function handleClickOutside(event) {
      if (popupRef.current && !popupRef.current.contains(event.target)) {
        setHoveredSection(null);
      }
    }

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  useEffect(() => {
    const currentPath = location.pathname;

    // Find which section contains the current path and set the label
    for (const section of navigatorSections) {
      const foundOption = section.options.find(opt => opt.path === currentPath);
      if (foundOption) {
        setSelectedLabel(foundOption.label);

        // Open the section containing the current path in expanded mode
        if (!sidebarCollapsed) {
          setOpenSections(prev => ({
            ...prev,
            [section.title]: true
          }));
        }
        break;
      }
    }
  }, [location.pathname, sidebarCollapsed]);

  // Function to find the section title for a given path
  const getSectionForPath = (path) => {
    for (const section of navigatorSections) {
      if (section.options.some(opt => opt.path === path)) {
        return section.title;
      }
    }
    return null;
  };

  // Active section based on current path
  const activeSection = getSectionForPath(location.pathname);

  return (
    <div className="flex max-h-screen min-h-screen bg-white">
      {/* Sidebar */}
      <div
        className={`flex flex-col max-h-screen min-h-screen bg-gray-800 relative transition-all duration-300 ease-in-out ${sidebarCollapsed ? "w-20" : "w-1/4"
          }`}
      >
        {/* Logo area */}
        <div
          className="flex justify-center p-4 border-b border-opacity-20"
          style={{ borderBottomColor: COLORS.secondary }}
        >
          <img
            src={chatbotIntro}
            alt="Chatbot Intro"
            className={`${sidebarCollapsed ? "h-8 w-8" : "h-16 w-16"} rounded-full object-contain border-4 bg-white transition-all duration-300`}
            style={{
              borderColor: COLORS.primary,
              boxShadow: `0 0 0 4px ${COLORS.secondary}`,
            }}
          />
        </div>

        {/* Toggle button - positioned at the middle right of sidebar */}
        <button
          onClick={toggleSidebar}
          className="absolute -right-3 top-1/2 transform -translate-y-1/2 bg-gray-800 border border-gray-600 hover:bg-gray-700 text-white rounded-full p-1 z-20"
          style={{ boxShadow: "0 0 5px rgba(0,0,0,0.3)" }}
        >
          {sidebarCollapsed ? <FaChevronRight size={14} /> : <FaChevronLeft size={14} />}
        </button>

        {/* Navigation sections */}
        <div className="text-white py-3 flex-1 overflow-y-auto">
          {navigatorSections.map((section, sectionIndex) => (
            <div
              key={sectionIndex}
              className="mb-1"
              onMouseEnter={() => sidebarCollapsed && setHoveredSection(section.title)}
            >
              <button
                onClick={() => toggleSection(section.title)}
                className={`flex items-center w-full p-2 rounded-lg transition duration-300 hover:bg-white/10 ${sidebarCollapsed ? "justify-center" : "space-x-2"
                  } ${activeSection === section.title && sidebarCollapsed ? "bg-white/20" : ""}`}
              >
                <span style={{ color: section.iconColor }}>
                  {section.icon}
                </span>
                {!sidebarCollapsed && (
                  <>
                    <span
                      className="font-bold text-sm"
                      style={{ color: section.titleColor }}
                    >
                      {section.title}
                    </span>
                    <span className="ml-auto text-white">
                      {openSections[section.title] ? <FaChevronUp size={12} /> : <FaChevronDown size={12} />}
                    </span>
                  </>
                )}
              </button>

              {/* Expanded sidebar section */}
              {!sidebarCollapsed && (
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
              )}

              {/* Popup menu for collapsed sidebar */}
              {sidebarCollapsed && hoveredSection === section.title && (
                <div
                  ref={popupRef}
                  className="fixed left-16 bg-gray-700 shadow-lg rounded-lg z-50 w-56"
                  style={{
                    top: '50%',
                    transform: 'translateY(-50%)',
                    maxHeight: '80vh',
                    overflow: 'hidden'
                  }}
                >
                  <div className="p-3 border-b border-gray-600 sticky top-0 bg-gray-700 z-10">
                    <div className="font-bold" style={{ color: section.titleColor }}>
                      {section.title}
                    </div>
                  </div>
                  <div className="py-2 overflow-y-auto" style={{ maxHeight: 'calc(80vh - 50px)' }}>
                    {section.options.map((option, optIndex) => (
                      <Link
                        key={optIndex}
                        to={option.path}
                        className={`flex items-center px-3 py-2 mx-1 rounded-lg transition duration-300 ${location.pathname === option.path
                          ? "bg-white text-[#073161]"
                          : "text-white hover:bg-white/10"
                          }`}
                        onClick={() => {
                          setSelectedLabel(option.label);
                          setHoveredSection(null);
                        }}
                      >
                        <span className="mr-3">
                          {option.icon}
                        </span>
                        <div>
                          <div className="font-medium">{option.label}</div>
                          {option.subtitle && (
                            <div className="text-xs opacity-70 mt-0.5">
                              {option.subtitle}
                            </div>
                          )}
                        </div>
                      </Link>
                    ))}
                  </div>
                </div>
              )}
            </div>
          ))}
        </div>
      </div>

      {/* Main Content Area */}
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