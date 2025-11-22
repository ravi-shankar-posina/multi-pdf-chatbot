import { useEffect, useRef, useState } from "react";
import {
  FaBook,
  FaCode,
  FaFilePdf,
  FaHeadset,
  FaTimes,
  FaTools,
  FaUser,
} from "react-icons/fa";
import {
  MdAppSettingsAlt,
  MdDeveloperMode,
  MdSupportAgent,
} from "react-icons/md";
import { TbDeviceDesktopAnalytics } from "react-icons/tb";
import { Link, Outlet, useLocation } from "react-router-dom";
import logo from "../assets/image.png";

const navigatorSections = [
  {
    title: "Development Navigator",
    icon: <MdDeveloperMode size={20} />,
    shortTitle: "Development",
    options: [
      {
        label: "ABAP Code Generator",
        path: "/abap-code-generator",
        icon: <FaCode size={14} />,
      },
      {
        label: "Smart Code Assistant",
        path: "/smart-connectors",
        icon: <FaTools size={14} />,
      },
      {
        label: "Test Script Generator",
        path: "/test-script-genarator",
        icon: <FaUser size={14} />,
      },
    ],
  },
  {
    title: "Knowledge Navigator",
    icon: <FaBook size={20} />,
    shortTitle: "Knowledge",
    options: [
      {
        label: "SOP Creation",
        path: "/sop-creation",
        icon: <FaFilePdf size={14} />,
      },
      {
        label: "KT Documents",
        path: "/kt-documents",
        icon: <FaFilePdf size={14} />,
      },
      {
        label: "SAP Best Practices",
        path: "/best-practices",
        icon: <FaFilePdf size={14} />,
      },
    ],
  },
  {
    title: "Support Navigator",
    icon: <MdSupportAgent size={20} />,
    shortTitle: "Support",
    options: [
      { label: "How To", path: "/how-to", icon: <FaHeadset size={14} /> },
      {
        label: "Incident Query",
        path: "/incident-query",
        icon: <FaCode size={14} />,
      },
      {
        label: "Incident Analysis",
        path: "/incident-analysis",
        icon: <TbDeviceDesktopAnalytics size={14} />,
      },
    ],
  },
  {
    title: "SAP Technical Agents",
    icon: <MdAppSettingsAlt size={20} />,
    shortTitle: "Technical",
    options: [
      {
        label: "IDOC Agent",
        path: "/idoc-agent",
        icon: <MdSupportAgent size={14} />,
      },
      {
        label: "Password Agent",
        path: "/pwd-agent",
        icon: <MdSupportAgent size={14} />,
      },
    ],
  },
  {
    title: "SAP Functional Agents",
    icon: <FaUser size={20} />,
    shortTitle: "Functional",
    options: [
      { label: "COA", path: "/coa", icon: <FaCode size={14} /> },
      {
        label: "PO Automation",
        path: "/poautomation",
        icon: <FaCode size={14} />,
      },
      {
        label: "Dynamic Mapping",
        path: "/apsuite",
        icon: <FaCode size={14} />,
      },
      {
        label: "BRD Agent",
        path: "/brd-agent",
        icon: <FaCode size={14} />,
      }
    ],
  },
];

const Layout = ({ onLogout }) => {
  const [selectedLabel, setSelectedLabel] = useState("SAP Support Framework");
  const [activeSection, setActiveSection] = useState(null);
  const [expandedSection, setExpandedSection] = useState(null);
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);
  const location = useLocation();
  const expandRef = useRef(null);

  const handleSectionClick = (sectionTitle) => {
    if (expandedSection === sectionTitle) {
      setExpandedSection(null);
    } else {
      setExpandedSection(sectionTitle);
    }
  };

  const closeExpanded = () => {
    setExpandedSection(null);
  };

  const handleOptionClick = (option) => {
    setSelectedLabel(option.label);
    setExpandedSection(null);
    setIsMobileMenuOpen(false);
  };

  const handleLogout = () => {
    if (onLogout) {
      onLogout();
    } else {
      alert("Logout clicked!");
    }
  };

  const toggleMobileMenu = () => {
    setIsMobileMenuOpen(!isMobileMenuOpen);
  };

  // Function to find the section title for a given path
  const getSectionForPath = (path) => {
    for (const section of navigatorSections) {
      if (section.options.some((opt) => opt.path === path)) {
        return section.title;
      }
    }
    return null;
  };

  // Close expanded section when clicking outside
  useEffect(() => {
    function handleClickOutside(event) {
      if (expandRef.current && !expandRef.current.contains(event.target)) {
        setExpandedSection(null);
      }
    }

    document.addEventListener("mousedown", handleClickOutside);
    return () => document.removeEventListener("mousedown", handleClickOutside);
  }, []);

  // Set initial selected label and active section based on current path
  useEffect(() => {
    const currentPath = location.pathname;

    if (currentPath === "/") {
      setSelectedLabel("G-Rise"); // or "SAP Support Framework"
      setActiveSection(null);
      return;
    }

    // Find which section contains the current path and set the label
    for (const section of navigatorSections) {
      const foundOption = section.options.find(
        (opt) => opt.path === currentPath
      );
      if (foundOption) {
        setSelectedLabel(foundOption.label);
        setActiveSection(section.title);
        return;
      }
    }
  }, [location.pathname]);

  // Close mobile menu when route changes
  useEffect(() => {
    setIsMobileMenuOpen(false);
  }, [location.pathname]);

  // Active section based on current path
  const activeSectionTitle = getSectionForPath(location.pathname);

  return (
    <div className="flex h-screen bg-white overflow-hidden">
      {/* Top Navbar */}
      <div className="fixed top-0 left-0 right-0 z-50 bg-white border-b border-gray-200 h-14">
        <div className="flex items-center justify-between h-full px-4">
          {/* Left side - Logo and Menu */}
          <div className="flex items-center space-x-4">
            <Link to="/">
              <img src={logo} alt="Logo" className="w-8 h-8 cursor-pointer" />
            </Link>

            {/* Mobile Menu Button */}
            <button
              onClick={toggleMobileMenu}
              className="lg:hidden p-2 text-black hover:bg-gray-100 rounded-lg transition-colors"
            >
              {isMobileMenuOpen ? (
                <FaTimes size={18} />
              ) : (
                <svg
                  width="18"
                  height="18"
                  viewBox="0 0 24 24"
                  fill="currentColor"
                >
                  <path d="M3 18h18v-2H3v2zm0-5h18v-2H3v2zm0-7v2h18V6H3z" />
                </svg>
              )}
            </button>
          </div>

          {/* Right side - Selected Option and User Actions */}
          <div className="flex items-center space-x-4">
            {/* Selected Option Display */}
            <div className="hidden sm:flex items-center bg-gray-100 px-3 py-1.5 rounded-lg">
              <span className="text-md font-semibold text-gray-700">
                {selectedLabel}
              </span>
            </div>

            {/* User Actions */}
            <div className="flex items-center space-x-2">
              <button className="p-2 text-black hover:bg-gray-100 rounded-lg transition-colors">
                <FaUser size={16} />
              </button>

              <button
                onClick={handleLogout}
                className="p-2 text-red-600 hover:bg-red-50 rounded-lg transition-colors"
                title="Logout"
              >
                <svg
                  width="16"
                  height="16"
                  viewBox="0 0 24 24"
                  fill="currentColor"
                >
                  <path d="M17 7l-1.41 1.41L18.17 11H8v2h10.17l-2.58 2.59L17 17l5-5zM4 5h8V3H4c-1.1 0-2 .9-2 2v14c0 1.1.9 2 2 2h8v-2H4V5z" />
                </svg>
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* Mobile Sidebar Overlay */}
      {isMobileMenuOpen && (
        <div className="lg:hidden fixed inset-0 z-40 bg-black bg-opacity-50 mt-14">
          <div className="fixed left-0 top-14 h-full w-72 bg-white transform transition-transform duration-300 ease-in-out overflow-y-auto">
            <div className="p-4">
              {navigatorSections.map((section, sectionIndex) => (
                <div key={sectionIndex} className="mb-6">
                  <div className="flex items-center mb-3 px-2 py-2 bg-gray-50 rounded-lg">
                    <span className="mr-3 text-black">{section.icon}</span>
                    <h3 className="font-semibold text-black text-sm">
                      {section.title}
                    </h3>
                  </div>
                  <div className="space-y-1 ml-2">
                    {section.options.map((option, optionIndex) => (
                      <Link
                        key={optionIndex}
                        to={option.path}
                        onClick={() => handleOptionClick(option)}
                        className={`flex items-center w-full p-3 rounded-lg transition-all duration-200 ${location.pathname === option.path
                            ? "bg-black text-white"
                            : "text-black hover:bg-gray-100"
                          }`}
                      >
                        <span className="mr-3">{option.icon}</span>
                        <span className="text-sm font-medium">
                          {option.label}
                        </span>
                      </Link>
                    ))}
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      )}

      {/* Desktop Sidebar */}
      <div className="hidden lg:flex w-16 xl:w-20 bg-white border-r border-gray-200 flex-col items-center py-4 mt-14 relative">
        {/* Navigation Icons */}
        <div className="flex-1 flex flex-col space-y-4">
          {navigatorSections.map((section, index) => (
            <div key={index} className="flex flex-col items-center">
              <button
                onClick={() => handleSectionClick(section.title)}
                className={`p-2.5 xl:p-3 rounded-lg transition-all duration-200 hover:bg-gray-300 ${activeSectionTitle === section.title
                    ? "bg-black text-white"
                    : "text-black"
                  } ${expandedSection === section.title ? "bg-gray-100" : ""}`}
              >
                {section.icon}
              </button>
              <span className="text-black text-xs mt-1 text-center leading-tight">
                {section.shortTitle}
              </span>
            </div>
          ))}
        </div>
      </div>

      {/* Desktop Expanded Section */}
      {expandedSection && (
        <div
          ref={expandRef}
          className="hidden lg:flex w-56 xl:w-64 bg-gray-100 border-r border-gray-200 flex-col shadow-lg mt-14"
        >
          <div className="p-3 xl:p-4 border-b border-gray-200 flex justify-between items-center bg-white">
            <h3 className="font-semibold text-black text-sm xl:text-base">
              {
                navigatorSections.find((s) => s.title === expandedSection)
                  ?.title
              }
            </h3>
            <button
              onClick={closeExpanded}
              className="text-gray-500 hover:text-black transition-colors"
            >
              <FaTimes size={14} />
            </button>
          </div>

          <div className="flex-1 p-2 bg-gray-50 overflow-y-auto">
            {navigatorSections
              .find((s) => s.title === expandedSection)
              ?.options.map((option, index) => (
                <Link
                  key={index}
                  to={option.path}
                  onClick={() => handleOptionClick(option)}
                  className={`flex items-center w-full p-2.5 xl:p-3 mb-1 rounded-lg transition-all duration-200 ${location.pathname === option.path
                      ? "bg-black text-white"
                      : "text-black hover:bg-gray-200"
                    }`}
                >
                  <span className="mr-3">{option.icon}</span>
                  <span className="text-sm font-medium">{option.label}</span>
                </Link>
              ))}
          </div>
        </div>
      )}

      {/* Main Content Area */}
      <div className="flex-1 flex flex-col h-screen">
        <main className="flex-1 overflow-auto mt-14 p-4 lg:p-6">
          <Outlet />
        </main>
      </div>
    </div>
  );
};

export default Layout;