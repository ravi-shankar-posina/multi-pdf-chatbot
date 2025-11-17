import React, { useState } from "react";
import {
  ChevronRight,
  TrendingUp,
  Zap,
  AlertCircle,
  Sparkles,
  ArrowLeft,
  Calendar,
  User,
  Share2,
  Bookmark,
  Code,
  Clock,
} from "lucide-react";
import { Link } from "react-router-dom";

// You need to import your actual logo
import logo from "../assets/image.png";

const Home = () => {
  const [currentView, setCurrentView] = useState("home");
  const [hoveredCard, setHoveredCard] = useState(null);

  // Placeholder logo - replace with your actual import
  // const logo = "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='100' height='100' viewBox='0 0 100 100'%3E%3Ccircle cx='50' cy='50' r='45' fill='%238B5CF6'/%3E%3Ctext x='50' y='65' font-size='40' text-anchor='middle' fill='white' font-family='Arial, sans-serif' font-weight='bold'%3EG%3C/text%3E%3C/svg%3E";

  // Get current date in format "Mon DD, YYYY"
  const getCurrentDate = () => {
    const date = new Date();
    return date.toLocaleDateString("en-US", {
      month: "short",
      day: "numeric",
      year: "numeric",
    });
  };

  const newsTopics = [
    {
      id: 1,
      slug: "sap-jule-up",
      title: "Latest in SAP Jule UP",
      description:
        "AI-powered automation revolutionizing enterprise workflows with intelligent test scripting.",
      gradient: "from-purple-500 via-pink-500 to-red-500",
      icon: <TrendingUp className="w-4 h-4" />,
      image:
        "https://images.unsplash.com/photo-1677442136019-21780ecad995?w=800&q=80",
      tag: "Latest on AI from SAP",
      date: getCurrentDate(),
      readTime: "5 min",
    },
    {
      id: 2,
      slug: "sap-up",
      title: "Latest on SAP UP",
      description:
        "Intelligent support systems with automated ticket resolution and real-time analytics.",
      gradient: "from-cyan-500 via-blue-500 to-indigo-500",
      icon: <Zap className="w-4 h-4" />,
      image:
        "https://images.unsplash.com/photo-1639762681485-074b7f938ba0?w=800&q=80",
      tag: "Latest on S4 from SAP",
      date: getCurrentDate(),
      readTime: "7 min",
    },
    {
      id: 3,
      slug: "grise-test",
      title: "G-RISE  latest in sap automation",
      description:
        "70% productivity boost with AI-driven  latest in sap automation and context-aware resolutions.",
      gradient: "from-orange-500 via-yellow-500 to-amber-500",
      icon: <AlertCircle className="w-4 h-4" />,
      image:
        "https://images.unsplash.com/photo-1551288049-bebda4e38f71?w=800&q=80",
      tag: "Latest on Automation from SAP",
      date: getCurrentDate(),
      readTime: "6 min",
    },
  ];

  const handleArticleClick = (slug) => {
    setCurrentView(slug);
  };

  const handleBackToHome = () => {
    setCurrentView("home");
  };

  // Article Components
  const SAPJuleUPArticle = () => (
    <div className="h-full bg-white text-black overflow-auto flex flex-col">
      <div className="flex-1">
        <div className="max-w-6xl mx-auto px-4 py-5">
          <button
            className="mb-4 text-black hover:text-gray-600 flex items-center gap-2 transition-colors text-sm"
            onClick={handleBackToHome}
          >
            <ArrowLeft className="w-4 h-4" />
            Back to Home
          </button>

          <div className="mb-4">
            <span className="inline-block px-2.5 py-1 bg-purple-100 text-purple-700 border border-purple-300 rounded-full text-xs font-semibold mb-2">
              AI Innovation
            </span>
            <h1 className="text-2xl font-bold mb-3 bg-gradient-to-r from-black via-purple-700 to-pink-700 bg-clip-text text-transparent">
              Latest on AI from SAP: Revolutionary AI-Powered Automation
            </h1>
            <div className="flex items-center gap-3 text-gray-600 text-xs">
              <span className="flex items-center gap-1">
                <Calendar className="w-3 h-3" />
                {getCurrentDate()}
              </span>
              <span className="flex items-center gap-1">
                <User className="w-3 h-3" />
                SAP Innovation Team
              </span>
              <span>5 min read</span>
            </div>
          </div>

          <img
            src="https://images.unsplash.com/photo-1677442136019-21780ecad995?w=1200&q=80"
            alt="SAP Jule UP"
            className="w-full h-40 object-cover rounded-xl mb-4"
          />

          <div className="grid md:grid-cols-2 gap-5">
            <div className="space-y-3 text-sm text-gray-700 leading-relaxed">
              <p className="text-base text-black font-medium">
                SAP Jule UP is ushering in a new era of enterprise automation,
                combining cutting-edge AI with proven SAP methodologies.
              </p>

              <div className="bg-gray-50 border border-gray-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-2">
                  What's New?
                </h3>
                <ul className="space-y-1.5 text-xs">
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-purple-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Intelligent Test Scripts:</strong> 85% reduction
                      in manual scripting time
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-purple-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Predictive Detection:</strong> ML algorithms
                      identify issues before production
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-purple-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Self-Healing:</strong> Automated remediation
                      without intervention
                    </span>
                  </li>
                </ul>
              </div>

              <p className="text-xs">
                Organizations implementing Jule UP have reported remarkable
                results: test coverage increased from 60% to 95%, while testing
                cycles shortened from weeks to days.
              </p>
            </div>

            <div className="space-y-3">
              <div className="bg-gradient-to-br from-purple-50 to-pink-50 border border-purple-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-3">
                  Key Benefits
                </h3>
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <div className="text-2xl font-bold text-purple-600 mb-1">
                      85%
                    </div>
                    <p className="text-xs text-gray-700">Scripting reduction</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-pink-600 mb-1">
                      95%
                    </div>
                    <p className="text-xs text-gray-700">Test coverage</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-purple-600 mb-1">
                      4x
                    </div>
                    <p className="text-xs text-gray-700">Faster deployments</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-pink-600 mb-1">
                      60%
                    </div>
                    <p className="text-xs text-gray-700">Cost reduction</p>
                  </div>
                </div>
              </div>

              <div className="text-xs text-gray-700 leading-relaxed space-y-2">
                <p>
                  A leading European retailer achieved automated test coverage
                  across 500 custom SAP modules within just 3 months.
                </p>
                <p>
                  The system's intelligent learning continuously improves
                  accuracy and efficiency, creating a virtuous cycle of
                  improvement.
                </p>
              </div>

              <div className="flex items-center gap-2 pt-2">
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Share2 className="w-3 h-3" />
                  Share
                </button>
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Bookmark className="w-3 h-3" />
                  Save
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );

  const SAPUPArticle = () => (
    <div className="h-full bg-white text-black overflow-auto flex flex-col">
      <div className="flex-1">
        <div className="max-w-6xl mx-auto px-4 py-5">
          <button
            className="mb-4 text-black hover:text-gray-600 flex items-center gap-2 transition-colors text-sm"
            onClick={handleBackToHome}
          >
            <ArrowLeft className="w-4 h-4" />
            Back to Home
          </button>

          <div className="mb-4">
            <span className="inline-block px-2.5 py-1 bg-blue-100 text-blue-700 border border-blue-300 rounded-full text-xs font-semibold mb-2">
              Breaking News
            </span>
            <h1 className="text-2xl font-bold mb-3 bg-gradient-to-r from-black via-cyan-700 to-blue-700 bg-clip-text text-transparent">
              Latest on S4 from SAP: Next-Generation Intelligent Support Systems
            </h1>
            <div className="flex items-center gap-3 text-gray-600 text-xs">
              <span className="flex items-center gap-1">
                <Calendar className="w-3 h-3" />
                {getCurrentDate()}
              </span>
              <span className="flex items-center gap-1">
                <User className="w-3 h-3" />
                G-RISE Research Lab
              </span>
              <span>7 min read</span>
            </div>
          </div>

          <img
            src="https://images.unsplash.com/photo-1639762681485-074b7f938ba0?w=1200&q=80"
            alt="SAP UP"
            className="w-full h-40 object-cover rounded-xl mb-4"
          />

          <div className="grid md:grid-cols-2 gap-5">
            <div className="space-y-3 text-sm text-gray-700 leading-relaxed">
              <p className="text-base text-black font-medium">
                SAP UP represents a paradigm shift in enterprise
                support—transforming reactive support into proactive problem
                prevention.
              </p>

              <div className="bg-gray-50 border border-gray-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-2">
                  Revolutionary Features
                </h3>
                <ul className="space-y-1.5 text-xs">
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-cyan-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>AI-Powered Triage:</strong> 98% accuracy in ticket
                      categorization
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-cyan-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Real-Time Analytics:</strong> Comprehensive system
                      health visibility
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-cyan-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Knowledge Base:</strong> Auto-generates
                      documentation from tickets
                    </span>
                  </li>
                </ul>
              </div>

              <p className="text-xs">
                SAP UP's AI engine processes over 10,000 support interactions
                daily, learning from each resolution to improve future
                responses.
              </p>

              <blockquote className="border-l-4 border-cyan-500 pl-3 italic text-gray-800 bg-gray-50 p-2 text-xs">
                "SAP UP has transformed our support operations. The AI doesn't
                just answer questions—it anticipates problems and prevents
                them."
                <footer className="text-gray-600 mt-1 text-xs not-italic">
                  — Director of IT Operations, Fortune 500
                </footer>
              </blockquote>
            </div>

            <div className="space-y-3">
              <div className="bg-gradient-to-br from-cyan-50 to-blue-50 border border-cyan-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-3">
                  Performance Metrics
                </h3>
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <div className="text-2xl font-bold text-cyan-600 mb-1">
                      98%
                    </div>
                    <p className="text-xs text-gray-700">Accurate triage</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-blue-600 mb-1">
                      65%
                    </div>
                    <p className="text-xs text-gray-700">Faster resolution</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-cyan-600 mb-1">
                      80%
                    </div>
                    <p className="text-xs text-gray-700">Auto-resolved</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-blue-600 mb-1">
                      24/7
                    </div>
                    <p className="text-xs text-gray-700">Monitoring</p>
                  </div>
                </div>
              </div>

              <div className="text-xs text-gray-700 leading-relaxed space-y-2">
                <p>
                  A global financial services firm saw average resolution time
                  drop from 48 hours to just 18 hours within the first quarter.
                </p>
                <p>
                  SAP UP not only meets support needs but exceeds them,
                  providing a foundation for continuous improvement in
                  enterprise operations.
                </p>
              </div>

              <div className="flex items-center gap-2 pt-2">
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Share2 className="w-3 h-3" />
                  Share
                </button>
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Bookmark className="w-3 h-3" />
                  Save
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );

  const GRISETestArticle = () => (
    <div className="h-full bg-white text-black overflow-auto flex flex-col">
      <div className="flex-1">
        <div className="max-w-6xl mx-auto px-4 py-5">
          <button
            className="mb-4 text-black hover:text-gray-600 flex items-center gap-2 transition-colors text-sm"
            onClick={handleBackToHome}
          >
            <ArrowLeft className="w-4 h-4" />
            Back to Home
          </button>

          <div className="mb-4">
            <span className="inline-block px-2.5 py-1 bg-orange-100 text-orange-700 border border-orange-300 rounded-full text-xs font-semibold mb-2">
              Game Changer
            </span>
            <h1 className="text-2xl font-bold mb-3 bg-gradient-to-r from-black via-orange-700 to-yellow-700 bg-clip-text text-transparent">
              Latest on Automation from SAP: 70% Productivity Revolution
            </h1>
            <div className="flex items-center gap-3 text-gray-600 text-xs">
              <span className="flex items-center gap-1">
                <Calendar className="w-3 h-3" />
                {getCurrentDate()}
              </span>
              <span className="flex items-center gap-1">
                <User className="w-3 h-3" />
                Technical Team
              </span>
              <span>6 min read</span>
            </div>
          </div>

          <img
            src="https://images.unsplash.com/photo-1551288049-bebda4e38f71?w=1200&q=80"
            alt="G-RISE  latest in sap automation"
            className="w-full h-40 object-cover rounded-xl mb-4"
          />

          <div className="grid md:grid-cols-2 gap-5">
            <div className="space-y-3 text-sm text-gray-700 leading-relaxed">
              <p className="text-base text-black font-medium">
                G-RISE latest in sap automation delivers a 70% productivity
                boost through intelligent, context-aware automation and ABAP
                code generation.
              </p>

              <div className="bg-gray-50 border border-gray-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-2">
                  Core Capabilities
                </h3>
                <ul className="space-y-1.5 text-xs">
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-orange-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Root Cause Analysis:</strong> Identifies issues in
                      minutes vs hours
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-orange-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>Context-Aware:</strong> Tailored solutions for
                      your SAP environment
                    </span>
                  </li>
                  <li className="flex items-start gap-2">
                    <div className="w-1.5 h-1.5 bg-orange-500 rounded-full mt-1.5 flex-shrink-0"></div>
                    <span>
                      <strong>ABAP Generation:</strong> Production-ready code
                      with error handling
                    </span>
                  </li>
                </ul>
              </div>

              <p className="text-xs">
                Traditional SAP testing requires significant manual effort.
                G-RISE automates time-consuming tasks, allowing teams to focus
                on strategic initiatives.
              </p>

              <blockquote className="border-l-4 border-orange-500 pl-3 italic text-gray-800 bg-gray-50 p-2 text-xs">
                "Before G-RISE, 60% of time went to repetitive tasks. Now we
                focus on innovation. The 70% productivity gain is real and
                transformative."
                <footer className="text-gray-600 mt-1 text-xs not-italic">
                  — VP of Technology, Global Manufacturing
                </footer>
              </blockquote>
            </div>

            <div className="space-y-3">
              <div className="bg-gradient-to-br from-orange-50 to-yellow-50 border border-orange-200 rounded-lg p-3">
                <h3 className="text-sm font-bold text-black mb-3">
                  Impact Analysis
                </h3>
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <div className="text-2xl font-bold text-orange-600 mb-1">
                      70%
                    </div>
                    <p className="text-xs text-gray-700">Productivity boost</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-yellow-600 mb-1">
                      90%
                    </div>
                    <p className="text-xs text-gray-700">Less manual testing</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-orange-600 mb-1">
                      50%
                    </div>
                    <p className="text-xs text-gray-700">Fewer dependencies</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-yellow-600 mb-1">
                      10x
                    </div>
                    <p className="text-xs text-gray-700">Faster diagnosis</p>
                  </div>
                </div>
              </div>

              <div className="text-xs text-gray-700 leading-relaxed space-y-2">
                <p>
                  G-RISE generates production-quality ABAP code automatically,
                  including comprehensive error handling and performance
                  optimization.
                </p>
                <p>
                  Knowledge democratization accelerates onboarding—new team
                  members become productive faster with G-RISE as a mentor.
                </p>
                <p>
                  As the system processes more scenarios, it becomes
                  increasingly accurate and efficient—continuous productivity
                  improvement.
                </p>
              </div>

              <div className="flex items-center gap-2">
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Share2 className="w-3 h-3" />
                  Share
                </button>
                <button className="flex items-center gap-2 px-3 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors text-xs">
                  <Bookmark className="w-3 h-3" />
                  Save
                </button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );

  // Home View
  if (currentView === "home") {
    return (
      <div className="h-full bg-white text-black flex flex-col overflow-hidden">
        {/* Hero Section */}
        <div className="relative border-b border-gray-200 py-2">
          <div className="absolute inset-0 bg-gradient-to-br from-gray-50 via-white to-white opacity-90"></div>

          <div className="absolute top-3 left-8 w-24 h-24 bg-gradient-to-br from-purple-200/20 to-transparent rounded-full blur-3xl animate-pulse"></div>
          <div
            className="absolute bottom-3 right-8 w-32 h-32 bg-gradient-to-br from-blue-200/20 to-transparent rounded-full blur-3xl animate-pulse"
            style={{ animationDelay: "1s" }}
          ></div>

          <div className="relative max-w-7xl mx-auto px-6 flex items-center justify-between">
            <div className="text-center">
              <div className="inline-flex items-center gap-3 mb-3">
                <Sparkles className="w-5 h-5 text-purple-600" />
                <h1 className="text-3xl sm:text-4xl md:text-5xl lg:text-6xl font-bold tracking-tighter flex items-center gap-2 md:gap-3">
                  <img
                    src={logo}
                    alt="Logo"
                    className="w-10 h-10 sm:w-12 sm:h-12 md:w-14 md:h-14 lg:w-16 lg:h-16"
                  />
                  <span className="bg-gradient-to-r from-black via-purple-700 to-blue-700 bg-clip-text text-transparent">
                    G-RISE
                  </span>
                </h1>
                <Sparkles className="w-5 h-5 text-blue-600" />
              </div>
              <p className="text-xs sm:text-sm text-gray-600">
                AI-Powered SAP Automation & Support Excellence
              </p>
            </div>
            <div className="grid md:grid-cols-3 gap-4 ">
              <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-4 hover:border-purple-300 hover:shadow-lg transition-all duration-300">
                <div className="text-4xl font-bold bg-gradient-to-r from-purple-600 to-pink-600 bg-clip-text text-transparent mb-2">
                  70%
                </div>
                <div className="text-gray-600 text-sm">
                  Productivity Increase
                </div>
              </div>

              <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-4 hover:border-blue-300 hover:shadow-lg transition-all duration-300">
                <Code className="w-8 h-8 text-blue-600 mb-3" />
                <div className="text-gray-600 text-sm">
                  Automated Code Generation
                </div>
              </div>

              <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-4 hover:border-orange-300 hover:shadow-lg transition-all duration-300">
                <Clock className="w-8 h-8 text-orange-600 mb-3" />
                <div className="text-gray-600 text-sm">
                  Real-Time Resolutions
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Main Content */}
        <div className="flex-1 overflow-hidden">
          <div className="h-full max-w-7xl mx-auto px-6 py-4 flex flex-col">
            <div className="mb-4">
              <div className="flex items-center gap-2 mb-2">
                <h2 className="text-xl sm:text-2xl md:text-3xl font-bold text-black">
                  Latest News & Updates
                </h2>
              </div>
              <p className="text-gray-600 text-xs sm:text-sm">
                Stay ahead with cutting-edge innovations
              </p>
              <div className="h-1 w-20 bg-gradient-to-r from-purple-500 via-blue-500 to-orange-500 mt-2"></div>
            </div>

            {/* News Cards Grid */}
            <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4 lg:gap-5 mb-4">
              {newsTopics.map((topic, index) => (
                <div
                  key={topic.id}
                  className="group relative bg-white border-2 border-gray-200 rounded-xl overflow-hidden transition-all duration-500 hover:scale-[1.02] hover:shadow-2xl hover:shadow-gray-400/30 hover:border-gray-300 cursor-pointer"
                  onMouseEnter={() => setHoveredCard(topic.id)}
                  onMouseLeave={() => setHoveredCard(null)}
                  onClick={() => handleArticleClick(topic.slug)}
                  style={{
                    animation: `fade-slide-up 0.6s ease-out ${
                      index * 0.15
                    }s both`,
                  }}
                >
                  {/* Image Container */}
                  <div className="relative h-36 sm:h-40 lg:h-44 overflow-hidden">
                    <img
                      src={topic.image}
                      alt={topic.title}
                      className="w-full h-full object-cover transition-transform duration-700 group-hover:scale-110"
                    />
                    <div
                      className={`absolute inset-0 bg-gradient-to-t ${topic.gradient} opacity-50 group-hover:opacity-70 transition-opacity duration-500`}
                    ></div>

                    <div className="absolute top-3 left-3 px-2.5 py-1 rounded-full bg-white/95 backdrop-blur-sm border border-gray-200 shadow-md text-black text-[10px] sm:text-xs font-semibold uppercase tracking-wider">
                      {topic.tag}
                    </div>

                    <div
                      className={`absolute top-3 right-3 p-2 rounded-full bg-white/95 backdrop-blur-sm border border-gray-200 shadow-md text-black transition-all duration-500 ${
                        hoveredCard === topic.id ? "scale-110 rotate-12" : ""
                      }`}
                    >
                      {topic.icon}
                    </div>

                    <div
                      className={`absolute inset-0 bg-black/10 backdrop-blur-[2px] transition-opacity duration-300 ${
                        hoveredCard === topic.id ? "opacity-100" : "opacity-0"
                      }`}
                    ></div>
                  </div>

                  <div
                    className={`absolute inset-0 rounded-xl bg-gradient-to-r ${topic.gradient} opacity-0 group-hover:opacity-10 transition-opacity duration-500 pointer-events-none`}
                  ></div>
                </div>
              ))}
            </div>

            {/* CTA */}
            <div className="text-center flex-shrink-0">
              <div className="inline-block bg-gradient-to-br from-gray-50 to-white border-2 border-gray-200 rounded-xl p-4 hover:border-gray-300 hover:shadow-lg transition-all duration-300">
                <h3 className="text-base font-bold mb-2 text-black">
                  Ready to Transform Your SAP Support?
                </h3>
                <p className="text-gray-600 text-xs mb-3 max-w-xl">
                  Join industry leaders who've boosted productivity by 70%
                </p>
                <Link to="/how-to">
                  <button className="px-5 py-2 bg-black text-white font-semibold rounded-lg hover:bg-gray-800 transition-all duration-300 hover:scale-105 text-xs">
                    Get Started Today
                  </button>
                </Link>
              </div>
            </div>
          </div>
        </div>

        <style jsx>{`
          @keyframes fade-slide-up {
            from {
              opacity: 0;
              transform: translateY(30px);
            }
            to {
              opacity: 1;
              transform: translateY(0);
            }
          }

          .line-clamp-2 {
            display: -webkit-box;
            -webkit-line-clamp: 2;
            -webkit-box-orient: vertical;
            overflow: hidden;
          }
        `}</style>
      </div>
    );
  }

  // Render article views
  if (currentView === "sap-jule-up") return <SAPJuleUPArticle />;
  if (currentView === "sap-up") return <SAPUPArticle />;
  if (currentView === "grise-test") return <GRISETestArticle />;

  return null;
};

export default Home;
