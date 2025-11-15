import React, { useState } from "react";
import {
  ChevronRight,
  TrendingUp,
  Zap,
  AlertCircle,
  Sparkles,
  Code,
  Clock,
  ArrowLeft,
  Calendar,
  User,
  Share2,
  Bookmark,
} from "lucide-react";
import logo from "../assets/image.png";
import { Link } from "react-router-dom";

const Home = () => {
  const [currentView, setCurrentView] = useState("home");
  const [hoveredCard, setHoveredCard] = useState(null);

  const newsTopics = [
    {
      id: 1,
      slug: "sap-jule-up",
      title: "Latest in SAP Jule UP",
      description:
        "Breakthrough updates in AI-powered automation. Discover how Jule UP is revolutionizing enterprise workflows with intelligent test scripting.",
      gradient: "from-purple-500 via-pink-500 to-red-500",
      icon: <TrendingUp className="w-6 h-6" />,
      image:
        "https://images.unsplash.com/photo-1677442136019-21780ecad995?w=800&q=80",
      tag: "AI Innovation",
      date: "November 12, 2025",
      author: "SAP Innovation Team",
      readTime: "5 min read",
    },
    {
      id: 2,
      slug: "sap-up",
      title: "Latest on SAP UP",
      description:
        "New era of intelligent support systems. SAP UP introduces groundbreaking features for automated ticket resolution and real-time analytics.",
      gradient: "from-cyan-500 via-blue-500 to-indigo-500",
      icon: <Zap className="w-6 h-6" />,
      image:
        "https://images.unsplash.com/photo-1639762681485-074b7f938ba0?w=800&q=80",
      tag: "Breaking News",
      date: "November 10, 2025",
      author: "G-RISE Research Lab",
      readTime: "7 min read",
    },
    {
      id: 3,
      slug: "grise-test",
      title: "G-RISE Test Automation",
      description:
        "Experience 70% productivity boost with AI-driven test automation. G-RISE delivers context-aware resolutions with unprecedented accuracy.",
      gradient: "from-orange-500 via-yellow-500 to-amber-500",
      icon: <AlertCircle className="w-6 h-6" />,
      image:
        "https://images.unsplash.com/photo-1551288049-bebda4e38f71?w=800&q=80",
      tag: "Game Changer",
      date: "November 8, 2025",
      author: "Technical Team",
      readTime: "6 min read",
    },
  ];

  const handleArticleClick = (slug) => {
    setCurrentView(slug);
    window.scrollTo({ top: 0, behavior: "smooth" });
  };

  const handleBackToHome = () => {
    setCurrentView("home");
    window.scrollTo({ top: 0, behavior: "smooth" });
  };

  // Article Components
  const SAPJuleUPArticle = () => (
    <div className="min-h-screen bg-white text-black">
      <div className="max-w-4xl mx-auto px-6 py-12">
        <button
          className="mb-8 text-black hover:text-gray-600 flex items-center gap-2 transition-colors"
          onClick={handleBackToHome}
        >
          <ArrowLeft className="w-4 h-4" />
          Back to Home
        </button>

        <div className="mb-8">
          <span className="inline-block px-3 py-1 bg-purple-100 text-purple-700 border border-purple-300 rounded-full text-xs font-semibold mb-4">
            AI Innovation
          </span>
          <h1 className="text-5xl font-bold mb-6 bg-gradient-to-r from-black via-purple-700 to-pink-700 bg-clip-text text-transparent">
            Latest in SAP Jule UP: Revolutionary AI-Powered Automation
          </h1>
          <div className="flex items-center gap-6 text-gray-600 text-sm">
            <span className="flex items-center gap-2">
              <Calendar className="w-4 h-4" />
              November 12, 2025
            </span>
            <span className="flex items-center gap-2">
              <User className="w-4 h-4" />
              SAP Innovation Team
            </span>
            <span>5 min read</span>
          </div>
        </div>

        <img
          src="https://images.unsplash.com/photo-1677442136019-21780ecad995?w=1200&q=80"
          alt="SAP Jule UP"
          className="w-full h-96 object-cover rounded-2xl mb-12"
        />

        <div className="space-y-6 text-gray-700 leading-relaxed">
          <p className="text-xl text-black font-medium">
            SAP Jule UP is ushering in a new era of enterprise automation,
            combining cutting-edge artificial intelligence with proven SAP
            methodologies to deliver unprecedented efficiency gains.
          </p>

          <div className="bg-gray-50 border border-gray-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-4">
              What's New in Jule UP?
            </h3>
            <ul className="space-y-3 text-gray-700">
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-purple-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Intelligent Test Script Generation:
                  </strong>{" "}
                  Automatically creates comprehensive test scenarios based on
                  business logic analysis, reducing manual scripting time by
                  85%.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-purple-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Predictive Error Detection:
                  </strong>{" "}
                  Machine learning algorithms identify potential issues before
                  they impact production systems.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-purple-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Self-Healing Workflows:
                  </strong>{" "}
                  Automated remediation of common failures without human
                  intervention.
                </span>
              </li>
            </ul>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Transforming Enterprise Workflows
          </h2>

          <p>
            The latest Jule UP release introduces groundbreaking capabilities
            that fundamentally change how organizations approach SAP testing and
            maintenance. By leveraging advanced natural language processing and
            deep learning models trained on millions of SAP transactions, Jule
            UP can now understand business intent and automatically generate
            optimized test cases.
          </p>

          <p>
            Organizations implementing Jule UP have reported remarkable results:
            test coverage increased from 60% to 95%, while testing cycles
            shortened from weeks to days. One Fortune 500 manufacturer reduced
            their regression testing time from 3 weeks to just 4 days, enabling
            faster feature releases and improved time-to-market.
          </p>

          <div className="bg-gradient-to-br from-purple-50 to-pink-50 border border-purple-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-6">Key Benefits</h3>
            <div className="grid md:grid-cols-2 gap-6">
              <div>
                <div className="text-4xl font-bold text-purple-600 mb-2">
                  85%
                </div>
                <p className="text-gray-700">
                  Reduction in manual scripting effort
                </p>
              </div>
              <div>
                <div className="text-4xl font-bold text-pink-600 mb-2">95%</div>
                <p className="text-gray-700">
                  Test coverage achieved automatically
                </p>
              </div>
              <div>
                <div className="text-4xl font-bold text-purple-600 mb-2">
                  4x
                </div>
                <p className="text-gray-700">Faster deployment cycles</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-pink-600 mb-2">60%</div>
                <p className="text-gray-700">Cost reduction in QA operations</p>
              </div>
            </div>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Real-World Impact
          </h2>

          <p>
            A leading European retailer recently shared their Jule UP success
            story. With over 500 custom SAP modules, their testing process was
            consuming enormous resources. After implementing Jule UP, they
            achieved automated test coverage across all modules within just 3
            months—a process that would have taken their team over 2 years
            manually.
          </p>

          <p>
            The system's intelligent learning capabilities mean it continuously
            improves. As Jule UP processes more transactions and scenarios, its
            accuracy and efficiency compound, creating a virtuous cycle of
            improvement that benefits all users across the platform.
          </p>

          <div className="border-t border-gray-200 my-8"></div>

          <div className="flex items-center gap-4 pt-6">
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Share2 className="w-4 h-4" />
              Share Article
            </button>
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Bookmark className="w-4 h-4" />
              Save for Later
            </button>
          </div>
        </div>
      </div>
    </div>
  );

  const SAPUPArticle = () => (
    <div className="min-h-screen bg-white text-black">
      <div className="max-w-4xl mx-auto px-6 py-12">
        <button
          className="mb-8 text-black hover:text-gray-600 flex items-center gap-2 transition-colors"
          onClick={handleBackToHome}
        >
          <ArrowLeft className="w-4 h-4" />
          Back to Home
        </button>

        <div className="mb-8">
          <span className="inline-block px-3 py-1 bg-blue-100 text-blue-700 border border-blue-300 rounded-full text-xs font-semibold mb-4">
            Breaking News
          </span>
          <h1 className="text-5xl font-bold mb-6 bg-gradient-to-r from-black via-cyan-700 to-blue-700 bg-clip-text text-transparent">
            SAP UP: Next-Generation Intelligent Support Systems
          </h1>
          <div className="flex items-center gap-6 text-gray-600 text-sm">
            <span className="flex items-center gap-2">
              <Calendar className="w-4 h-4" />
              November 10, 2025
            </span>
            <span className="flex items-center gap-2">
              <User className="w-4 h-4" />
              G-RISE Research Lab
            </span>
            <span>7 min read</span>
          </div>
        </div>

        <img
          src="https://images.unsplash.com/photo-1639762681485-074b7f938ba0?w=1200&q=80"
          alt="SAP UP"
          className="w-full h-96 object-cover rounded-2xl mb-12"
        />

        <div className="space-y-6 text-gray-700 leading-relaxed">
          <p className="text-xl text-black font-medium">
            SAP UP represents a paradigm shift in how enterprises handle support
            tickets, system monitoring, and issue resolution—transforming
            reactive support into proactive problem prevention.
          </p>

          <div className="bg-gray-50 border border-gray-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-4">
              Revolutionary Features
            </h3>
            <ul className="space-y-3 text-gray-700">
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-cyan-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    AI-Powered Ticket Triage:
                  </strong>{" "}
                  Instantly categorizes and prioritizes support tickets with 98%
                  accuracy, routing them to the right experts automatically.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-cyan-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Real-Time Analytics Dashboard:
                  </strong>{" "}
                  Comprehensive visibility into system health, ticket trends,
                  and resolution metrics.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-cyan-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Knowledge Base Auto-Generation:
                  </strong>{" "}
                  Automatically creates and updates documentation based on
                  resolved tickets.
                </span>
              </li>
            </ul>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Intelligent Automation at Scale
          </h2>

          <p>
            SAP UP's advanced AI engine processes over 10,000 support
            interactions daily, learning from each resolution to improve future
            responses. The system analyzes patterns across tickets, identifying
            recurring issues and suggesting preventive measures before problems
            escalate.
          </p>

          <p>
            The platform's natural language understanding capabilities allow it
            to interpret user queries in multiple languages and technical
            contexts. Whether a user describes an issue in business terms or
            technical jargon, SAP UP translates the intent and delivers precise
            solutions.
          </p>

          <div className="bg-gradient-to-br from-cyan-50 to-blue-50 border border-cyan-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-6">
              Performance Metrics
            </h3>
            <div className="grid md:grid-cols-2 gap-6">
              <div>
                <div className="text-4xl font-bold text-cyan-600 mb-2">98%</div>
                <p className="text-gray-700">Accurate ticket categorization</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-blue-600 mb-2">65%</div>
                <p className="text-gray-700">Faster resolution times</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-cyan-600 mb-2">80%</div>
                <p className="text-gray-700">Auto-resolved common issues</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-blue-600 mb-2">
                  24/7
                </div>
                <p className="text-gray-700">
                  Continuous intelligent monitoring
                </p>
              </div>
            </div>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Customer Success Stories
          </h2>

          <p>
            A global financial services firm implemented SAP UP across 15
            regional offices, handling over 5,000 monthly tickets. Within the
            first quarter, they saw their average resolution time drop from 48
            hours to just 18 hours. More impressively, 80% of routine issues
            were resolved automatically without human intervention.
          </p>

          <blockquote className="border-l-4 border-cyan-500 pl-6 italic text-gray-800 my-8 py-4 bg-gray-50">
            "SAP UP has transformed our support operations. What used to require
            a team of 20 specialists now runs efficiently with 8, while our
            customer satisfaction scores have never been higher. The AI doesn't
            just answer questions—it anticipates problems and prevents them."
            <footer className="text-gray-600 mt-2 text-sm not-italic">
              — Director of IT Operations, Fortune 500 Financial Institution
            </footer>
          </blockquote>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            The Future of Support
          </h2>

          <p>
            SAP UP represents more than incremental improvement—it's a complete
            reimagining of enterprise support. By combining machine learning,
            natural language processing, and deep SAP expertise, the platform
            delivers support experiences that are faster, smarter, and more
            satisfying for both end-users and support teams.
          </p>

          <p>
            As organizations continue to expand their SAP footprints, the need
            for intelligent, scalable support becomes critical. SAP UP not only
            meets this need but exceeds it, providing a foundation for
            continuous improvement and innovation in enterprise support
            operations.
          </p>

          <div className="border-t border-gray-200 my-8"></div>

          <div className="flex items-center gap-4 pt-6">
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Share2 className="w-4 h-4" />
              Share Article
            </button>
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Bookmark className="w-4 h-4" />
              Save for Later
            </button>
          </div>
        </div>
      </div>
    </div>
  );

  const GRISETestArticle = () => (
    <div className="min-h-screen bg-white text-black">
      <div className="max-w-4xl mx-auto px-6 py-12">
        <button
          className="mb-8 text-black hover:text-gray-600 flex items-center gap-2 transition-colors"
          onClick={handleBackToHome}
        >
          <ArrowLeft className="w-4 h-4" />
          Back to Home
        </button>

        <div className="mb-8">
          <span className="inline-block px-3 py-1 bg-orange-100 text-orange-700 border border-orange-300 rounded-full text-xs font-semibold mb-4">
            Game Changer
          </span>
          <h1 className="text-5xl font-bold mb-6 bg-gradient-to-r from-black via-orange-700 to-yellow-700 bg-clip-text text-transparent">
            G-RISE Test Automation: 70% Productivity Revolution
          </h1>
          <div className="flex items-center gap-6 text-gray-600 text-sm">
            <span className="flex items-center gap-2">
              <Calendar className="w-4 h-4" />
              November 8, 2025
            </span>
            <span className="flex items-center gap-2">
              <User className="w-4 h-4" />
              Technical Team
            </span>
            <span>6 min read</span>
          </div>
        </div>

        <img
          src="https://images.unsplash.com/photo-1551288049-bebda4e38f71?w=1200&q=80"
          alt="G-RISE Test Automation"
          className="w-full h-96 object-cover rounded-2xl mb-12"
        />

        <div className="space-y-6 text-gray-700 leading-relaxed">
          <p className="text-xl text-black font-medium">
            G-RISE Test Automation is revolutionizing SAP application support by
            delivering a 70% productivity boost through intelligent,
            context-aware test automation and ABAP code generation powered by
            advanced AI.
          </p>

          <div className="bg-gray-50 border border-gray-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-4">
              Core Capabilities
            </h3>
            <ul className="space-y-3 text-gray-700">
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-orange-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Automated Root Cause Analysis:
                  </strong>{" "}
                  Deep-dive diagnostics that identify underlying issues in
                  minutes instead of hours or days.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-orange-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    Context-Aware Resolutions:
                  </strong>{" "}
                  AI understands your specific SAP environment and delivers
                  tailored solutions.
                </span>
              </li>
              <li className="flex items-start gap-3">
                <div className="w-2 h-2 bg-orange-500 rounded-full mt-2 flex-shrink-0"></div>
                <span>
                  <strong className="text-black">
                    ABAP Code Auto-Generation:
                  </strong>{" "}
                  Produces production-ready code with comprehensive error
                  handling and optimization.
                </span>
              </li>
            </ul>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            The 70% Productivity Transformation
          </h2>

          <p>
            The 70% productivity increase isn't just a marketing claim—it's the
            real-world result experienced by organizations implementing G-RISE.
            This dramatic improvement comes from multiple sources: automated
            test script generation that eliminates weeks of manual coding,
            intelligent ticket triage that instantly directs issues to the right
            resolution path, and self-service capabilities that empower users to
            solve common problems without support intervention.
          </p>

          <p>
            Traditional SAP testing and support requires significant manual
            effort—writing test scripts, analyzing logs, debugging code, and
            documenting resolutions. G-RISE automates these time-consuming
            tasks, allowing teams to focus on strategic initiatives rather than
            repetitive maintenance work.
          </p>

          <div className="bg-gradient-to-br from-orange-50 to-yellow-50 border border-orange-200 rounded-xl p-6">
            <h3 className="text-xl font-bold text-black mb-6">
              Impact Analysis
            </h3>
            <div className="grid md:grid-cols-2 gap-6">
              <div>
                <div className="text-4xl font-bold text-orange-600 mb-2">
                  70%
                </div>
                <p className="text-gray-700">Overall productivity increase</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-yellow-600 mb-2">
                  90%
                </div>
                <p className="text-gray-700">
                  Reduction in manual testing effort
                </p>
              </div>
              <div>
                <div className="text-4xl font-bold text-orange-600 mb-2">
                  50%
                </div>
                <p className="text-gray-700">Fewer resource dependencies</p>
              </div>
              <div>
                <div className="text-4xl font-bold text-yellow-600 mb-2">
                  10x
                </div>
                <p className="text-gray-700">
                  Faster root cause identification
                </p>
              </div>
            </div>
          </div>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Intelligent ABAP Code Generation
          </h2>

          <p>
            One of G-RISE's most powerful features is its ability to generate
            production-quality ABAP code automatically. The AI understands SAP
            best practices, coding standards, and performance optimization
            techniques. When generating code, it considers your specific SAP
            version, custom configurations, and business logic requirements.
          </p>

          <p>
            The generated code includes comprehensive error handling, follows
            naming conventions, includes inline documentation, and is optimized
            for performance. What might take a developer hours or days to write
            and test, G-RISE produces in minutes—with higher consistency and
            fewer bugs.
          </p>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Real-Time Context-Aware Support
          </h2>

          <p>
            G-RISE doesn't just provide generic solutions—it understands
            context. The system analyzes your SAP environment, recent changes,
            user roles, transaction history, and system configuration to deliver
            resolutions specifically tailored to your situation. This
            context-awareness dramatically improves first-time fix rates and
            reduces back-and-forth troubleshooting.
          </p>

          <blockquote className="border-l-4 border-orange-500 pl-6 italic text-gray-800 my-8 py-4 bg-gray-50">
            "Before G-RISE, our team spent 60% of their time on repetitive
            testing and troubleshooting tasks. Now, we've redirected that effort
            toward innovation and strategic projects. The 70% productivity gain
            is real, measurable, and transformative."
            <footer className="text-gray-600 mt-2 text-sm not-italic">
              — VP of Technology, Global Manufacturing Enterprise
            </footer>
          </blockquote>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            Reducing Resource Dependencies
          </h2>

          <p>
            A major challenge in SAP environments is dependency on specialized
            resources. When key experts are unavailable, work stalls. G-RISE
            democratizes expertise by capturing and codifying the knowledge of
            your best engineers. Junior team members can leverage G-RISE to
            solve problems that previously required senior experts, reducing
            bottlenecks and improving team resilience.
          </p>

          <p>
            This knowledge democratization also accelerates onboarding. New team
            members become productive faster when they have G-RISE as a mentor,
            guiding them through complex SAP scenarios and teaching best
            practices through real-world examples.
          </p>

          <h2 className="text-3xl font-bold text-black mt-12 mb-6">
            The Future of SAP Support
          </h2>

          <p>
            G-RISE represents the future of enterprise application
            support—intelligent, automated, and continuously improving. As the
            system processes more tickets, generates more code, and analyzes
            more scenarios, it becomes increasingly accurate and efficient.
            Organizations implementing G-RISE aren't just adopting a tool;
            they're embarking on a journey of continuous productivity
            improvement.
          </p>

          <div className="border-t border-gray-200 my-8"></div>

          <div className="flex items-center gap-4 pt-6">
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Share2 className="w-4 h-4" />
              Share Article
            </button>
            <button className="flex items-center gap-2 px-4 py-2 border border-gray-300 text-black rounded-lg hover:bg-gray-50 transition-colors">
              <Bookmark className="w-4 h-4" />
              Save for Later
            </button>
          </div>
        </div>
      </div>
    </div>
  );

  // Home View
  if (currentView === "home") {
    return (
      <div className="min-h-screen bg-white text-black">
        {/* Hero Section with G-RISE Introduction */}
        <div className="relative overflow-hidden border-b border-gray-200">
          <div className="absolute inset-0 bg-gradient-to-br from-gray-50 via-white to-white opacity-90"></div>

          {/* Animated Background Elements */}
          <div className="absolute top-20 left-10 w-64 h-64 bg-gradient-to-br from-purple-200/20 to-transparent rounded-full blur-3xl animate-pulse"></div>
          <div
            className="absolute bottom-20 right-10 w-80 h-80 bg-gradient-to-br from-blue-200/20 to-transparent rounded-full blur-3xl animate-pulse"
            style={{ animationDelay: "1s" }}
          ></div>

          <div className="relative max-w-7xl mx-auto px-6 py-16 md:py-24">
            {/* Logo/Brand Area */}
            <div className="text-center mb-12 animate-fade-in">
              <div className="inline-flex items-center gap-2 md:gap-3 mb-6">
                {/* Left Sparkle */}
                <Sparkles className="w-5 h-5 text-purple-600 md:w-8 md:h-8" />

                <h1 className="text-4xl sm:text-5xl md:text-8xl font-bold tracking-tighter flex items-center gap-2 md:gap-4">
                  {/* Logo — smaller only on mobile */}
                  <img
                    src={logo}
                    alt="Logo"
                    className="w-12 h-12 sm:w-16 sm:h-16 md:w-20 md:h-20"
                  />

                  <span className="bg-gradient-to-r from-black via-purple-700 to-blue-700 bg-clip-text text-transparent">
                    G-RISE
                  </span>
                </h1>

                {/* Right Sparkle */}
                <Sparkles className="w-5 h-5 text-blue-600 md:w-8 md:h-8" />
              </div>

              <p className="text-lg sm:text-xl md:text-2xl text-gray-600 mb-3 px-2">
                AI-Powered SAP Application Support
              </p>
            </div>

            {/* Value Proposition */}
            <div
              className="max-w-4xl mx-auto space-y-8 animate-fade-in"
              style={{ animationDelay: "0.3s" }}
            >
              <p className="text-lg md:text-xl text-gray-700 leading-relaxed text-center">
                G-RISE transforms SAP application support by{" "}
                <span className="text-black font-semibold">
                  automating ticket triage
                </span>
                ,
                <span className="text-black font-semibold">
                  {" "}
                  analyzing root causes
                </span>
                , and delivering
                <span className="text-black font-semibold">
                  {" "}
                  real-time, context-aware resolutions
                </span>
                —powered by AI-driven test script automation and ABAP code
                generation.
              </p>

              {/* Key Benefits */}
              <div className="grid md:grid-cols-3 gap-6 mt-12">
                <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-6 hover:border-purple-300 hover:shadow-lg transition-all duration-300">
                  <div className="text-5xl font-bold bg-gradient-to-r from-purple-600 to-pink-600 bg-clip-text text-transparent mb-2">
                    70%
                  </div>
                  <div className="text-gray-600 text-sm">
                    Productivity Increase
                  </div>
                </div>

                <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-6 hover:border-blue-300 hover:shadow-lg transition-all duration-300">
                  <Code className="w-10 h-10 text-blue-600 mb-3" />
                  <div className="text-gray-600 text-sm">
                    Automated Code Generation
                  </div>
                </div>

                <div className="bg-gradient-to-br from-gray-50 to-white border border-gray-200 rounded-xl p-6 hover:border-orange-300 hover:shadow-lg transition-all duration-300">
                  <Clock className="w-10 h-10 text-orange-600 mb-3" />
                  <div className="text-gray-600 text-sm">
                    Real-Time Resolutions
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Latest News & Updates Section */}
        <div className="max-w-7xl mx-auto px-6 py-16">
          <div className="mb-12">
            <div className="flex items-center gap-3 mb-3">
              <div className="w-2 h-2 bg-red-500 rounded-full animate-pulse"></div>
              <h2 className="text-4xl md:text-5xl font-bold text-black">
                Latest News & Updates
              </h2>
            </div>
            <p className="text-gray-600 text-lg">
              Stay ahead with cutting-edge innovations and industry insights
            </p>
            <div className="h-1 w-32 bg-gradient-to-r from-purple-500 via-blue-500 to-orange-500 mt-4"></div>
          </div>

          {/* News Cards Grid */}
          <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-8">
            {newsTopics.map((topic, index) => (
              <div
                key={topic.id}
                className="group relative bg-white border-2 border-gray-200 rounded-2xl overflow-hidden transition-all duration-500 hover:scale-[1.02] hover:shadow-2xl hover:shadow-gray-300/50 cursor-pointer"
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
                <div className="relative h-56 overflow-hidden">
                  <img
                    src={topic.image}
                    alt={topic.title}
                    className="w-full h-full object-cover transition-transform duration-700 group-hover:scale-110"
                  />
                  <div
                    className={`absolute inset-0 bg-gradient-to-t ${topic.gradient} opacity-50 group-hover:opacity-70 transition-opacity duration-500`}
                  ></div>

                  {/* Tag Badge */}
                  <div className="absolute top-4 left-4 px-3 py-1 rounded-full bg-white/90 backdrop-blur-sm border border-gray-200 text-black text-xs font-semibold uppercase tracking-wider">
                    {topic.tag}
                  </div>

                  {/* Icon Badge */}
                  <div
                    className={`absolute top-4 right-4 p-3 rounded-full bg-white/90 backdrop-blur-sm border border-gray-200 text-black transition-all duration-500 ${
                      hoveredCard === topic.id ? "scale-110 rotate-12" : ""
                    }`}
                  >
                    {topic.icon}
                  </div>

                  {/* Hover Overlay */}
                  <div
                    className={`absolute inset-0 bg-black/10 backdrop-blur-[2px] transition-opacity duration-300 ${
                      hoveredCard === topic.id ? "opacity-100" : "opacity-0"
                    }`}
                  ></div>
                </div>

                {/* Content */}
                <div className="p-6 space-y-4">
                  <h3 className="text-2xl font-bold tracking-tight leading-tight text-black group-hover:text-transparent group-hover:bg-gradient-to-r group-hover:from-black group-hover:to-gray-600 group-hover:bg-clip-text transition-all duration-300">
                    {topic.title}
                  </h3>

                  <p className="text-gray-600 leading-relaxed text-sm line-clamp-3">
                    {topic.description}
                  </p>

                  <div className="flex items-center gap-4 text-xs text-gray-500">
                    <span>{topic.readTime}</span>
                    <span>•</span>
                    <span>{topic.date}</span>
                  </div>

                  {/* Action Button */}
                  <button className="flex items-center gap-2 text-black font-medium group/btn mt-4 hover:gap-4 transition-all duration-300">
                    <span className="text-sm">Read Full Article</span>
                    <ChevronRight className="w-4 h-4 transition-transform duration-300 group-hover/btn:translate-x-1" />
                  </button>
                </div>

                {/* Gradient Border Effect on Hover */}
                <div
                  className={`absolute inset-0 rounded-2xl bg-gradient-to-r ${topic.gradient} opacity-0 group-hover:opacity-10 transition-opacity duration-500 -z-10`}
                ></div>
              </div>
            ))}
          </div>

          {/* Call to Action */}
          <div className="mt-20 text-center">
            <div className="inline-block bg-gradient-to-br from-gray-50 to-white border-2 border-gray-200 rounded-2xl p-10 hover:border-gray-300 hover:shadow-xl transition-all duration-300">
              <h3 className="text-3xl font-bold mb-4 text-black">
                Ready to Transform Your SAP Support?
              </h3>
              <p className="text-gray-600 mb-6 max-w-2xl">
                Join industry leaders who've boosted productivity by 70% with
                G-RISE's intelligent automation
              </p>
              <Link to="/how-to">
                <button className="px-8 py-4 bg-black text-white font-semibold rounded-lg hover:bg-gray-800 transition-all duration-300 hover:scale-105">
                  Get Started Today
                </button>
              </Link>
            </div>
          </div>
        </div>

        <style jsx>{`
          @keyframes fade-in {
            from {
              opacity: 0;
              transform: translateY(20px);
            }
            to {
              opacity: 1;
              transform: translateY(0);
            }
          }

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

          .animate-fade-in {
            animation: fade-in 0.8s ease-out;
          }

          .line-clamp-3 {
            display: -webkit-box;
            -webkit-line-clamp: 3;
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
