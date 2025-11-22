import React, { useEffect, useState } from "react";
import {
  Navigate,
  Route,
  BrowserRouter as Router,
  Routes,
} from "react-router-dom";
import SmartConn from "./components/SmartConn";
import SopCreation from "./components/SopCreation";
import Accessmanagement from "./pages/Accessmanagement";
import Chat from "./pages/Chat";
import IncidentAnalysis from "./pages/IncidentAnalysis";
import Layout from "./pages/Layout";
import LoginPage from "./pages/LoginPage";
import SapTestCase from "./pages/SapTestCase";
import TestScriptGenerator from "./pages/TestScriptGenerator";
import GriseAgent from "./components/GriseAgent";
import SupportAgent from "./components/SupportAgent";
import PasswordAgent from "./components/PasswordAgent";
import Coa from "./components/Coa";
import PoAutomation from "./components/PoAutomation";
import ApSuite from "./components/ApSuite";
import Home from "./pages/Home";
import BRDToABAPGenerator from "./pages/BRDToABAPGenerator";

// Same user data as in LoginPage
const USERS = [
  { username: "admin", password: "grise2025!", token: "admin-token" },
  { username: "703055690", password: "Password@2025", token: "user-token" },
  { username: "703070518", password: "Password@2025", token: "user-token" },
  { username: "302009439", password: "Password@2025", token: "user-token" },
];

// Validate token against current user credentials
const validateToken = (token) => {
  try {
    const decoded = atob(token);
    const [username, password] = decoded.split(':');

    // Check if user exists with current password
    const user = USERS.find(u => u.username === username && u.password === password);
    return !!user;
  } catch (error) {
    return false;
  }
};

function App() {
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    // Check if token exists and is valid
    const token = localStorage.getItem("authToken");

    if (token) {
      const isValidToken = validateToken(token);
      if (isValidToken) {
        setIsAuthenticated(true);
      } else {
        // Token is invalid, clear storage
        localStorage.removeItem("authToken");
        localStorage.removeItem("username");
        setIsAuthenticated(false);
      }
    } else {
      setIsAuthenticated(false);
    }

    setIsLoading(false);
  }, []);

  // Also check token validity periodically (every 5 minutes)
  useEffect(() => {
    if (isAuthenticated) {
      const interval = setInterval(() => {
        const token = localStorage.getItem("authToken");
        if (token && !validateToken(token)) {
          handleLogout();
        }
      }, 60 * 60 * 1000); // Check every 5 minutes

      return () => clearInterval(interval);
    }
  }, [isAuthenticated]);

  const handleLogin = () => {
    setIsAuthenticated(true);
  };

  const handleLogout = () => {
    // Remove token and reset authentication
    localStorage.removeItem("authToken");
    localStorage.removeItem("username");
    setIsAuthenticated(false);
  };

  // Show loading while checking authentication
  if (isLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-lg">Loading...</div>
      </div>
    );
  }

  return (
    <Router>
      <Routes>
        {/* Login Page */}
        <Route
          path="/login"
          element={
            isAuthenticated ? (
              <Navigate to="/" replace />
            ) : (
              <LoginPage onLogin={handleLogin} />
            )
          }
        />

        {/* Secured Routes */}
        {isAuthenticated ? (
          <Route element={<Layout onLogout={handleLogout} />}>
            <Route path="/" element={<Home />} />
            <Route
              path="/how-to"
              element={<Chat key="csv/query" api="csv/query" />}
            />
            <Route
              path="/incident-query"
              element={<Chat key="analyze" api="analyze" />}
            />
            <Route
              path="/best-practices"
              element={<Chat key="pdf/query" api="pdf/query" />}
            />
            <Route
              path="/codexpert"
              element={<BRDToABAPGenerator />}
            />
            <Route path="/password-management" element={<Accessmanagement />} />
            <Route path="/sap-test-case-genarator" element={<SapTestCase />} />
            <Route
              path="/scriptxpert"
              element={<TestScriptGenerator />}
            />
            <Route path="/incident-analysis" element={<IncidentAnalysis />} />
            <Route path="/sop-creation" element={<SopCreation />} />
            <Route path="/kt-documents" element={<Chat key="kt/query" api="kt/query" />} />
            <Route path="/smart-connectors" element={<SmartConn />} />
            <Route path="/grise-agent" element={<GriseAgent />} />
            <Route path="/idoc-agent" element={<SupportAgent />} />
            <Route path="/pwd-agent" element={<PasswordAgent />} />
            <Route path="/coa" element={<Coa />} />
            <Route path="/poautomation" element={<PoAutomation />} />
            <Route path="/apsuite" element={<ApSuite />} />
          </Route>
        ) : (
          <Route path="*" element={<Navigate to="/login" replace />} />
        )}
      </Routes>
    </Router>
  );
}

export default App;