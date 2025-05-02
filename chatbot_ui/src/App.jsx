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
function App() {
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    // Check if token exists in local storage
    const token = localStorage.getItem("authToken");
    setIsAuthenticated(!!token);
  }, []);

  const handleLogin = (token) => {
    // Store token and set authentication
    localStorage.setItem("authToken", token);
    setIsAuthenticated(true);
  };

  const handleLogout = () => {
    // Remove token and reset authentication
    localStorage.removeItem("authToken");
    setIsAuthenticated(false);
  };

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
            <Route
              path="/"
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
              path="/abap-code-generator"
              element={<Chat key="modify-abap" api="query" />}
            />
            <Route path="/password-management" element={<Accessmanagement />} />
            <Route path="/sap-test-case-genarator" element={<SapTestCase />} />
            <Route
              path="/test-script-genarator"
              element={<TestScriptGenerator />}
            />
            <Route path="/incident-analysis" element={<IncidentAnalysis />} />
            <Route path="/sop-creation" element={<SopCreation />} />
            <Route path="/kt-documents" element={<Chat key="kt/query" api="kt/query" />} />
            <Route path="/smart-connectors" element={<SmartConn />} />
            <Route path="/grise-agent" element={<GriseAgent />} />
            <Route path="/idoc-agent" element={<SupportAgent />} />
          </Route>
        ) : (
          <Route path="*" element={<Navigate to="/login" replace />} />
        )}
      </Routes>
    </Router>
  );
}

export default App;
