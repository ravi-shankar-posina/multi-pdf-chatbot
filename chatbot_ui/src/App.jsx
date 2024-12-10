import React, { useState, useEffect } from "react";
import {
  BrowserRouter as Router,
  Routes,
  Route,
  Navigate,
} from "react-router-dom";
import Layout from "./pages/Layout";
import Chat from "./pages/Chat";
import Accessmanagement from "./pages/Accessmanagement";
import SapTestCase from "./pages/SapTestCase";
import TestScriptGenerator from "./pages/TestScriptGenerator";
import IncidentAnalysis from "./pages/IncidentAnalysis";
import LoginPage from "./pages/LoginPage";

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
              element={<Chat key="query" api="query" />}
            />
            <Route path="/access-management" element={<Accessmanagement />} />
            <Route path="/sap-test-case-genarator" element={<SapTestCase />} />
            <Route
              path="/test-script-genarator"
              element={<TestScriptGenerator />}
            />
            <Route path="/incident-analysis" element={<IncidentAnalysis />} />
          </Route>
        ) : (
          <Route path="*" element={<Navigate to="/login" replace />} />
        )}
      </Routes>
    </Router>
  );
}

export default App;
