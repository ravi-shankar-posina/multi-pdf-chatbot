import React from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom"; 
import Layout from "./pages/Layout"; 
import Chat from "./pages/Chat"; 
import Accessmanagement from "./pages/Accessmanagement";
import  SapTestCase  from "./pages/SapTestCase";
import TestScriptGenerator from "./pages/TestScriptGenerator";
import IncidentAnalysis from "./pages/IncidentAnalysis";

function App() {
  return (
    <Router>
      <Routes>
        <Route element={<Layout />}>
          <Route path="/" element={<Chat key="csv/query" api="csv/query" />} />
          <Route path="/incident" element={<Chat key="analyze" api="analyze" />} />
          <Route path="/best-practices" element={<Chat key="pdf/query" api="pdf/query" />} />
          <Route path="/abap-code-generator" element={<Chat key="query" api="query" />} />
          <Route path="/access-management" element={<Accessmanagement />} />
          <Route path="/sap-test-case-genarator" element={<SapTestCase />} />
          <Route path="/test-script-genarator" element={<TestScriptGenerator />}  />
          <Route path= "/incident-analysis" element={<IncidentAnalysis />} />
        </Route>
      </Routes>
    </Router>
  );
}

export default App;
