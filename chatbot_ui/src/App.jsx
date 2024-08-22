import { useState } from "react";
import "./App.css";
import Chat from "./Chat";
function App() {
  const [count, setCount] = useState(0);

  return <>
  <Chat/>
  </>;
}

export default App;



// import React from 'react';
// import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
// import Layout from './components/Layout';
// import SupportHelp from './components/SupportHelp';
// import PdfReader from './components/PdfReader';
// import AbapCodeGenerator from './components/AbapCodeGenerator';

// const App = () => {
//   return (
//     <Router>
//       <Layout>
//         <Routes>
//           {/* <Route path="/support-help" element={<SupportHelp />} /> */}
//           <Route path="/pdf-reader" element={<PdfReader />} />
//           <Route path="/abap-code-generator" element={<AbapCodeGenerator />} />
//           <Route path="/" element={<SupportHelp />} />
//         </Routes>
//       </Layout>
//     </Router>
//   );
// };

// export default App;