import { useState } from "react";
import "./App.css";

import ChatbotInterface from "./ChatbotInterface";

function App() {
  const [count, setCount] = useState(0);

  return <>
  <ChatbotInterface/>
  </>;
}

export default App;
