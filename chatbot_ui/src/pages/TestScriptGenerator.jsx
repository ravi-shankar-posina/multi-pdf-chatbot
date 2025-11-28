// // // TestScriptGenerator.jsx
// import React from "react";

// const TestScriptGenerator = () => {
//   return (
//     <div className="flex-1 h-full">
//       <iframe
//         src={import.meta.env.VITE_TEST_SCRIPT_URL}
//         title="SAP Chat Application"
//         width="100%"
//         height="100%"
//         style={{ border: "none" }}
//       />
//     </div>
//   );
// };
// export default TestScriptGenerator;


import React, { useState, useRef } from 'react';
import { FileText, Code, Download, Loader2, CheckCircle, AlertCircle, BookOpen, GitBranch, X, Send, FileSpreadsheet } from 'lucide-react';

const TestScriptGenerator = () => {
  const [file, setFile] = useState(null);
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState(null);
  const [error, setError] = useState(null);
  const [activeTab, setActiveTab] = useState('scripts');
  const [currentStep, setCurrentStep] = useState(0);
  const [chatQuery, setChatQuery] = useState('');
  const [chatResponse, setChatResponse] = useState('');
  const [chatLoading, setChatLoading] = useState(false);
  const fileInputRef = useRef(null);

  const API_URL = import.meta.env?.VITE_API_URL || 'http://localhost:8502';
  const API_TIMEOUT = 60 * 60 * 1000; // 60 minutes in milliseconds

  const steps = [
    { label: 'Extracting Text', icon: FileText },
    { label: 'Generating Summary', icon: BookOpen },
    { label: 'Building Knowledge Graph', icon: GitBranch },
    { label: 'Generating Test Scripts', icon: Code },
  ];

  const handleFileChange = (e) => {
    const selectedFile = e.target.files[0];
    if (selectedFile) {
      const ext = selectedFile.name.split('.').pop().toLowerCase();
      if (['pdf', 'docx'].includes(ext)) {
        setFile(selectedFile);
        setError(null);
        setResult(null);
        setChatResponse('');
      } else {
        setError('Only PDF and DOCX files are supported');
        setFile(null);
      }
    }
  };

  const removeFile = () => {
    setFile(null);
    setResult(null);
    setChatResponse('');
    if (fileInputRef.current) fileInputRef.current.value = '';
  };

  const fetchWithTimeout = (url, options, timeout) => {
    return Promise.race([
      fetch(url, options),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error('Request timeout after 60 minutes')), timeout)
      )
    ]);
  };

  const handleGenerate = async () => {
    if (!file) return;

    setLoading(true);
    setError(null);
    setResult(null);
    setCurrentStep(0);

    const formData = new FormData();
    formData.append('file', file);

    const stepInterval = setInterval(() => {
      setCurrentStep((prev) => (prev < 3 ? prev + 1 : prev));
    }, 4000);

    try {
      const response = await fetchWithTimeout(
        `${API_URL}/test-scripts/process`,
        {
          method: 'POST',
          body: formData,
        },
        API_TIMEOUT
      );

      clearInterval(stepInterval);
      setCurrentStep(4);

      const data = await response.json();

      if (data.success) {
        setResult(data.data);
        setActiveTab('scripts');
      } else {
        setError(data.error || 'Processing failed');
      }
    } catch (err) {
      clearInterval(stepInterval);
      setError(`Failed to connect to server: ${err.message}`);
    } finally {
      setLoading(false);
    }
  };

  const handleChatSubmit = async () => {
    if (!chatQuery.trim()) return;

    setChatLoading(true);
    setChatResponse('');

    try {
      const response = await fetchWithTimeout(
        `${API_URL}/chat`,
        {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ query: chatQuery }),
        },
        API_TIMEOUT
      );

      const data = await response.json();
      setChatResponse(data.answer || 'No response received');
    } catch (err) {
      setChatResponse(`Error: ${err.message}`);
    } finally {
      setChatLoading(false);
    }
  };

  const handleKeyPress = (e) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleChatSubmit();
    }
  };

  const handleDownload = (content, filename) => {
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
  };

  const handleExcelDownload = () => {
    if (result?.files?.test_xlsx) {
      window.open(`${API_URL}/test-scripts/download/xlsx/${result.files.test_xlsx}`, '_blank');
    }
  };

  const formatFileSize = (bytes) => {
    if (bytes < 1024) return bytes + ' B';
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
    return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
  };

  return (
    <div className="h-full bg-white p-6">
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-semibold text-gray-900 mb-1">Test Script Expert</h1>
          <p className="text-gray-500 text-sm">
            Transform your business requirement documents into SAP test scripts
          </p>
        </div>

        {/* Upload Section */}
        <div className="mb-6">
          <label className="block text-sm font-medium text-gray-700 mb-2">
            Upload Document
          </label>

          {!file ? (
            <input
              ref={fileInputRef}
              type="file"
              accept=".pdf,.docx"
              onChange={handleFileChange}
              className="block w-full text-sm text-gray-500 file:mr-4 file:py-2 file:px-4 file:rounded-lg file:border-0 file:text-sm file:font-medium file:bg-blue-50 file:text-blue-700 hover:file:bg-blue-100 file:cursor-pointer cursor-pointer border border-gray-200 rounded-lg"
            />
          ) : (
            <div className="flex items-center justify-between p-3 border border-gray-200 rounded-lg bg-gray-50">
              <div className="flex items-center gap-3">
                <div className="w-8 h-8 rounded bg-blue-100 flex items-center justify-center">
                  <FileText className="w-4 h-4 text-blue-600" />
                </div>
                <div>
                  <p className="text-sm font-medium text-gray-900">{file.name}</p>
                  <p className="text-xs text-gray-500">{formatFileSize(file.size)}</p>
                </div>
              </div>
              {!loading && (
                <button onClick={removeFile} className="p-1 hover:bg-gray-200 rounded transition-colors">
                  <X className="w-4 h-4 text-gray-500" />
                </button>
              )}
            </div>
          )}

          {file && !loading && !result && (
            <button
              onClick={handleGenerate}
              className="mt-3 bg-blue-600 hover:bg-blue-700 text-white text-sm font-medium py-2 px-4 rounded-lg transition-colors flex items-center gap-2"
            >
              <Code className="w-4 h-4" />
              Generate Test Scripts
            </button>
          )}
        </div>

        {/* Progress Steps */}
        {loading && (
          <div className="mb-6 p-4 border border-gray-200 rounded-lg">
            <div className="flex items-center justify-between">
              {steps.map((step, idx) => (
                <React.Fragment key={idx}>
                  <div className="flex flex-col items-center">
                    <div className={`w-8 h-8 rounded-full flex items-center justify-center transition-all
                                            ${idx < currentStep ? 'bg-green-500' : idx === currentStep ? 'bg-blue-600' : 'bg-gray-200'}`}>
                      {idx < currentStep ? (
                        <CheckCircle className="w-4 h-4 text-white" />
                      ) : idx === currentStep ? (
                        <Loader2 className="w-4 h-4 text-white animate-spin" />
                      ) : (
                        <step.icon className="w-4 h-4 text-gray-400" />
                      )}
                    </div>
                    <span className={`text-xs mt-2 text-center font-medium 
                                            ${idx < currentStep ? 'text-green-600' : idx === currentStep ? 'text-blue-600' : 'text-gray-400'}`}>
                      {step.label}
                    </span>
                  </div>
                  {idx < steps.length - 1 && (
                    <div className={`flex-1 h-0.5 mx-2 rounded ${idx < currentStep ? 'bg-green-500' : 'bg-gray-200'}`} />
                  )}
                </React.Fragment>
              ))}
            </div>
          </div>
        )}

        {/* Error Display */}
        {error && (
          <div className="mb-6 bg-red-50 border border-red-100 rounded-lg p-3 flex items-start gap-3">
            <AlertCircle className="w-5 h-5 text-red-500 flex-shrink-0" />
            <p className="text-sm text-red-700">{error}</p>
          </div>
        )}

        {/* Results Section */}
        {result && (
          <div className="mb-6 border border-gray-200 rounded-lg overflow-hidden">
            <div className="bg-green-50 border-b border-green-100 px-4 py-2 flex items-center gap-2">
              <CheckCircle className="w-4 h-4 text-green-600" />
              <span className="text-sm text-green-700 font-medium">Generation Complete</span>
            </div>

            {/* Tabs */}
            <div className="flex border-b border-gray-200 bg-gray-50">
              {[
                { id: 'scripts', label: 'Test Scripts', icon: Code },
                { id: 'summary', label: 'Summary', icon: FileText },
              ].map((tab) => (
                <button
                  key={tab.id}
                  onClick={() => setActiveTab(tab.id)}
                  className={`flex items-center gap-2 py-2.5 px-4 text-sm font-medium transition-all border-b-2 -mb-px
                                        ${activeTab === tab.id
                      ? 'border-blue-600 text-blue-600 bg-white'
                      : 'border-transparent text-gray-500 hover:text-gray-700'}`}
                >
                  <tab.icon className="w-4 h-4" />
                  {tab.label}
                </button>
              ))}
            </div>

            {/* Content */}
            <div className="p-4">
              <div className="bg-gray-900 rounded-lg p-4 max-h-64 overflow-auto">
                <pre className="text-sm text-gray-100 whitespace-pre-wrap font-mono leading-relaxed">
                  {activeTab === 'scripts' && result.test_scripts}
                  {activeTab === 'summary' && result.summary}
                </pre>
              </div>

              <div className="mt-3 flex items-center gap-3 flex-wrap">
                <button
                  onClick={() => {
                    const content = activeTab === 'scripts' ? result.test_scripts : result.summary;
                    const filename = activeTab === 'scripts' ? 'test_scripts.txt' : 'summary.txt';
                    handleDownload(content, filename);
                  }}
                  className="bg-gray-900 hover:bg-gray-800 text-white text-sm font-medium py-2 px-4 rounded-lg transition-colors flex items-center gap-2"
                >
                  <Download className="w-4 h-4" />
                  Download TXT
                </button>

                {result.files?.test_xlsx && (
                  <button
                    onClick={handleExcelDownload}
                    className="bg-green-600 hover:bg-green-700 text-white text-sm font-medium py-2 px-4 rounded-lg transition-colors flex items-center gap-2"
                  >
                    <FileSpreadsheet className="w-4 h-4" />
                    Download Excel
                  </button>
                )}

                <button
                  onClick={removeFile}
                  className="text-sm text-gray-500 hover:text-gray-700 font-medium ml-auto"
                >
                  Process another file
                </button>
              </div>
            </div>
          </div>
        )}

        {/* Chat Section */}
        {result && (
          <div className="border border-gray-200 rounded-lg p-4">
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Ask a Question
            </label>
            <div className="flex gap-2">
              <input
                type="text"
                value={chatQuery}
                onChange={(e) => setChatQuery(e.target.value)}
                onKeyPress={handleKeyPress}
                placeholder="Ask anything about the generated test scripts..."
                className="flex-1 px-3 py-2 border border-gray-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                disabled={chatLoading}
              />
              <button
                onClick={handleChatSubmit}
                disabled={chatLoading || !chatQuery.trim()}
                className="bg-blue-600 hover:bg-blue-700 disabled:bg-gray-300 text-white px-4 py-2 rounded-lg transition-colors flex items-center gap-2"
              >
                {chatLoading ? <Loader2 className="w-4 h-4 animate-spin" /> : <Send className="w-4 h-4" />}
              </button>
            </div>

            {chatResponse && (
              <div className="mt-3 p-3 bg-gray-50 rounded-lg border border-gray-200">
                <p className="text-sm text-gray-700 whitespace-pre-wrap">{chatResponse}</p>
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
};

export default TestScriptGenerator;