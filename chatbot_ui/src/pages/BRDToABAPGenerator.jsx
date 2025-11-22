import React, { useState } from 'react';
import { Upload, FileText, Code, Download, Loader2, CheckCircle, AlertCircle, BookOpen, GitBranch } from 'lucide-react';

const BRDToABAPGenerator = () => {
    const [file, setFile] = useState(null);
    const [loading, setLoading] = useState(false);
    const [result, setResult] = useState(null);
    const [error, setError] = useState(null);
    const [activeTab, setActiveTab] = useState('abap');
    const [currentStep, setCurrentStep] = useState(0);

    const API_URL = import.meta.env?.VITE_API_URL || 'http://localhost:8502';

    const steps = [
        { label: 'Extracting Text', icon: FileText },
        { label: 'Generating Summary', icon: BookOpen },
        { label: 'Building Knowledge Graph', icon: GitBranch },
        { label: 'Generating ABAP Code', icon: Code },
    ];

    const handleFileChange = (e) => {
        const selectedFile = e.target.files[0];
        if (selectedFile) {
            const ext = selectedFile.name.split('.').pop().toLowerCase();
            if (['pdf', 'docx'].includes(ext)) {
                setFile(selectedFile);
                setError(null);
                setResult(null);
            } else {
                setError('Only PDF and DOCX files are supported');
                setFile(null);
            }
        }
    };

    const handleDrop = (e) => {
        e.preventDefault();
        const droppedFile = e.dataTransfer.files[0];
        if (droppedFile) {
            const ext = droppedFile.name.split('.').pop().toLowerCase();
            if (['pdf', 'docx'].includes(ext)) {
                setFile(droppedFile);
                setError(null);
                setResult(null);
            } else {
                setError('Only PDF and DOCX files are supported');
            }
        }
    };

    const handleGenerate = async () => {
        if (!file) return;

        setLoading(true);
        setError(null);
        setResult(null);
        setCurrentStep(0);

        const formData = new FormData();
        formData.append('file', file);

        // Simulate step progression
        const stepInterval = setInterval(() => {
            setCurrentStep((prev) => (prev < 3 ? prev + 1 : prev));
        }, 3000);

        try {
            const response = await fetch(`${API_URL}/brd/process`, {
                method: 'POST',
                body: formData,
            });

            clearInterval(stepInterval);
            setCurrentStep(4);

            const data = await response.json();

            if (data.success) {
                setResult(data.data);
                setActiveTab('abap');
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

    const handleDownload = (content, filename) => {
        const blob = new Blob([content], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = filename;
        a.click();
        URL.revokeObjectURL(url);
    };

    return (
        <div className="min-h-screen bg-gradient-to-br from-slate-900 via-slate-800 to-slate-900 p-6">
            <div className="max-w-6xl mx-auto">
                {/* Header */}
                <div className="text-center mb-8">
                    <h1 className="text-3xl font-bold text-white mb-2 flex items-center justify-center gap-3">
                        <span className="text-4xl">📘</span>
                        BRD → ABAP Code Generator
                    </h1>
                    <p className="text-slate-400">
                        Upload a <span className="text-blue-400 font-medium">PDF or DOCX</span> and automatically generate{' '}
                        <span className="text-green-400 font-medium">ABAP code</span> using AI
                    </p>
                </div>

                {/* Upload Section */}
                <div className="bg-slate-800/50 backdrop-blur rounded-xl p-6 mb-6 border border-slate-700">
                    <div
                        onDrop={handleDrop}
                        onDragOver={(e) => e.preventDefault()}
                        className={`border-2 border-dashed rounded-lg p-8 text-center transition-all cursor-pointer
              ${file ? 'border-green-500 bg-green-500/10' : 'border-slate-600 hover:border-blue-500 hover:bg-slate-700/50'}`}
                    >
                        <input
                            type="file"
                            accept=".pdf,.docx"
                            onChange={handleFileChange}
                            className="hidden"
                            id="file-upload"
                        />
                        <label htmlFor="file-upload" className="cursor-pointer">
                            {file ? (
                                <div className="flex flex-col items-center gap-3">
                                    <CheckCircle className="w-12 h-12 text-green-500" />
                                    <div>
                                        <p className="text-green-400 font-medium">{file.name}</p>
                                        <p className="text-slate-500 text-sm">{(file.size / 1024).toFixed(1)} KB</p>
                                    </div>
                                </div>
                            ) : (
                                <div className="flex flex-col items-center gap-3">
                                    <Upload className="w-12 h-12 text-slate-500" />
                                    <div>
                                        <p className="text-slate-300 font-medium">Drop your BRD/PDD/FDD file here</p>
                                        <p className="text-slate-500 text-sm">or click to browse (PDF, DOCX)</p>
                                    </div>
                                </div>
                            )}
                        </label>
                    </div>

                    {file && !loading && (
                        <button
                            onClick={handleGenerate}
                            className="mt-4 w-full bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-500 hover:to-blue-600 text-white font-medium py-3 px-6 rounded-lg transition-all flex items-center justify-center gap-2"
                        >
                            <Code className="w-5 h-5" />
                            🚀 Generate ABAP Code
                        </button>
                    )}
                </div>

                {/* Progress Steps */}
                {loading && (
                    <div className="bg-slate-800/50 backdrop-blur rounded-xl p-6 mb-6 border border-slate-700">
                        <div className="flex items-center justify-between mb-4">
                            {steps.map((step, idx) => (
                                <div key={idx} className="flex flex-col items-center flex-1">
                                    <div className={`w-10 h-10 rounded-full flex items-center justify-center mb-2 transition-all
                    ${idx < currentStep ? 'bg-green-500' : idx === currentStep ? 'bg-blue-500 animate-pulse' : 'bg-slate-700'}`}>
                                        {idx < currentStep ? (
                                            <CheckCircle className="w-5 h-5 text-white" />
                                        ) : idx === currentStep ? (
                                            <Loader2 className="w-5 h-5 text-white animate-spin" />
                                        ) : (
                                            <step.icon className="w-5 h-5 text-slate-400" />
                                        )}
                                    </div>
                                    <span className={`text-xs text-center ${idx <= currentStep ? 'text-white' : 'text-slate-500'}`}>
                                        {step.label}
                                    </span>
                                    {idx < steps.length - 1 && (
                                        <div className={`absolute h-0.5 w-full top-5 left-1/2 -z-10 
                      ${idx < currentStep ? 'bg-green-500' : 'bg-slate-700'}`} />
                                    )}
                                </div>
                            ))}
                        </div>
                        <p className="text-center text-slate-400 text-sm">
                            Processing file → Summary → Knowledge Graph → ABAP Generation...
                        </p>
                    </div>
                )}

                {/* Error Display */}
                {error && (
                    <div className="bg-red-500/10 border border-red-500/50 rounded-xl p-4 mb-6 flex items-center gap-3">
                        <AlertCircle className="w-6 h-6 text-red-500 flex-shrink-0" />
                        <p className="text-red-400">{error}</p>
                    </div>
                )}

                {/* Results Section */}
                {result && (
                    <div className="bg-slate-800/50 backdrop-blur rounded-xl border border-slate-700 overflow-hidden">
                        {/* Success Banner */}
                        <div className="bg-green-500/20 border-b border-green-500/30 p-4 flex items-center gap-3">
                            <CheckCircle className="w-6 h-6 text-green-500" />
                            <span className="text-green-400 font-medium">🎉 ABAP Code Generated Successfully!</span>
                        </div>

                        {/* Tabs */}
                        <div className="flex border-b border-slate-700">
                            {[
                                { id: 'abap', label: '💻 ABAP Code', icon: Code },
                                { id: 'summary', label: '📝 Summary', icon: FileText },
                                { id: 'kg', label: '🔗 Knowledge Graph', icon: GitBranch },
                            ].map((tab) => (
                                <button
                                    key={tab.id}
                                    onClick={() => setActiveTab(tab.id)}
                                    className={`flex-1 py-3 px-4 text-sm font-medium transition-all flex items-center justify-center gap-2
                    ${activeTab === tab.id
                                            ? 'bg-slate-700 text-white border-b-2 border-blue-500'
                                            : 'text-slate-400 hover:text-white hover:bg-slate-700/50'}`}
                                >
                                    {tab.label}
                                </button>
                            ))}
                        </div>

                        {/* Content */}
                        <div className="p-4">
                            <div className="bg-slate-900 rounded-lg p-4 max-h-96 overflow-auto">
                                <pre className="text-sm text-slate-300 whitespace-pre-wrap font-mono">
                                    {activeTab === 'abap' && result.abap_code}
                                    {activeTab === 'summary' && result.summary}
                                    {activeTab === 'kg' && result.knowledge_graph}
                                </pre>
                            </div>

                            {/* Download Button */}
                            <button
                                onClick={() => {
                                    const content = activeTab === 'abap' ? result.abap_code
                                        : activeTab === 'summary' ? result.summary
                                            : result.knowledge_graph;
                                    const filename = activeTab === 'abap' ? 'generated_abap_code.txt'
                                        : activeTab === 'summary' ? 'document_summary.txt'
                                            : 'knowledge_graph.txt';
                                    handleDownload(content, filename);
                                }}
                                className="mt-4 bg-gradient-to-r from-green-600 to-green-700 hover:from-green-500 hover:to-green-600 text-white font-medium py-2 px-4 rounded-lg transition-all flex items-center gap-2"
                            >
                                <Download className="w-4 h-4" />
                                ⬇️ Download {activeTab === 'abap' ? 'ABAP Code' : activeTab === 'summary' ? 'Summary' : 'Knowledge Graph'}
                            </button>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};

export default BRDToABAPGenerator;