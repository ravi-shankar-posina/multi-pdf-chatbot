import { useState } from "react";
import { IoCloudUploadOutline } from "react-icons/io5";
import { BiErrorCircle } from "react-icons/bi";
import axios from "axios";

// Custom Alert Component
const Alert = ({ children, variant = "error" }) => {
  const bgColor = variant === "error" ? "bg-red-100" : "bg-blue-100";
  const textColor = variant === "error" ? "text-red-800" : "text-blue-800";
  const borderColor =
    variant === "error" ? "border-red-400" : "border-blue-400";

  return (
    <div
      className={`${bgColor} ${textColor} ${borderColor} border rounded-lg p-4 flex items-start space-x-2`}
    >
      {children}
    </div>
  );
};

const TestScriptGenerator = () => {
  const [selectedFile, setSelectedFile] = useState(null);
  const [uploadType, setUploadType] = useState("single");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const handleFileSelect = (event) => {
    const file = event.target.files[0];
    if (file) {
      if (uploadType === "zip" && !file.name.endsWith(".zip")) {
        setError("Please select a ZIP file");
        return;
      }
      if (uploadType === "single" && !file.name.match(/\.(pdf|txt|docx)$/i)) {
        setError("Please select a PDF, TXT, or DOCX file");
        return;
      }
      setSelectedFile(file);
      setError(null);
    }
  };

  const handleUpload = async () => {
    if (!selectedFile) {
      setError("Please select a file first");
      return;
    }

    setLoading(true);
    setError(null);

    const formData = new FormData();
    formData.append("file", selectedFile);

    try {
      const endpoint =
        uploadType === "zip"
          ? `${import.meta.env.VITE_API_URL}/upload-zip`
          : `${import.meta.env.VITE_API_URL}/upload-file`;

      // Set timeout to 5 minutes (300,000 milliseconds)
      const response = await axios.post(endpoint, formData, {
        timeout: 5 * 60 * 1000, // 5 minutes
        headers: { "Content-Type": "multipart/form-data" },
      });

      if (uploadType === "single") {
        // Handle single file download
        const url = window.URL.createObjectURL(new Blob([response.data]));
        const a = document.createElement("a");
        a.href = url;
        a.download = selectedFile.name.replace(/\.[^/.]+$/, ".xlsx");
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
      } else {
        // Handle zip file response
        console.log("Processed files:", response.data.results);
      }
    } catch (err) {
      if (err.code === "ECONNABORTED") {
        setError("Upload timed out");
      } else {
        setError(err.message);
      }
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen bg-gray-100 py-8">
      <div className="max-w-2xl mx-auto bg-white p-8 rounded-lg shadow">
        <h1 className="text-2xl font-bold mb-6">Test Script Generator</h1>

        <div className="space-y-6">
          <div className="flex space-x-4">
            <button
              className={`px-4 py-2 rounded transition-colors duration-200 ${
                uploadType === "single"
                  ? "bg-blue-600 text-white"
                  : "bg-gray-200 text-gray-700 hover:bg-gray-300"
              }`}
              onClick={() => setUploadType("single")}
            >
              Single File
            </button>
            <button
              className={`px-4 py-2 rounded transition-colors duration-200 ${
                uploadType === "zip"
                  ? "bg-blue-600 text-white"
                  : "bg-gray-200 text-gray-700 hover:bg-gray-300"
              }`}
              onClick={() => setUploadType("zip")}
            >
              ZIP File
            </button>
          </div>

          <div className="border-2 border-dashed border-gray-300 rounded-lg p-6 hover:border-blue-400 transition-colors duration-200">
            <div className="flex flex-col items-center">
              <IoCloudUploadOutline className="w-12 h-12 text-gray-400 mb-4" />
              <label className="block w-full">
                <span className="sr-only">Choose file</span>
                <input
                  type="file"
                  className="block w-full text-sm text-gray-500
                    file:mr-4 file:py-2 file:px-4
                    file:rounded-md file:border-0
                    file:text-sm file:font-semibold
                    file:bg-blue-50 file:text-blue-700
                    hover:file:bg-blue-100
                    cursor-pointer"
                  accept={uploadType === "zip" ? ".zip" : ".pdf,.txt,.docx"}
                  onChange={handleFileSelect}
                />
              </label>
              <p className="mt-2 text-sm text-gray-500">
                {selectedFile
                  ? `Selected: ${selectedFile.name}`
                  : uploadType === "zip"
                  ? "Upload a ZIP file containing your documents"
                  : "Upload PDF, TXT, or DOCX files"}
              </p>
            </div>
          </div>

          {error && (
            <Alert>
              <BiErrorCircle className="w-5 h-5 text-red-500 mt-0.5" />
              <span>{error}</span>
            </Alert>
          )}

          <button
            className={`w-full py-2 px-4 rounded transition-colors duration-200 ${
              loading || !selectedFile
                ? "bg-gray-400 cursor-not-allowed"
                : "bg-blue-600 hover:bg-blue-700"
            } text-white font-medium`}
            onClick={handleUpload}
            disabled={loading || !selectedFile}
          >
            {loading ? (
              <div className="flex items-center justify-center">
                <div className="w-5 h-5 border-t-2 border-white rounded-full animate-spin mr-2" />
                Processing...
              </div>
            ) : (
              "Generate Test Scripts"
            )}
          </button>
        </div>
      </div>
    </div>
  );
};

export default TestScriptGenerator;
