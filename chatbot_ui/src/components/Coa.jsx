import React, { useState, useEffect } from "react";
import * as XLSX from "xlsx";
import { Loader } from "lucide-react";

const Coa = () => {
  const [tableData, setTableData] = useState([]);
  const [messages, setMessages] = useState([]);
  const [rightSteps, setRightSteps] = useState([]);
  const [processingStarted, setProcessingStarted] = useState(false);
  const [isLoading, setIsLoading] = useState(false);
  const [processComplete, setProcessComplete] = useState(false);

  const handleFileUpload = (e) => {
    const file = e.target.files[0];
    if (!file) return;

    setIsLoading(true);
    setMessages([]);
    setRightSteps([]);
    setProcessComplete(false);

    const reader = new FileReader();
    reader.onload = (evt) => {
      const data = new Uint8Array(evt.target.result);
      const workbook = XLSX.read(data, { type: "array" });
      const sheetName = workbook.SheetNames[0];
      const worksheet = workbook.Sheets[sheetName];
      const jsonData = XLSX.utils.sheet_to_json(worksheet, { header: 1 });
      const headers = jsonData[0];
      const rows = jsonData.slice(1).map((row) => {
        const obj = {};
        headers.forEach((header, index) => {
          obj[header] = row[index];
        });
        return obj;
      });

      setTableData(rows);

      // Automatically start processing after a short delay
      setTimeout(() => {
        setProcessingStarted(true);
      }, 1000);
    };
    reader.readAsArrayBuffer(file);
  };

  // Auto-scroll for messages and steps using scrollIntoView
  useEffect(() => {
    const messageElements = document.querySelectorAll(".message-item");
    if (messageElements.length > 0) {
      messageElements[messageElements.length - 1].scrollIntoView({
        behavior: "smooth",
        block: "end",
      });
    }
  }, [messages]);

  useEffect(() => {
    const stepElements = document.querySelectorAll(".step-item");
    if (stepElements.length > 0) {
      stepElements[stepElements.length - 1].scrollIntoView({
        behavior: "smooth",
        block: "end",
      });
    }
  }, [rightSteps]);

  useEffect(() => {
    if (!processingStarted) return;

    const timeoutSequence = [
      { delay: 2000, msg: "Message: Data validation completed successfully" },
      { delay: 2000, msg: "Simulation is in progress" },
    ];

    const rightMessages = [
      "Transfer line items from Source GLs to intermediary GL - Completed",
      "Perform Clearing in Source GL - Completed",
      "Block Source GL for further postings - Completed",
      "Change GL master data settings - Completed",
      "Unblock the source GLs - Completed",
      "Transfer line items from intermediary GLs - Completed",
      "Confirm the balance in Source GL & intermediary account - Completed",
    ];

    let totalDelay = 0;
    const timers = [];

    timeoutSequence.forEach(({ delay, msg }) => {
      totalDelay += delay;
      timers.push(
        setTimeout(() => setMessages((prev) => [...prev, msg]), totalDelay)
      );
    });

    rightMessages.forEach((step, i) => {
      const extraDelay = i === rightMessages.length - 1 ? 10000 : 5000;
      totalDelay += extraDelay;
      timers.push(
        setTimeout(() => setRightSteps((prev) => [...prev, step]), totalDelay)
      );
    });

    // Add "Process Successful" message after all right steps are completed
    totalDelay += 5000;
    timers.push(
      setTimeout(() => {
        setMessages((prev) => [...prev, "Process Successful"]);
        setIsLoading(false);
        setProcessComplete(true);
      }, totalDelay)
    );

    return () => timers.forEach(clearTimeout);
  }, [processingStarted]);

  return (
    <div className="h-full flex flex-col bg-white">
      <div className="flex flex-1 p-4 gap-4 overflow-hidden">
        <div className="w-2/3 flex flex-col bg-white shadow rounded-lg overflow-hidden border border-gray-300">
          <div className="p-3 bg-white border-b border-gray-300 font-medium text-gray-900 flex justify-between items-center">
            <span>Data Preview & Processing Status</span>
            {isLoading && !processComplete && (
              <div className="flex items-center text-gray-900">
                <Loader className="animate-spin mr-2" size={16} />
                <span className="text-sm">Processing...</span>
              </div>
            )}
            {processComplete && (
              <div className="text-green-600 font-semibold flex items-center">
                <svg
                  className="w-5 h-5 mr-1"
                  fill="currentColor"
                  viewBox="0 0 20 20"
                  xmlns="http://www.w3.org/2000/svg"
                >
                  <path
                    fillRule="evenodd"
                    d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
                    clipRule="evenodd"
                  />
                </svg>
                Complete
              </div>
            )}
          </div>

          <div
            className="flex-1 overflow-auto"
            style={{ maxHeight: "calc(100vh - 240px)" }}
          >
            {tableData.length > 0 ? (
              <div className="overflow-x-auto">
                <table className="min-w-full text-sm">
                  <thead>
                    <tr className="bg-white">
                      {Object.keys(tableData[0]).map((key) => (
                        <th
                          key={key}
                          className="border border-gray-300 p-2 text-left font-semibold text-gray-900"
                        >
                          {key}
                        </th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {tableData.map((row, idx) => (
                      <tr
                        key={idx}
                        className={idx % 2 === 0 ? "bg-white" : "bg-gray-50"}
                      >
                        {Object.values(row).map((val, i) => (
                          <td
                            key={i}
                            className="border border-gray-300 p-2 text-gray-900"
                          >
                            {val}
                          </td>
                        ))}
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            ) : (
              <div className="flex items-center justify-center h-32 text-gray-600">
                No data loaded. Please upload a file to begin.
              </div>
            )}
          </div>

          <div
            className="mt-2 p-3 border-t border-gray-300 bg-white max-h-40 overflow-y-auto"
            id="messages-container"
          >
            {messages.length > 0 ? (
              <div className="space-y-2">
                {messages.map((msg, idx) => (
                  <div
                    key={idx}
                    className={`message-item p-2 rounded ${
                      msg === "Process Successful"
                        ? "bg-green-50 text-green-700 font-semibold border border-green-200"
                        : "bg-blue-50 text-blue-700 border border-blue-200"
                    }`}
                  >
                    {msg === "Process Successful" ? (
                      <div className="flex items-center">
                        <svg
                          className="w-5 h-5 mr-1"
                          fill="currentColor"
                          viewBox="0 0 20 20"
                          xmlns="http://www.w3.org/2000/svg"
                        >
                          <path
                            fillRule="evenodd"
                            d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
                            clipRule="evenodd"
                          />
                        </svg>
                        {msg}
                      </div>
                    ) : (
                      msg
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-gray-600 text-sm italic">
                Processing messages will appear here
              </div>
            )}
          </div>
        </div>

        <div className="w-1/3 bg-white shadow rounded-lg border border-gray-300 overflow-hidden">
          <div className="p-3 bg-white border-b border-gray-300 font-medium text-gray-900">
            Processing Steps
          </div>
          <div
            className="p-4 space-y-2 text-sm overflow-y-auto"
            id="steps-container"
            style={{ maxHeight: "calc(100vh - 200px)" }}
          >
            {rightSteps.length > 0 ? (
              rightSteps.map((step, idx) => (
                <div
                  key={idx}
                  className="step-item flex items-center p-2 border-b border-gray-200 text-gray-900"
                >
                  <svg
                    className="w-5 h-5 mr-2 text-green-500"
                    fill="currentColor"
                    viewBox="0 0 20 20"
                    xmlns="http://www.w3.org/2000/svg"
                  >
                    <path
                      fillRule="evenodd"
                      d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z"
                      clipRule="evenodd"
                    />
                  </svg>
                  {step}
                </div>
              ))
            ) : (
              <div className="text-gray-600 text-sm italic py-4 text-center">
                Processing steps will appear here
              </div>
            )}
          </div>
        </div>
      </div>

      <div className="p-4 border-t border-gray-300 bg-white flex items-center">
        <div className="flex items-center w-full">
          <label
            htmlFor="file-upload"
            className={`cursor-pointer px-4 py-2 rounded border border-gray-900 shadow text-white ${
              isLoading
                ? "bg-gray-400 cursor-not-allowed"
                : "bg-gray-900 hover:bg-gray-800"
            }`}
          >
            {isLoading ? (
              <div className="flex items-center">
                <Loader className="animate-spin mr-2" size={16} />
                Processing
              </div>
            ) : (
              "Upload File & Start Process"
            )}
            <input
              id="file-upload"
              type="file"
              accept=".csv, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet, application/vnd.ms-excel"
              onChange={handleFileUpload}
              className="hidden"
              disabled={isLoading}
            />
          </label>
          <span className="text-sm text-gray-900 ml-4">
            {tableData.length > 0
              ? `${tableData.length} rows loaded`
              : "No file selected"}
          </span>
        </div>
      </div>
    </div>
  );
};

export default Coa;
