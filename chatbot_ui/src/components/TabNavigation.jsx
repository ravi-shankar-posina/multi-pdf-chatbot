import React from "react";
import { FileSpreadsheet, PieChart, Table } from "lucide-react";

const TabNavigation = ({ activeTab, onTabChange }) => {
  const tabs = [
    { id: "analysis", title: "Analysis ", icon: PieChart },
    { id: "complexity", title: "Complexity", icon: FileSpreadsheet },
    { id: "resourcing", title: "Resourcing", icon: Table },
  ];

  return (
    <div className="flex space-x-2 mr-4">
      {tabs.map((tab) => {
        const Icon = tab.icon;
        return (
          <button
            key={tab.id}
            onClick={() => onTabChange(tab.id)}
            className={`flex items-center space-x-2 px-4 py-2 rounded-t-lg transition-colors
              ${
                activeTab === tab.id
                  ? "bg-gray-600 text-white font-bold"
                  : "bg-white text-gray-600 hover:bg-gray-100"
              }`}
          >
            <Icon size={18} />
            <span>{tab.title}</span>
          </button>
        );
      })}
    </div>
  );
};

export default TabNavigation;
