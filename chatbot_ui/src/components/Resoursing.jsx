import React from "react";

const ResourceUtilization = () => {
  const data = [
    {
      level: "1x",
      resolvers: 11,
      utilization: 92.65,
    },
    {
      level: "1.25x",
      resolvers: 9,
      utilization: 90.59,
    },
    {
      level: "1.3x",
      resolvers: 8,
      utilization: 97.99,
    },
    {
      level: "1.35x",
      resolvers: 8,
      utilization: 94.36,
    },
  ];

  return (
    <div className="w-full space-y-4">
      {data.map((item, index) => (
        <div
          key={index}
          className="bg-white rounded-lg shadow-md p-6 hover:shadow-lg transition-shadow duration-200"
        >
          <div className="grid grid-cols-3 gap-8">
            <div className="flex flex-col items-center">
              <span className="text-3xl font-semibold mb-2">{item.level}</span>
              <span className="text-sm text-gray-500 text-center">
                First Productivity Level
              </span>
            </div>

            <div className="flex flex-col items-center">
              <span className="text-3xl font-semibold mb-2">
                {item.resolvers}
              </span>
              <span className="text-sm text-gray-500 text-center">
                Sum of Resolvers Needed
              </span>
            </div>

            <div className="flex flex-col items-center">
              <span className="text-3xl font-semibold mb-2">
                {item.utilization}
              </span>
              <span className="text-sm text-gray-500 text-center">
                Utilization (%)
              </span>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};

export default ResourceUtilization;