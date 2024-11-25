import React, { useEffect } from 'react';
import { FaCode, FaFilePdf, FaHeadset, FaUser } from 'react-icons/fa';
import { MdAccessAlarm } from 'react-icons/md';
import chatbotIntro from '../assets/logo-new1.jpg';
import { Link, Outlet, useLocation } from 'react-router-dom';
const options = [
    { label: 'How To?', path: '/', api: 'csv/query', icon: <FaHeadset /> },
    // { label: 'Incident', path: '/incident', api: 'analyze', icon: <FaCode /> },
    {
        label: 'Best Practices',
        path: '/best-practices',
        api: 'pdf/query',
        icon: <FaFilePdf />,
    },
    // {
    //     label: 'ABAP Code Generator',
    //     path: '/abap-code-generator',
    //     api: 'query',
    //     icon: <FaCode />,
    // },
    {
        label: 'Access Management',
        path: '/access-management',
        icon: <MdAccessAlarm />,
    },
    // {
    //   label: "SAP Test Case Generator",
    //   path: "/sap-test-case-genarator",
    //   icon: <FaUser />,
    // },
    // {
    //     label: 'Test Script Generator',
    //     path: '/test-script-genarator',
    //     icon: <FaUser />,
    // },
];

const Layout = () => {
    const [selectedLabel, setSelectedLabel] = React.useState(options[0].label);
    const location = useLocation();
    useEffect(() => {
        const currentItem = options.find((item) => item.path === location.pathname);
        setSelectedLabel(currentItem ? currentItem.label : 'How To');
    }, [location.pathname]);

    return (
        <div className="flex max-h-screen min-h-screen bg-white overflow-hidden relative">
            <div className="w-72 bg-gray-100 min-h-screen overflow-hidden">
                <div className='py-6 px-6'>
                    <img src={chatbotIntro} alt="Chatbot Intro" className="h-34 mr-2" />
                </div>
                <div className="text-black p-10 hidden md:block">
                    <ul className="space-y-4">
                        {options.map((option, index) => (
                            <li key={index} className="flex items-center">
                                <Link
                                    to={option.path}
                                    className={`flex items-center p-2 space-x-3 rounded-md transition duration-300 ${
                                        location.pathname === option.path ? 'bg-gray-300' : 'hover:bg-gray-200'
                                    }`}
                                >
                                    <span className="text-sm text-black-700">{option.icon}</span>
                                    <span className="font-medium text-black-700">{option.label}</span>
                                </Link>
                            </li>
                        ))}
                    </ul>
                </div>
            </div>
            <div className="flex flex-col w-full max-h-screen min-h-screen overflow-hidden">
                <header className="bg-white border-b border-gray-200 p-2 flex justify-between items-center">
                    <div className="text-green-900 text-xl font-bold">{selectedLabel}</div>
                    <div className="text-green-900 text-3xl">
                        <FaUser />
                    </div>
                </header>
                <main className="flex-1 overflow-hidden">
                    <Outlet />
                </main>
            </div>
        </div>
    );
};

export default Layout;
