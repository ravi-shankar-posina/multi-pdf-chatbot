import React from 'react'

const IframePage = ({ source }) => {
    return (
        <div className="flex-1 h-full">
            <iframe
                src={source}
                title="SAP Chat Application"
                width="100%"
                height="100%"
                style={{ border: "none" }}
            />
        </div>
    );
};

export default IframePage