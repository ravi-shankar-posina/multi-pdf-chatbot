import React from 'react'

const SopCreation = () => {
  return (
    <div className="flex-1 h-full">
    <iframe
      src={import.meta.env.VITE_SOP_TEST_SCRIPT_URL}
      title="SAP Chat Application"
      width="100%"
      height="100%"
      style={{ border: "none" }}
    />
  </div>
  )
}

export default SopCreation