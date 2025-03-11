import React, { useState, useEffect } from "react";
import { UserIcon, LockIcon, EyeIcon, EyeOffIcon } from "lucide-react";
import chatbotIntro from "../assets/logo.png";
import { use } from "react";

// Custom color palette
const COLORS = {
  primary: "#FFA500",
  secondary: "#00AECF",
  dark: "#073161",
};

const USERS = [
  { username: "admin", password: "grise2024!", token: "admin-token" },
  { username: "703055690", password: "Welcome@2025", token: "user-token" },
  { username: "703070518", password: "Welcome@2025", token: "user-token" },
  { username: "302009439", password: "Welcome@2025", token: "user-token" },
];

const LoginPage = ({ onLogin }) => {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [error, setError] = useState("");

  const handleLogin = (e) => {
    e.preventDefault();

    const user = USERS.find(
      (u) => u.username === username && u.password === password
    );

    if (user) {
      // Store token in localStorage
      console.log(user.token);
      localStorage.setItem("authToken", user.token);

      onLogin(); // Update authentication state
      setError("");
    } else {
      setError("Invalid credentials. Please try again.");
    }
  };

  return (
    <div className="min-h-screen bg-gray-100 flex items-center justify-center px-4">
      <div className="w-full max-w-md">
        <div className="flex item-center justify-center">
          <img src={chatbotIntro} alt="Chatbot Intro" className="h-28 " />
        </div>
        <div
          className="bg-white shadow-xl rounded-xl border-t-4 overflow-hidden"
          style={{ borderTopColor: COLORS.primary }}
        >
          <div className="bg-gray-50 py-4 text-center">
            <h2 className="text-2xl font-bold" style={{ color: COLORS.dark }}>
              Login to Continue
            </h2>
          </div>

          <div className="p-8">
            <form onSubmit={handleLogin} className="space-y-6">
              <div>
                <label
                  htmlFor="username"
                  className="block mb-2 font-medium"
                  style={{ color: COLORS.secondary }}
                >
                  Username
                </label>
                <div className="relative">
                  <UserIcon
                    className="absolute left-3 top-1/2 transform -translate-y-1/2"
                    style={{ color: COLORS.primary }}
                  />
                  <input
                    type="text"
                    value={username}
                    onChange={(e) => setUsername(e.target.value)}
                    placeholder="Enter your username"
                    className="w-full pl-10 pr-3 py-2 border-2 rounded-md focus:outline-none"
                    style={{
                      borderColor: COLORS.secondary,
                    }}
                    required
                  />
                </div>
              </div>

              <div>
                <label
                  htmlFor="password"
                  className="block mb-2 font-medium"
                  style={{ color: COLORS.secondary }}
                >
                  Password
                </label>
                <div className="relative">
                  <LockIcon
                    className="absolute left-3 top-1/2 transform -translate-y-1/2"
                    style={{ color: COLORS.primary }}
                  />
                  <input
                    type={showPassword ? "text" : "password"}
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    placeholder="Enter your password"
                    className="w-full pl-10 pr-12 py-2 border-2 rounded-md focus:outline-none"
                    style={{
                      borderColor: COLORS.secondary,
                    }}
                    required
                  />
                  <button
                    type="button"
                    onClick={() => setShowPassword(!showPassword)}
                    className="absolute right-3 top-1/2 transform -translate-y-1/2"
                    style={{ color: COLORS.dark }}
                  >
                    {showPassword ? <EyeOffIcon /> : <EyeIcon />}
                  </button>
                </div>
              </div>

              {error && (
                <div
                  className="text-center py-2 rounded"
                  style={{
                    backgroundColor: `${COLORS.primary}20`,
                    color: COLORS.primary,
                  }}
                >
                  {error}
                </div>
              )}

              <button
                type="submit"
                className="w-full py-2 rounded-md text-white font-semibold transition-colors duration-300"
                style={{
                  backgroundColor: COLORS.dark,
                }}
              >
                Sign In
              </button>
            </form>

            <div className="text-center mt-6">
              <a
                href="#"
                className="text-sm"
                style={{ color: COLORS.secondary }}
              >
                Forgot Password?
              </a>
            </div>
          </div>
        </div>

        <div className="text-center mt-4">
          <p className="text-sm" style={{ color: COLORS.dark }}>
            © 2024 Grise SAP Support Framework. All Rights Reserved.
          </p>
        </div>
      </div>
    </div>
  );
};

export default LoginPage;
