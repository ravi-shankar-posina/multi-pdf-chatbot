import { EyeIcon, EyeOffIcon, LockIcon, UserIcon } from "lucide-react";
import { useEffect, useState } from "react";
import logo from "../assets/image.png";

// Updated color palette for black and white theme
const COLORS = {
  primary: "#000000",    // Black
  secondary: "#6b7280",  // Gray-500
  light: "#f3f4f6",      // Gray-100
  accent: "#3b82f6",     // Blue-500 for subtle accents
};

const USERS = [
  { username: "admin", password: "grise2024!", token: "admin-token" },
  { username: "703055690", password: "Password@2025", token: "user-token" },
  { username: "703070518", password: "Password@2025", token: "user-token" },
  { username: "302009439", password: "Password@2025", token: "user-token" },
];

// Generate secure token that includes password hash
const generateSecureToken = (username, password) => {
  // Create a simple hash of username + password + timestamp
  const data = `${username}:${password}:${Date.now()}`;
  return btoa(data); // Base64 encode for simplicity
};

// Validate token against current user credentials
const validateToken = (token) => {
  try {
    const decoded = atob(token);
    const [username, password] = decoded.split(':');

    // Check if user exists with current password
    const user = USERS.find(u => u.username === username && u.password === password);
    return !!user;
  } catch (error) {
    return false;
  }
};

const LoginPage = ({ onLogin }) => {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [error, setError] = useState("");

  // Check if existing token is still valid on component mount
  useEffect(() => {
    const existingToken = localStorage.getItem("authToken");
    if (existingToken && !validateToken(existingToken)) {
      // Token is invalid (password changed), remove it
      localStorage.removeItem("authToken");
      localStorage.removeItem("username"); // Also remove stored username
      setError("Your session has expired due to password change. Please login again.");
    }
  }, []);

  const handleLogin = (e) => {
    e.preventDefault();

    const user = USERS.find(
      (u) => u.username === username && u.password === password
    );

    if (user) {
      // Generate secure token with current password
      const secureToken = generateSecureToken(username, password);

      // Store token and username
      localStorage.setItem("authToken", secureToken);
      localStorage.setItem("username", username);

      onLogin(); // Update authentication state
      setError("");
    } else {
      setError("Invalid credentials. Please try again.");
    }
  };

  return (
    <div className="min-h-screen bg-gray-50 flex items-center justify-center px-4 py-8">
      <div className="w-full max-w-md">
        {/* Logo and Branding Section */}
        <div className="flex flex-col items-center justify-center mb-8">
          <div className="flex items-center mb-4">
            <img 
              src={logo} 
              alt="G-Rise Logo" 
              className="w-12 h-12 sm:w-16 sm:h-16 mr-3" 
            />
            <div className="flex items-center">
              <h1 className="text-3xl sm:text-4xl font-bold text-gray-800">
                G-Rise
              </h1>
              <span className="ml-2 bg-blue-300 text-gray-800 text-sm font-semibold px-2 py-1 rounded-md">
               Plus
              </span>
            </div>
          </div>
          <p className="text-gray-600 text-sm text-center">
            SAP Support Framework
          </p>
        </div>

        {/* Login Card */}
        <div className="bg-white shadow-2xl rounded-2xl border border-gray-200 overflow-hidden">
          <div className="bg-gray-50 py-6 text-center border-b border-gray-200">
            <h2 className="text-2xl font-bold text-gray-900">
              Welcome Back
            </h2>
            <p className="text-sm text-gray-600 mt-1">
              Please sign in to continue
            </p>
          </div>

          <div className="p-8">
            <form onSubmit={handleLogin} className="space-y-6">
              <div>
                <label
                  htmlFor="username"
                  className="block mb-2 text-sm font-semibold text-gray-700"
                >
                  Username
                </label>
                <div className="relative">
                  <UserIcon
                    className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400"
                    size={20}
                  />
                  <input
                    type="text"
                    value={username}
                    onChange={(e) => setUsername(e.target.value)}
                    placeholder="Enter your username"
                    className="w-full pl-10 pr-4 py-3 border-2 border-gray-300 rounded-xl focus:outline-none focus:ring-2 focus:ring-black focus:border-transparent text-gray-900 transition-all duration-200 hover:border-gray-400"
                    required
                  />
                </div>
              </div>

              <div>
                <label
                  htmlFor="password"
                  className="block mb-2 text-sm font-semibold text-gray-700"
                >
                  Password
                </label>
                <div className="relative">
                  <LockIcon
                    className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400"
                    size={20}
                  />
                  <input
                    type={showPassword ? "text" : "password"}
                    value={password}
                    onChange={(e) => setPassword(e.target.value)}
                    placeholder="Enter your password"
                    className="w-full pl-10 pr-12 py-3 border-2 border-gray-300 rounded-xl focus:outline-none focus:ring-2 focus:ring-black focus:border-transparent text-gray-900 transition-all duration-200 hover:border-gray-400"
                    required
                  />
                  <button
                    type="button"
                    onClick={() => setShowPassword(!showPassword)}
                    className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-500 hover:text-gray-700 transition-colors duration-200"
                  >
                    {showPassword ? <EyeOffIcon size={20} /> : <EyeIcon size={20} />}
                  </button>
                </div>
              </div>

              {error && (
                <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-xl text-sm text-center">
                  {error}
                </div>
              )}

              <button
                type="submit"
                className="w-full py-3 px-4 bg-black text-white font-semibold rounded-xl hover:bg-gray-800 focus:outline-none focus:ring-2 focus:ring-black focus:ring-offset-2 transition-all duration-200 transform hover:scale-[1.02] active:scale-[0.98]"
              >
                Sign In
              </button>
            </form>

            <div className="text-center mt-6">
              <a
                href="#"
                className="text-sm text-gray-500 hover:text-black transition-colors duration-200"
              >
                Forgot Password?
              </a>
            </div>
          </div>
        </div>

        {/* Footer */}
        <div className="text-center mt-8">
          <p className="text-sm text-gray-500">
            © 2024 G-Rise SAP Support Framework. All Rights Reserved.
          </p>
        </div>
      </div>
    </div>
  );
};

export default LoginPage;