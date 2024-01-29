module.exports = {
  content: ["./src/**/*.{html,js,jsx,ts,tsx,elm}"],
  theme: {
    extend: {},
  },
  plugins: [require("daisyui")],
  daisyui: {
    themes: ["light", "dark", "cupcake", "corporate", "retro"],
  },
}