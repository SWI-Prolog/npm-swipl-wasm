const path = require("path");

module.exports = {
  entry: "./src/index.js",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "public"),
  },
  resolve: {
    // Makes require("crypto") give an empty module.
    // These modules are not used in browser anyway (runtime check).
    fallback: {
      crypto: false,
      fs: false,
      path: false,
      perf_hooks: false,
      "node:crypto": false,
      "node:fs": false,
      "node:path": false,
      "node:perf_hooks": false,
    },
  },
  module: {
    rules: [
      {
        test: /\.(wasm|data)$/,
        type: "asset/resource",
      },
    ],
  },
};
