const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const CopyPlugin = require("copy-webpack-plugin");
const RemarkHTML = require("remark-html");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const { resolve } = require("path");

const mode =
  process.env.NODE_ENV === "production" ? "production" : "development";
const isProduction = mode === "production";

module.exports = {
  mode,
  entry: "./index.js",
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin({
      title:
        "A gentle introduction to conflict-free replicated data types (CRDT) - Guillaume Bogard",
    }),
    new MiniCssExtractPlugin({
      filename: isProduction ? "[name].[contenthash].css" : "[name].css",
    }),
    new CopyPlugin({ patterns: [{ from: "slides", to: "slides" }] }),
  ],
  devServer: {
    contentBase: "./dist",
    hot: true,
    historyApiFallback: true,
  },
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
      },
      {
        test: /\.md$/,
        use: [
          {
            loader: "html-loader",
          },
          {
            loader: "remark-loader",
            options: {
              remarkOptions: {
                plugins: [RemarkHTML],
              },
            },
          },
        ],
      },
      {
        test: /\.png$/i,
        use: [
          {
            loader: "file-loader",
          },
        ],
      },
    ],
  },
  output: {
    filename: isProduction ? "[name].[contenthash].js" : "[name].js",
    path: resolve(__dirname, "dist"),
    publicPath: "/",
  },
};
