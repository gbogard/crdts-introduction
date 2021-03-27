const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const { resolve } = require("path");

const mode =
  process.env.NODE_ENV === "production" ? "production" : "development";
const isProduction = mode === "production";

module.exports = {
  mode,
  entry: "./index.js",
  plugins: [
    new CleanWebpackPlugin(),
    new HtmlWebpackPlugin(),
    new CompressionPlugin(),
    new MiniCssExtractPlugin({
      filename: isProduction ? "[name].[contenthash].css" : "[name].css",
    }),
  ],
  devServer: {
    contentBase: "./dist",
    hot: true
  },
  module: {
    rules: [
      {
        test: /\.s[ac]ss$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader", "sass-loader"],
      },
    ],
  },
  output: {
    filename: isProduction ? "[name].[contenthash].js" : "[name].js",
    path: resolve(__dirname, "dist"),
  },
};
