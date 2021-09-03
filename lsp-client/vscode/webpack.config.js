//@ts-check

'use strict';

const path = require('path');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const CSON = require('cson-parser');

function stringify_regex(obj) {
    for (let key in obj) {
        if (typeof obj[key] === "object") {
            if (obj[key] instanceof RegExp) {
                obj[key] = obj[key].source;
            } else {
                stringify_regex(obj[key]);
            }
        }
    }
}

function compile_cson(content, mode) {
    let parsed_cson = CSON.parse(content);
    stringify_regex(parsed_cson);
    let new_content = JSON.stringify(parsed_cson);
    return new_content;
}

/**@type {import('webpack').Configuration}*/
const config = {
    target: 'node', // vscode extensions run in a Node.js-context 📖 -> https://webpack.js.org/configuration/node/
    mode: 'none', // this leaves the source code as close as possible to the original (when packaging we set this to 'production')

    entry: './client/src/extension.ts', // the entry point of this extension, 📖 -> https://webpack.js.org/configuration/entry-context/
    output: {
        // the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
        path: path.resolve(__dirname, "dist"),
        filename: 'extension.js',
        libraryTarget: 'commonjs2'
    },
    devtool: 'nosources-source-map',
    externals: {
        vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
    },
    resolve: {
        // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
        extensions: ['.ts', '.js']
    },
    module: {
        rules: [
            {
                test: /\.ts$/,
                exclude: /node_modules/,
                use: [
                    {
                        loader: 'ts-loader'
                    }
                ]
            }
        ]
    },
    plugins: [
        new CopyWebpackPlugin({
            patterns: [
                ["./language-config.cson", "./language-config.json"],
                ["./syntaxes/turing.cson", "./turing.tmLanguage.json"]
            ].map(arg => {
                return {
                    from: arg[0],
                    to: arg[1],
                    transform(content, _path) { return compile_cson(content, config.mode); }
                };
            }),
        })
    ]
};
module.exports = config;