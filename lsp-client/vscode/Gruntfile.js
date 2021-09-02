const webpackConfig = require('./webpack.config');

module.exports = function (grunt) {
    // Project config
    // Configured to convert the cson file

    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        cson: {
            compile: {
                files: {
                    "dist/turing.tmLanguage.json": ["syntaxes/turing.cson"],
                    "dist/language-config.json": ["language-config.cson"]
                }
            }
        },
        webpack: {
            do_watch: Object.assign({ watch: true }, webpackConfig),
            do_package: Object.assign(webpackConfig, {
                mode: 'production',
                devtool: undefined,
            }, ),
            do_compile: webpackConfig,
        }
    });

    grunt.loadNpmTasks('grunt-cson');
    grunt.loadNpmTasks('grunt-webpack');

    // Aggregate tasks
    grunt.registerTask('default', ['cson', 'webpack:do_compile']);
    grunt.registerTask('compile', ['cson', 'webpack:do_compile']);
    grunt.registerTask('watch', ['cson', 'webpack:do_watch']);
    grunt.registerTask('package', ['cson', 'webpack:do_package']);
};
