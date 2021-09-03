const webpackConfig = require('./webpack.config');

module.exports = function (grunt) {
    // Project config
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        webpack: {
            do_watch: Object.assign({ watch: true }, webpackConfig),
            do_package: Object.assign(webpackConfig, {
                mode: 'production',
                devtool: 'cheap-source-map',
            }),
            do_compile: webpackConfig,
        }
    });

    grunt.loadNpmTasks('grunt-webpack');

    // Aggregate tasks
    grunt.registerTask('default', ['webpack:do_compile']);
    grunt.registerTask('compile', ['webpack:do_compile']);
    grunt.registerTask('watch', ['webpack:do_watch']);
    grunt.registerTask('package', ['webpack:do_package']);
};
