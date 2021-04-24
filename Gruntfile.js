module.exports = function (grunt) {
    // Project config
    // Configured to convert the cson file

    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        cson: {
            compile: {
                files: {
                    "compiled/turing.tmLanguage.json": ["syntaxes/turing.cson"],
                    "compiled/language-config.json": ["language-config.cson"]
                }
            }
        }
    });

    grunt.loadNpmTasks('grunt-cson')
};