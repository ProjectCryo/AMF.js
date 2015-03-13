module.exports = (grunt) ->
  grunt.initConfig
    coffee:
      compile:
        expand: true
        flatten: true
        cwd: 'src/'
        src: ['*.coffee']
        dest: 'lib/'
        ext: '.js'
    mochaTest:
      options:
        reporter: 'nyan'
      src: ['test/test.coffee']

  grunt.loadNpmTasks 'grunt-contrib-coffee'
  grunt.loadNpmTasks 'grunt-mocha-test'

  grunt.registerTask 'default', ['coffee', 'mochaTest']