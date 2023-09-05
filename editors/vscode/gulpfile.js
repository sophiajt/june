var gulp = require('gulp')
var yaml = require('gulp-yaml')

gulp.task('yaml', function(cb) {
  gulp
    .src('./syntaxes/*.yml')
    .pipe(yaml({ safe: false, space: 4 }))
    .pipe(
      gulp.dest(function (f) {
        return f.base
      })
    )
  cb()
})

gulp.task(
  'watch-yaml',
  gulp.series('yaml', function(cb) {
    gulp.watch('./syntaxes/*.yml', gulp.series('yaml'))
    cb()
  })
)
