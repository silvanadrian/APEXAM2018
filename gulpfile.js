
var gulp = require('gulp');
var shell = require('gulp-shell');
var notify = require("gulp-notify");

gulp.task('latex', function() {
  return gulp.src('*.tex', {read: false})
  .pipe(shell(["pdflatex --shell-escape report.tex"],{quiet: true})
  .on('error', notify.onError({
    title: "Compiling Failed",
    message: "Latex Document couldn't get typesetted.",
    "sound": "Sosumi"
  })
))
  .pipe(notify({
    title: "Compiling Succeded!",
    message: "Documentation has been typesetted Succesfully!",
    "sound": "Pop"
    }
  ))
});

gulp.task('watch', function() {
  gulp.watch('**/*.tex', ['latex']);
});
