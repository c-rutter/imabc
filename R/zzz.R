.onLoad <- function(libname, pkgname) {
  # Set default package options
  op <- options()
  op.imabc <- list(
    imabc.target_eval_distance = "chisquare"
  )
  toset <- !(names(op.imabc) %in% names(op))
  if(any(toset)) options(op.imabc[toset])

  invisible()
}
