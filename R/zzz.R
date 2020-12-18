.onLoad <- function(libname, pkgname) {
  # Set default package options
  op <- options()
  # stringsAsFactors included to ensure R < 4.0 works as well
  op <- op[grep("[^(stringsAsFactors)]", names(op))]
  op.imabc <- list(
    imabc.target_eval_distance = "chisquare",
    stringsAsFactors = FALSE
  )
  toset <- !(names(op.imabc) %in% names(op))
  if(any(toset)) options(op.imabc[toset])

  invisible()
}
