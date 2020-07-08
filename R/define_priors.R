define_priors <- function(...) {
  # Store added functions
  fs <- list(...)

  # Pull all important attributes of the priors into named lists
  f <- unlist(lapply(fs, FUN = function(x) { x[["f"]] }))
  min <- unlist(lapply(fs, FUN = function(x) { x[["min"]] }))
  max <- unlist(lapply(fs, FUN = function(x) { x[["max"]] }))
  sd <- unlist(lapply(fs, FUN = function(x) { x[["sd"]] }))
  mu <- unlist(lapply(fs, FUN = function(x) { x[["mean"]] }))
  fixed <- unlist(lapply(fs, FUN = function(x) { x[["fixed"]] }))

  # Store the vectors as attributes for the entire prior object
  attributes(f)$mins <- min
  attributes(f)$maxs <- max
  attributes(f)$means <- mu
  attributes(f)$sds <- sd
  attributes(f)$fixed <- fixed

  return(f)
}
