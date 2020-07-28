define_priors <- function(...) {
  fs <- list(...)

  # Store added functions
  f <- lapply(fs, FUN = function(x) {
    list(
      density_function = x[["density_function"]],
      quantile_function = x[["quantile_function"]]
    )
  })

  # Pull all important attributes of the priors into named lists
  min <- unlist(lapply(fs, FUN = function(x) { x[["min"]] }))
  max <- unlist(lapply(fs, FUN = function(x) { x[["max"]] }))
  sd <- rep.int(0L, length(min))
  names(sd) <- names(min)
  distribution <- unlist(lapply(fs, FUN = function(x) { x[["distribution"]] }))

  # Store the vectors as attributes for the entire prior object
  attributes(f)$mins <- min
  attributes(f)$maxs <- max
  attributes(f)$sds <- sd
  attributes(f)$distributions <- distribution

  return(f)
}
