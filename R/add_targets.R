add_targets <- function(...) {
  dots <- list(...)

  # CM NOTE: This is one form for specification, could also add version where it is a data.frame that is input
  targets <- unlist(lapply(dots, FUN = function(x) { x[["target"]] }))
  lower_bounds_start <- unlist(lapply(dots, FUN = function(x) { x[["low_bound_start"]] }))
  upper_bounds_start <- unlist(lapply(dots, FUN = function(x) { x[["up_bound_start"]] }))
  lower_bounds_stop <- unlist(lapply(dots, FUN = function(x) { x[["low_bound_stop"]] }))
  upper_bounds_stop <- unlist(lapply(dots, FUN = function(x) { x[["up_bound_stop"]] }))


  return(list(
    names = names(dots),
    targets = targets,
    lower_bounds_start = lower_bounds_start,
    upper_bounds_start = upper_bounds_start,
    lower_bounds_stop = lower_bounds_stop,
    upper_bounds_stop = upper_bounds_stop
  ))
}
