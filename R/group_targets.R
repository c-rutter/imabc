group_targets <- function(...) {
  dots <- list(...)

  # Get names for the added targets
  new_names <- name_targets(dots, grouping = TRUE)

  # Pull all lists into a single list
  targets <- unlist(lapply(dots, FUN = function(x) { x[["target"]] }))
  lower_bounds_start <- unlist(lapply(dots, FUN = function(x) { x[["low_bound_start"]] }))
  upper_bounds_start <- unlist(lapply(dots, FUN = function(x) { x[["up_bound_start"]] }))
  lower_bounds_stop <- unlist(lapply(dots, FUN = function(x) { x[["low_bound_stop"]] }))
  upper_bounds_stop <- unlist(lapply(dots, FUN = function(x) { x[["up_bound_stop"]] }))

  # Affix appropriate names to the list elements
  names(targets) <- new_names
  names(lower_bounds_start) <- new_names
  names(upper_bounds_start) <- new_names
  names(lower_bounds_stop) <- new_names
  names(upper_bounds_stop) <- new_names

  return(list(
    names = new_names,
    targets = targets,
    lower_bounds_start = lower_bounds_start,
    upper_bounds_start = upper_bounds_start,
    lower_bounds_stop = lower_bounds_stop,
    upper_bounds_stop = upper_bounds_stop
  ))
}
