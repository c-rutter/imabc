group_targets <- function(..., group_name = NULL) {
  targets_list <- list(...)

  # Pull all lists into a single list
  targets <- unlist(lapply(targets_list, FUN = function(x) { x[["target"]] }))
  lower_bounds_start <- unlist(lapply(targets_list, FUN = function(x) { x[["low_bound_start"]] }))
  upper_bounds_start <- unlist(lapply(targets_list, FUN = function(x) { x[["up_bound_start"]] }))
  lower_bounds_stop <- unlist(lapply(targets_list, FUN = function(x) { x[["low_bound_stop"]] }))
  upper_bounds_stop <- unlist(lapply(targets_list, FUN = function(x) { x[["up_bound_stop"]] }))
  FUNS <- unlist(lapply(targets_list, FUN = function(x) { if (is.null(x[["FUN"]])) NA else x[["FUN"]] }))

  # Pull the names of the parameters. Check for uniqueness and add unique names where no names are provided.
  target_names <- .unique_names(things_list = targets_list, thing = "target_name")
  names(targets) <- target_names
  names(lower_bounds_start) <- target_names
  names(upper_bounds_start) <- target_names
  names(lower_bounds_stop) <- target_names
  names(upper_bounds_stop) <- target_names
  if (all(is.na(FUNS))) {
    FUNS <- as.null()
  } else {
    PickOut <- which(!is.na(FUNS))
    FUNS <- FUNS[PickOut]
    names(FUNS) <- target_names[PickOut]
  }

  return(list(
    target_group = group_name,
    names = target_names,
    targets = targets,
    lower_bounds_start = lower_bounds_start,
    upper_bounds_start = upper_bounds_start,
    lower_bounds_stop = lower_bounds_stop,
    upper_bounds_stop = upper_bounds_stop,
    FUNS = FUNS
  ))
}
