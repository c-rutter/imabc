get_update_targets <- function(targets) {
  updates <- unlist(lapply(targets, FUN = function(x) {
    !all(
      x$lower_bounds_start == x$lower_bounds_stop &
        x$upper_bounds_start == x$upper_bounds_stop
    )
  }))

  return(updates)
}
