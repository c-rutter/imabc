get_update_targets <- function(targets) {

  done <- targets$current_lower_bounds == targets$stopping_lower_bounds &
    targets$current_upper_bounds == targets$stopping_upper_bounds
  updates <- attr(targets, "target_groups")[!done]

  return(updates)
}
