get_update_targets <- function(targets) {
  # Determine which targets are at both their stopping bounds
  done <- targets$current_lower_bounds == targets$stopping_lower_bounds &
    targets$current_upper_bounds == targets$stopping_upper_bounds

  # Return the group names of those targets
  updates <- attr(targets, "target_groups")[!done]

  return(updates)
}
