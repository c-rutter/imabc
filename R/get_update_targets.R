get_update_targets <- function(targets) {
  # Determine which targets are at both their stopping bounds
  done <- signif(targets$current_lower_bounds,5) == signif(targets$stopping_lower_bounds,5) &
    signif(targets$current_upper_bounds,5) == signif(targets$stopping_upper_bounds,5)

  # Return the group names of those targets
  updates <- attr(targets, "target_groups")[!done]

  return(updates)
}
