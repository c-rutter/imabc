eval_targets <- function(sim_targets, target_list, criteria = c("start", "update", "stop")) {
  # selected criteria point
  criteria <- match.arg(criteria, c("start", "update", "stop"))

  # Calculate distances (by group if target groups exist)
  distances <- target_distance(dt = sim_targets, target_list = target_list)

  # Determine which targets are in range (by group if target groups exist)
  check <- get_in_range(compare_list = target_list, check_dt = sim_targets, criteria = criteria, out = "numeric")
  fin_distance <- distances*check

  return(as.data.table(fin_distance))
}
