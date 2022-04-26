eval_targets <- function(sim_targets, target_list, criteria = c("start", "update", "stop")) {
  # selected criteria point
  criteria <- match.arg(criteria, c("start", "update", "stop"))

  # Subset to only targets that have yet to finish updating
  targs_by_group <- attr(target_list, "target_groups")
  targ_groups <- unique(targs_by_group)
  update_targets <- attr(target_list, "update")
  done_groups <- !targ_groups %in% update_targets
  if (any(done_groups)) {
    include_t1 <- names(targs_by_group[targs_by_group %in% targ_groups[done_groups]])
    include_t2 <- names(update_targets)
    include_ts <- unique(c(include_t1, include_t2))
    final_ts <- attr(target_list, "target_names")[attr(target_list, "target_names") %in% include_ts]
    new_target_list <- target_list[final_ts]
  } else {
    new_target_list <- target_list[names(update_targets)]
  }

  # Calculate distances (by group if target groups exist)
  distances <- target_distance(dt = sim_targets, target_list = new_target_list)

  # Determine which targets are in range (by group if target groups exist)
  check <- get_in_range(compare_list = target_list, check_dt = sim_targets, criteria = criteria, out = "numeric")
  fin_distance <- distances*check

  return(as.data.table(fin_distance))
}
