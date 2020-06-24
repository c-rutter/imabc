eval_targets <- function(dist_target, target_names, target_list) {
  # Flips targets to negative if they are not in their bounds
  dist_target[, (target_names) := lapply(target_names, FUN = function(x, target_list, df) {
    y <- df[[x]]
    check <- in_range(y, target_list[[x]]$low_bound_start, target_list[[x]]$up_bound_start)
    y <- y*(check*2 - 1)
    y
  }, target_list = target_list, df = dist_target)]
  # Calculate the distance
  dist_target <- get_distance(dt = dist_target, target_names = target_names)

  return(dist_target)
}
