eval_targets <- function(sim_targets, target_list, criteria = c("start", "update", "stop")) {
  # selected criteria point
  criteria <- match.arg(criteria, c("start", "update", "stop"))

  # Calculate Good Distances by major targets
  tot_dist <- sapply(target_list, FUN = function(x, dt) {
    # Get total distance for a given major target
    tot_dist <- get_distance(dt = dt, target_list = x)

    # Determine if all sub targets are in their appropriate ranges
    check <- rep.int(1, times = nrow(dt))
    for (sub in x$names) {
      if (criteria == "start") {
        check <- check*in_range(dt[, sub, with = FALSE], x$lower_bounds_start[sub], x$upper_bounds_start[sub])
      } else if (criteria == "update") {
        check <- check*in_range(dt[, sub, with = FALSE], x$lower_bounds_new[sub], x$upper_bounds_new[sub])
      } else if (criteria == "stop") {
        check <- check*in_range(dt[, sub, with = FALSE], x$lower_bounds_stop[sub], x$upper_bounds_stop[sub])
      } else {
        stop("criteria must be either start, update, or stop")
      }
    }

    # Turn total distance negative if sub targets aren't all in the appropriate range
    fin_distance <- tot_dist*(check*2 - 1)

    return(fin_distance)
  }, dt = sim_targets)

  return(tot_dist)
}
