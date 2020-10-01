#' @export
get_distance <- function(dt, target_list, dist = getOption("imabc.target_eval_distance")) {
  distance <- sapply(attr(target_list, which = "target_names"), FUN = function(x, dt, target_list, dist_opt) {
    sim <- dt[[x]]
    obs <- target_list$targets[x]

    # Control for 0 targets and give alternate distance metric
    if (obs == 0 | dist_opt == "stopping_range") {
      new_scale <- target_list$stopping_upper_bounds[x] - target_list$stopping_lower_bounds[x]
      new_scale <- new_scale*0.5

      return(((obs - sim)^2)/(new_scale^2))
    } else {

      return(((obs - sim)^2)/(obs^2))
    }
  }, dt = dt, target_list = target_list, dist_opt = dist)

  # If grouped targets exist, aggregate distances
  if (inherits(target_list, "grouped")) {
    distance <- t(rowsum(t(distance), attr(target_list, which = "target_groups"), reorder = FALSE))
  }

  return(distance)
}
