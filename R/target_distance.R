target_distance <- function(dt, target_list, dist = getOption("imabc.target_eval_distance")) {
  distance <- sapply(attr(target_list, which = "target_names"), FUN = function(x, dt, target_list, dist_opt) {
    # Get simulated target value
    sim <- dt[[x]]
    # Get desired target value
    obs <- target_list$targets[x]

    # Control for 0 targets and give alternate distance metric
    if (obs == 0 | dist_opt == "stopping_range") {
      # If the target value is 0, or the user requests it by setting:
      # options(imabc.target_eval_distance = "stopping_range")
      # The range of the stopping bounds times 0.5 is used to scale the distance for a given target
      new_scale <- target_list$stopping_upper_bounds[x] - target_list$stopping_lower_bounds[x]
      new_scale <- new_scale*0.5

      return(((obs - sim)^2)/(new_scale^2))
    } else {
      # Default of getOption("imabc.target_eval_distance") == "chisquare"
      return(((obs - sim)^2)/(obs^2))
    }
  }, dt = dt, target_list = target_list, dist_opt = dist)

  # If grouped targets exist, aggregate distances
  if (inherits(target_list, "grouped")) {
    distance <- t(rowsum(t(distance), attr(target_list, which = "target_groups"), reorder = FALSE))
  }

  return(distance)
}
