target_distance <- function(dt, target_list, dist = getOption("imabc.target_eval_distance")) {
  distance <- sapply(attr(target_list, which = "target_names"), FUN = function(x, dt, target_list, dist_opt) {
    # Get simulated target value
    sim <- dt[[x]]
    # Get desired target value
    obs <- target_list$targets[x]
    scale <- target_list$scales[x]

    # Control for 0 targets and give alternate distance metric
    if (dist_opt == "scale" & !is.na(scale)) {
      # If the user provides a scale
      # options(imabc.target_eval_distance = "scale")
      final_scale <- scale

    } else if (obs == 0 | dist_opt == "stopping_range") {
      # If the target value is 0, or the user requests it by setting:
      # options(imabc.target_eval_distance = "stopping_range")
      # The range of the stopping bounds times 0.5 is used to scale the distance for a given target
      final_scale <- target_list$stopping_upper_bounds[x] - target_list$stopping_lower_bounds[x]
      final_scale <- final_scale*0.5

    } else {
      # Default of getOption("imabc.target_eval_distance") == "chisquare"
      final_scale <- obs

    }

    if (dist_opt == "scale" & !is.na(scale)) {
      return(sqrt((obs - sim)^2)/final_scale)
    }else{
      return(((obs - sim)^2)/(final_scale^2))
    }
  }, dt = dt, target_list = target_list, dist_opt = dist)

  # If grouped targets exist, aggregate distances
  if (inherits(target_list, "grouped")) {
    distance <- t(rowsum(t(distance), attr(target_list, which = "target_groups"), reorder = FALSE))
  }

  return(distance)
}
