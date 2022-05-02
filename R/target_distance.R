target_distance <- function(dt, target_list, dist = getOption("imabc.target_eval_distance")) {
  distance <- sapply(attr(target_list, which = "target_names"), FUN = function(x, dt, target_list, dist_opt) {
    # Get simulated target value
    sim <- dt[[x]]
    # Get desired target value
    obs <- target_list$targets[x]
    scale <- target_list$scales[x]
    upper_stop <- target_list$stopping_upper_bounds[x]
    lower_stop <- target_list$stopping_lower_bounds[x]

    # Control for 0 targets and give alternate distance metric
    if (!is.na(scale) & dist_opt == "zscore") {
      # If the user provides a scale
      # options(imabc.target_eval_distance = "zscore")
      final_dist <- sqrt((obs - sim)^2)/scale

    } else if (!is.na(scale) & dist_opt == "weighted_euclidian") {
      # options(imabc.target_eval_distance = "weighted_euclidian")
      final_dist <- ((obs - sim)^2)/scale^2

    } else if (obs == 0 | dist_opt == "stopping_range") {
      # If the target value is 0, or the user requests it by setting:
      # options(imabc.target_eval_distance = "stopping_range")
      # The range of the stopping bounds times 0.5 is used to scale the distance for a given target
      est_scale <- upper_stop - lower_stop
      est_scale <- est_scale*0.5
      final_dist <- ((obs - sim)^2)/(est_scale^2)

    } else {
      # Default of getOption("imabc.target_eval_distance") == "chisquare"
      final_dist <- ((obs - sim)^2)/(obs^2)

    }
    # Set distance to 0 if in stopping bounds
    in_stop <- in_range(sim, low = lower_stop, high = upper_stop)
    final_dist[in_stop] <- 0

    return(final_dist)
  }, dt = dt, target_list = target_list, dist_opt = dist)

  # If grouped targets exist, aggregate distances
  if (inherits(target_list, "grouped")) {
    if (dist == "zscore") {
      groups <- attr(target_list, which = "target_groups")
      groupings <- factor(groups, levels = unique(groups))
      distance_list <- list()
      for (i1 in 1:nrow(distance)) {
        distance_list[[i1]] <- tapply(distance[i1, ], groupings, max)
      }
      distance <- do.call(rbind, distance_list)
    } else {
      distance <- t(rowsum(t(distance), attr(target_list, which = "target_groups"), reorder = FALSE))
    }
  }

  return(distance)
}
