get_new_bounds <- function(to_update, targets_list, sims) {
  # Get target names from target groups to update
  targ_names <- names(attributes(targets_list)$target_groups)[attributes(targets_list)$target_groups %in% to_update]

  # Pull current bound rules
  new_bounds <- apply(sims[, targ_names, with = FALSE], 2, range)
  current <- matrix(
    c(targets_list$current_lower_bounds[targ_names], targets_list$current_upper_bounds[targ_names]),
    nrow = 2, byrow = TRUE
  )
  colnames(current) <- targ_names
  stoppin <- matrix(
    c(targets_list$stopping_lower_bounds[targ_names], targets_list$stopping_upper_bounds[targ_names]),
    nrow = 2, byrow = TRUE
  )
  colnames(stoppin) <- targ_names

  # Restrict the min of new_bounds to be between the min of start and the min of stop
  tmp <- array(c(current[1, ], new_bounds[1, ]), dim = c(1, ncol(new_bounds), 2))
  new_bounds[1, ] <- apply(tmp, 2, max)
  tmp <- array(c(new_bounds[1, ], stoppin[1, ]), dim = c(1, ncol(new_bounds), 2))
  new_bounds[1, ] <- apply(tmp, 2, min)
  # Restrict the max of new_bounds to be between the max of start and the max of stop
  tmp <- array(c(current[2, ], new_bounds[2, ]), dim = c(1, ncol(new_bounds), 2))
  new_bounds[2, ] <- apply(tmp, 2, min)
  tmp <- array(c(new_bounds[2, ], stoppin[2, ]), dim = c(1, ncol(new_bounds), 2))
  new_bounds[2, ] <- apply(tmp, 2, max)

  # Calculate movement towards the stopping bounds
  deltas <- (current - new_bounds)/(current - stoppin)
  deltas[is.na(deltas)] <- 999

  # If grouped targets exist, Pick the least amount of movement within each group
  if (inherits(targets_list, "grouped")) {
    groups <- attr(targets_list[targ_names], which = "target_groups")
    for (i1 in unique(groups)) {
      subtargets <- names(groups[groups == i1])
      # Pick the least amount of movement (within target groups)
      most_allowed <- apply(deltas[, subtargets, drop = FALSE], 1, min)
      deltas[, subtargets] <- matrix(rep(most_allowed, length(subtargets)), nrow = 2)
    }
  }

  # Convert this into an actual change for each subtarget
  max_movement <- abs(current - stoppin)
  final_movements <- max_movement*deltas

  # Initialize new bounds
  targets_list$new_lower_bounds <- targets_list$current_lower_bounds
  targets_list$new_upper_bounds <- targets_list$current_upper_bounds

  # Movements up from minimum bounds
  targets_list$new_lower_bounds[targ_names] <- targets_list$new_lower_bounds[targ_names] + final_movements[1, ]
  # Movements down from maximum bounds
  targets_list$new_upper_bounds[targ_names] <- targets_list$new_upper_bounds[targ_names] - final_movements[2, ]

  # Extra check to make sure bounds do not go beyond the stopping point
  targets_list$new_lower_bounds[targ_names] <- ifelse(
    targets_list$new_lower_bounds[targ_names] >= stoppin[1, ], stoppin[1, ], targets_list$new_lower_bounds[targ_names]
  )
  targets_list$new_upper_bounds[targ_names] <- ifelse(
    targets_list$new_upper_bounds[targ_names] <= stoppin[2, ], stoppin[2, ], targets_list$new_upper_bounds[targ_names]
  )

  return(targets_list)
}
