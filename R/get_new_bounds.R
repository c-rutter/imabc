get_new_bounds <- function(targets_list, sims) {
  targets_list <- lapply(targets_list, FUN = function(x, sim) {
    # Pull empirical bounds and starting/stopping bounds into matrices
    new_bounds <- apply(sim[, attr(x, "target_ids"), with = FALSE], 2, range)
    starts <- matrix(c(x$lower_bounds_start, x$upper_bounds_start), nrow = 2, byrow = TRUE)
    stops <- matrix(c(x$lower_bounds_stop, x$upper_bounds_stop), nrow = 2, byrow = TRUE)

    # Restrict the min of new_bounds to be between the min of start and the min of stop
    tmp <- array(c(starts[1, ], new_bounds[1, ]), dim = c(1, ncol(new_bounds), 2))
    new_bounds[1, ] <- apply(tmp, 2, max)
    tmp <- array(c(new_bounds[1, ], stops[1, ]), dim = c(1, ncol(new_bounds), 2))
    new_bounds[1, ] <- apply(tmp, 2, min)
    # Restrict the max of new_bounds to be between the max of start and the max of stop
    tmp <- array(c(starts[2, ], new_bounds[2, ]), dim = c(1, ncol(new_bounds), 2))
    new_bounds[2, ] <- apply(tmp, 2, min)
    tmp <- array(c(new_bounds[2, ], stops[2, ]), dim = c(1, ncol(new_bounds), 2))
    new_bounds[2, ] <- apply(tmp, 2, max)

    # Calculate movement towards the stopping bounds
    deltas <- (starts - new_bounds)/(starts - stops)
    deltas[is.na(deltas)] <- 999
    # Pick the least amount of movement from the subtargets
    deltas <- apply(deltas, 1, min)
    # Convert this into an actual change for each subtarget
    max_movement <- abs(starts - stops)
    final_movements <- max_movement*deltas

    # Movements up from minimum bounds
    x$lower_bounds_new <- x$lower_bounds_start + final_movements[1, ]
    # Movements down from maximum bounds
    x$upper_bounds_new <- x$upper_bounds_start - final_movements[2, ]

    # Extra check to make sure bounds do not go beyond the stopping point
    x$lower_bounds_new <- ifelse(x$lower_bounds_new > stops[1, ], stops[1, ], x$lower_bounds_new)
    x$upper_bounds_new <- ifelse(x$upper_bounds_new < stops[2, ], stops[2, ], x$upper_bounds_new)

    return(x)
  }, sim = sims)

  return(targets_list)
}
