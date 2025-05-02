total_distance <- function(
    dt, target_names, update_targets = NULL, wt = 1,
    scale = FALSE, mu = NULL, sd = NULL, dist = getOption("imabc.target_eval_distance")) {
  # scale == FALSE is used for targets
  # scale == TRUE is used for priors
  if (!scale) {
    if (!is.null(update_targets)) {
      done_targets <- target_names[!target_names %in% update_targets]
      if (dist == "zscore") {
        # abs is just a pre-caution. Good draws are determined by whether the individual groups/targets have negative, not
        #   by whether the total distance is negative or positive. Still, just in case a calculation is done on all draws
        #   and the calculation could blow up with a negative, I ensure the row max is a positive value
        up_ts <- do.call(pmax, dt[, update_targets, with = FALSE])
        done_ts <- do.call(pmax, dt[, done_targets, with = FALSE]*wt)
        distance <- abs(pmax(up_ts, done_ts))
      } else if (dist %in% c("chisquare", "weighted_euclidian")) {
        distance <- euclid_distance(
          dt, target_names = target_names, update_targets = update_targets, wt = wt)
      } else {
        # Simple Euclidian Distance
        up_ts <- rowSums(dt[, update_targets, with = FALSE]^2)
        done_ts <- rowSums((dt[, done_targets, with = FALSE]*wt)^2)
        distance <- sqrt(up_ts + done_ts)
      }
    } else {
      if (dist == "zscore") {
        # abs is just a pre-caution. Good draws are determined by whether the individual groups/targets have negative, not
        #   by whether the total distance is negative or positive. Still, just in case a calculation is done on all draws
        #   and the calculation could blow up with a negative, I ensure the row max is a positive value
        distance <- abs(do.call(pmax, dt[, target_names, with = FALSE]))
      } else if (dist %in% c("chisquare", "weighted_euclidian")) {
        distance <- euclid_distance(dt[, target_names, with = FALSE])
      } else {
        # Simple Euclidian Distance
        # CM NOTE: Come back to whether we square the sums
        distance <- sqrt(rowSums(dt[, target_names, with = FALSE]^2))
      }
    }
  } else {
    # Check for necessary inputs
    if (is.null(mu) | is.null(sd)) {
      stop("Scaled distance requires both mu and sd.")
    }
    sd[sd == 0] <- 1
    distance <- Reduce("+", sapply(target_names, FUN = function(x, df, mu, sd) {
      x <- ((df[, x, with = FALSE] - mu[x])^2)/sd[x]
      x
    }, df = dt, mu = mu, sd = sd))
  }

  return(distance)
}

# Used in imabc as well as in total_distance
euclid_distance <- function(dt, update_targets = NULL, target_names = NULL, wt = 1) {
  if (!is.null(update_targets)) {
    done_targets <- target_names[!target_names %in% update_targets]
    up_ts <- rowSums(abs(dt[, update_targets, with = FALSE]))
    done_ts <- rowSums(abs(dt[, done_targets, with = FALSE]*wt))
    sqrt(up_ts + done_ts)
  } else {
    sqrt(rowSums(abs(dt)))
  }
}
