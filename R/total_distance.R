total_distance <- function(dt, target_names, scale = FALSE, mu = NULL, sd = NULL, dist = getOption("imabc.target_eval_distance")) {
  # scale == FALSE is used for targets
  # scale == TRUE is used for priors
  if (!scale) {
    if (dist == "zscore") {
      # abs is just a pre-caution. Good draws are determined by whether the individual groups/targets have negative, not
      #   by whether the total distance is negative or positive. Still, just in case a calculation is done on all draws
      #   and the calculation could blow up with a negative, I ensure the row max is a positive value
      distance <- abs(do.call(pmax, dt[, target_names, with = FALSE]))
    } else if (dist == "weighted_euclidian") {
      distance <- sqrt(rowSums(dt[, target_names, with = FALSE]))
    } else {
      # Simple Euclidian Distance
      # CM NOTE: Come back to whether we square the sums
      distance <- sqrt(rowSums(dt[, target_names, with = FALSE]^2))
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
