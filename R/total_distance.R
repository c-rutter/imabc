total_distance <- function(dt, target_names, scale = FALSE, mu = NULL, sd = NULL) {
  # scale == TRUE is used for targets
  # scale == FALSE is used for priors
  if (!scale) {
    # Simple Euclidian Distance
    distance <- sqrt(rowSums(dt[, target_names, with = FALSE]^2))
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
