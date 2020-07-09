total_distance <- function(dt, target_names, scale = FALSE, mu = NULL, sd = NULL) {

  if (!scale) {
    # Simple Euclidian Distance
    # dt[, tot_dist := sqrt(rowSums(.SD^2)), .SDcols = target_names]
    distance <- sqrt(rowSums(dt[, target_names, with = FALSE]^2))
  } else {
    # Check for necessary inputs
    if (is.null(mu) | is.null(sd)) {
      stop("get_distance: Scaled distance requires both mu and sd.")
    }
    sd[sd == 0] <- 1
    # CM NOTE: Old method
    # x <- dt[, (target_names), with = FALSE]
    # x <- sweep(x, 2, mu, "-")
    # x <- x^2
    # x <- sweep(x, 2, sd, "/")
    # x_sum <- apply(x, 1, sum)
    # data.table method
    distance <- Reduce("+", sapply(target_names, FUN = function(x, df, mu, sd) {
      x <- ((df[, x, with = FALSE] - mu[x])^2)/sd[x]
      x
    }, df = dt, mu = mu, sd = sd))
  }

  return(distance)
}
