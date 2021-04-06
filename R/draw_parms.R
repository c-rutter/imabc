draw_parms <- function(n_add, mu, sigma, priors_list) {
  # Samples parameters from a truncated normal under the assumption of independence
  center_id <- NULL

  # Make sure mu is a matrix
  if (is.vector(mu)) {
    mu_name <- names(mu)
    mu <- matrix(mu, byrow = TRUE, nrow = 1)
    colnames(mu) <- mu_name
  } # is.vector(mu)

  # Prepare information for calculation
  n_centers <- nrow(mu)
  n_parms <- ncol(mu)
  n_row_final <- n_add*n_centers
  parm_names <- names(priors_list)
  lower_bounds <- attr(priors_list, "mins")
  upper_bounds <- attr(priors_list, "maxs")

  # Initialize results data.table
  draws <- data.table(matrix(0, nrow = n_row_final, ncol = n_parms))
  setnames(draws, parm_names)

  # Create sample from parameter info and truncated normal
  draws[, center_id := rep(1:n_centers, each = n_add)]
  draws[, (parm_names) := lapply(parm_names, FUN = function(x, n, mu, sigma, lower, upper) {
    # Handle parameters with no variance (i.e. fixed) by assigning their mean value
    if (sigma[center_id, x] != 0) {
      rtruncnorm(
        n, # Number of draws per center
        a = lower[x], # Lower bound of a given parameter
        b = upper[x], # Upper bound of a given parameter
        mean = mu[center_id, x], # Mean of each center
        sd = sigma[center_id, x] # Standard Deviation of each center
      )
    } else {
      # Fixed parameters
      rep(mu[center_id, x], n)
    }
  }, n = n_add, mu = mu, sigma = sigma, lower = lower_bounds, upper = upper_bounds), by = center_id]
  draws$center_id <- NULL

  return(draws)
}
