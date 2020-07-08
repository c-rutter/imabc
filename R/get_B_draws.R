get_B_draws <- function(B, inflate, center, cov_matrix, priors, parm_names) {
  # Simulate values following a multivariate normal
  x <- as.data.table(mvrnorm(n = trunc(inflate*B), mu = center, Sigma = cov_matrix))
  x$in_range <- get_in_range(parms = x, priors = priors, parm_names = parm_names)
  if (sum(x$in_range) > 0) {
    setorder(x, -in_range)
    x$in_range <- NULL

    return(x[1:B, ])
  } else {
    # CM NOTE: Should this be an actual error?
    print(paste("get_B_draws error: no parameter draws are in range with inflate=", inflate))

    return(NULL)
  }
}