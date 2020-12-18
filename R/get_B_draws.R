get_B_draws <- function(B, inflate, center, cov_matrix, priors) {
  # Samples parameters from a multivariate normal with no assumption of independence

  # inflate allows the function to generate extra values in case some or considered to be invalid
  x <- as.data.table(mvrnorm(n = trunc(inflate*B), mu = center, Sigma = cov_matrix))

  # Reject out of bounds parameters
  x$in_range <- get_in_range(compare_list = priors, check_dt = x, out = "numeric")
  if (any(x$in_range > 0)) {
    x <- x[in_range > 0]
    x$in_range <- NULL

    # If sum(in_range > 0) < B, then B - sum(in_range > 0) rows will be assigned NAs
    return(x[1:B, ])
  } else {
    # Warning is created and handled by imabc.R
    return(NULL)
  }
}
