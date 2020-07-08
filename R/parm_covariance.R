parm_covariance <- function(df) {
  # Calculate the covariance matrix of calibrated parameters.............
  if (anyNA(df)) {
    # CM NOTE: Should this be an error since the code returns nothing if it happens?
    warning("Error: missing data in covariance matrix")

    return(-1)
  }
  sample_cov <- cov(df)
  # CM NOTE: Should 1e-8 be an input
  if (test_singularity(sample_cov, 1e-8)) {
    # check that sample.cov is PSD. Use a diagonal matrix if near singularity
    warning("parm_covariance: singular covariance matrix, using diagonal matrix")
    parm_var <- diag(sample_cov) # extracts diagonal to a vector
    sample_cov <- diag(parm_var) # vector on diagnoal and 0 everywhere else
  }

  return(sample_cov)
}