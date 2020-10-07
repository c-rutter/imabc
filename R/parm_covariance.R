#' @export
parm_covariance <- function(df) {
  # Calculate the covariance matrix of calibrated parameters.............
  if (anyNA(df)) {
    stop("Missing data in covariance matrix")
  }
  sample_cov <- cov(df)
  # CM NOTE: Should 1e-8 be an input
  if (test_singularity(sample_cov, 1e-8)) {
    # check that sample.cov is PSD. Use a diagonal matrix if near singularity
    warning("Singular covariance matrix, Using diagonal matrix")
    parm_var <- diag(sample_cov) # extracts diagonal to a vector
    sample_cov <- diag(parm_var) # vector on diagonal and 0 everywhere else
  }

  return(sample_cov)
}
