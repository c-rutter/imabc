parm_covariance <- function(df) {
  # Calculate the covariance matrix of calibrated parameters.............
  if (anyNA(df)) {
    stop("Missing data in covariance matrix")
  }

  # Get covariance of data.table
  sample_cov <- cov(df)
  if (test_singularity(sample_cov, 1e-8)) {
    # check that sample_cov is PSD. Use a diagonal matrix if near singularity
    warning("Singular covariance matrix, Using diagonal matrix")
    parm_var <- diag(sample_cov) # extracts diagonal to a vector
    if (!is.null(dim(parm_var))) {
      sample_cov <- diag(parm_var) # vector on diagonal and 0 everywhere else
      rownames(sample_cov) <- names(df)
      colnames(sample_cov) <- names(df)
    }
  }

  return(sample_cov)
}
