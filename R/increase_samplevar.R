increase_samplevar <- function(df,sds) {
  # Calculate the correlation matrix of calibrated parameters.............
  if (anyNA(df)) {
    stop("Missing data in correlation matrix")
  }

  # Get covariance of data.table
  sample_cor <- cor(df)
  # check that sample_cor is PSD. Use a diagonal matrix if near singularity
  if (test_singularity(sample_cor, 1e-8)) {
    sample_cor <- diag(rep(1,ncol(df))) # identity matrix
    rownames(sample_cor) <- names(df)
    colnames(sample_cor) <- names(df)

    warning("Singular correlation matrix, Using identity matrix")
  }

  sample_cov <- diag(sds) %*% sample_cor %*% diag(sds)

  # check that sample_cov is PSD. Use a diagonal matrix if near singularity
  if (test_singularity(sample_cov, 1e-8)) {
    sample_cov <- diag(sds^2) # vector on diagonal and 0 everywhere else
    rownames(sample_cov) <- names(df)
    colnames(sample_cov) <- names(df)
    warning("Singular covariance matrix when attempting to increase sampling variance, Using diagonal matrix")
 }

  return(sample_cov)
}