dMvn <- function(X, mu, Sigma) {
  # Multivariate Normal density function
  # Function pulled from: http://gallery.rcpp.org/articles/dmvnorm_arma/
  # "The bayesm package pure R implementation which is much faster than mvtnorm:::dmvnorm()"
  k <- ncol(X)
  rooti <- backsolve(chol(Sigma), diag(k))
  quads <- colSums((crossprod(rooti, (t(X) - mu)))^2)
  return(exp(-(k/2)*log(2*pi) + sum(log(diag(rooti))) - 0.5*quads))
}
