dMvn <- function(X,mu,Sigma) {
  # reference: http://gallery.rcpp.org/articles/dmvnorm_arma/
  k <- ncol(X)
  rooti <- backsolve(chol(Sigma),diag(k))
  quads <- colSums((crossprod(rooti,(t(X)-mu)))^2)
  return(exp(-(k/2)*log(2*pi) + sum(log(diag(rooti))) - .5*quads))
}
