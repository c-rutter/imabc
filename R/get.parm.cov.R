get.parm.cov <- function(var.data,sample.mean.i){
  # Calculate the covariance matrix of calibrated parameters.............
  if(anyNA(var.data)){
    print("Error: missing data in covariance matrix")
    return(-1)
  }
  sample.cov = cov(var.data)
  if(test.singularity(sample.cov,1e-8)){
    # check that sample.cov is PSD. Use a diagonal matrix if near singularity
    print("singular covariance matrix")
    parm.var = diag(sample.cov)
    sample.cov=diag(parm.var)
  }
  return(sample.cov)
}