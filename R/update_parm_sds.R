update_parm_sds <- function(priors, dt, parms) {
  # Calculate the empirical standard deviation for each parameter and save it to priors object
  attr(priors, "sds") <- sapply(parms, FUN = function(x, dt) {
    sd(dt[[x]], na.rm = TRUE)
  }, dt = dt)

  return(priors)
}
