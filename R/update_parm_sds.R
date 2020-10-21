update_parm_sds <- function(priors, dt, parms) {
  attr(priors, "sds") <- sapply(parms, FUN = function(x, dt) {
    sd(dt[[x]], na.rm = TRUE)
  }, dt = dt)

  return(priors)
}
