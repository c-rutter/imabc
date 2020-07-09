get_log_prior_d <- function(parms, parm_names, priors) {
  log_prior_d <- Reduce(`+`, lapply(parm_names, FUN = function(x, dt, priors) {
    if (!is.na(attr(priors, "means")[x])) {
      calc <- log(
        dtruncnorm(
          x = unlist(dt[, x, with = FALSE]),
          a = attr(priors, "mins")[x],
          b = attr(priors, "maxs")[x],
          mean = attr(priors, "means")[x],
          sd = attr(priors, "sds")[x]
        )
      )
    } else {
      calc <- rep(0L, nrow(dt))
    }

    return(calc)
  }, dt = parms, priors = priors))
  return(log_prior_d)
}
