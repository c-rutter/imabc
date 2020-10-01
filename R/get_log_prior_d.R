#' @export
get_log_prior_d <- function(parms, parm_names, priors) {
  log_prior_d <- Reduce(`+`, lapply(parm_names, FUN = function(x, dt, priors) {
    # Uniform adds no distance
    if (attr(priors, "distributions")[x] == "unif") {
      calc <- rep(0L, nrow(dt))
    } else {
      calc <- log(
        priors[[x]]$density_function(unlist(dt[, x, with = FALSE]))
      )
    }

    return(calc)
  }, dt = parms, priors = priors))



  return(log_prior_d)
}
