#' @export
get_weight <- function(parms, parm_names, priors, mixture_file, n) {

  # CM NOTE: Should it be possible for the parms to be outside of the min/max of the prior info?
  # calculate log(prior density) for normally distributed paramters
  log_prior_d <- get_log_prior_d(parms = parms, parm_names = parm_names, priors = priors)
  sampling_d <- get_sampling_d(parms = parms, parm_names = parm_names, mixture_file = mixture_file)
  mix_wt <- 1/log1p(sampling_d/exp(log_prior_d)/n)
  mix_wt <- mix_wt/sum(mix_wt)

  return(mix_wt)
}
