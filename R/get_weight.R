get_weight <- function(parms, parm_names, priors, mixture_file, n) {
  # Calculate log(prior density) for non-uniformly distributed parameters
  log_prior_d <- get_log_prior_d(parms = parms, parm_names = parm_names, priors = priors)
  # CM NOTE: See get_mix_dist note
  # Calculate multivariate Normal Density
  sampling_d <- get_sampling_d(parms = parms, parm_names = parm_names, mixture_file = mixture_file)
  # Calculate final weights
  mix_wt <- 1/log1p(sampling_d/exp(log_prior_d)/n)
  mix_wt <- mix_wt/sum(mix_wt)

  return(mix_wt)
}
