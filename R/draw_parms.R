#' @export
draw_parms <- function(n_add, mu, sigma, priors_list, targets_list) {
  # independently draw parameters values, with the exception of beta2, sampled conditional on beta2
  # parm.priors columns are: parm,norm.dist,a,b,c,d
  # parm: parameter name,
  # a,b: hyper-parameters corresponding to N(a,b) or U(a,b)
  # c,d: the prior range, [c,d] for U(a,b) we expect a=c and b=d

  # Make sure mu is a matrix
  if (is.vector(mu)) {
    mu_name <- names(mu)
    mu <- matrix(mu, byrow = TRUE, nrow = 1)
    colnames(mu) <- mu_name
  } # is.vector(mu)

  # Prepare information for calculation
  n_centers <- nrow(mu)
  n_parms <- ncol(mu)
  n_row_final <- n_add*n_centers
  parm_names <- names(priors_list)
  lower_bounds <- attr(priors_list, "mins")
  upper_bounds <- attr(priors_list, "maxs")

  # Initialize results data.table
  draws <- data.table(matrix(0, nrow = n_row_final, ncol = n_parms))
  # CM NOTE: Not used
  # u_draws <- matrix(runif(2*n_row_final), ncol = 2, nrow = n_row_final)
  setnames(draws, parm_names)
  # CM NOTE: Not worked on yet
  # Range.p10mmIn10yrs=unlist(calib.targets[["Range.p10mmIn10yrs"]])

  # Create sample from parameter info and truncated normal
  # CM NOTE: This method does not deal with Range.p10mmIn10yrs
  draws[, center_id := rep(1:n_centers, each = n_add)]
  draws[, (parm_names) := lapply(parm_names, FUN = function(x, n, mu, sigma, lower, upper) {
    rtruncnorm(
      n, # Number of draws per center
      a = lower[x], # Lower bound of a given parameter
      b = upper[x], # Upper bound of a given parameter
      mean = mu[center_id, x], # Mean of each center
      sd = sigma[center_id, x] # Standard Deviation of each center
    )
  }, n = n_add, mu = mu, sigma = sigma, lower = lower_bounds, upper = upper_bounds), by = center_id]
  draws$center_id <- NULL
  # CM NOTE: Old method used loop and handled Range.p10mmIn10yrs differently
  # for (center_i1 in 1:n_centers) {
  #   row_range = ((center_i1 - 1)*n_add + 1):(center_i1*n_add)
  #   for(i.parm in 1:n.parms){
  #     if(parm.priors[i.parm,'prior.dist']==3){
  #       beta1 = gsub("2", "1", parm.names[i.parm])
  #       unif.lim=parm.priors[parm.priors$parm==parm.priors[i.parm,'parm'],c('a','b')]
  #
  #       lower.bound=beta2.min(draws[[beta1]][row_range],
  #                             Range.p10mmIn10yrs,
  #                             unif.lim)
  #       upper.bound=beta2.max(draws[[beta1]][row_range],
  #                             Range.p10mmIn10yrs,
  #                             unif.lim)
  #
  #     }else{
  #       lower.bound=parm.priors$c[i.parm]
  #       upper.bound=parm.priors$d[i.parm]
  #     }
  #
  #     draws[row_range,parm.names[i.parm] := rtruncnorm(n_add,
  #                                                      a=lower.bound,
  #                                                      b=upper.bound,
  #                                                      mean=mu[center_i1,i.parm],
  #                                                      sd=sigma[center_i1,i.parm])]
  #
  #   } # for(i.parm in 1:n.parms)
  # } # over centers

  return(draws)
}
