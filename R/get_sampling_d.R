get_sampling_d <- function(parms, parm_names, mixture_file) {
  parm <- iter <- step <- NULL
  # CM NOTE: See get_mix_dist note
  mean_cov <- get_mix_dist(parm_names, mixture_file)

  # Get information on values to use
  n_mix <- nrow(mean_cov[parm == 0, ])
  B_draws <- unique(mean_cov[, c("iter", "step", "B.in"), with = FALSE])

  # Initialize matrix of parm info only
  parm_mat <- as.matrix(sapply(parms[, parm_names, with = FALSE], as.numeric))
  # Set to a n_parm x 1 matrix when n_good == 1
  if (nrow(parms) == 1) { parm_mat <- t(parm_mat) }

  # Loop over all relevant info and sum
  sum_H <- rep(0, nrow(parms))
  for (i1 in 1:n_mix) {
    iter_i1 <- B_draws$iter[i1]
    step_i1 <- B_draws$step[i1]
    B_i1 <- B_draws[iter == iter_i1 & step == step_i1, ]$B.in

    # Pull the mean
    center_i1 <- as.vector(as.numeric(
      mean_cov[iter == iter_i1 & step == step_i1 & parm == 0, parm_names, with = FALSE]
    ))
    # Pull the standard deviation
    sigma_i1 <-  as.matrix(sapply(
      mean_cov[iter == iter_i1 & step == step_i1 & parm > 0, parm_names, with = FALSE],
      as.numeric
    ))

    # Only use calibrated parameters
    is_calib <- (1:length(center_i1))[!is.na(center_i1)]
    center_i1 <- center_i1[is_calib]
    sigma_i1 <- sigma_i1[, is_calib]

    # If the variance matrix isn't square send an error
    stopifnot("Variance Matrix is not square" = nrow(sigma_i1) == ncol(sigma_i1))

    # Calculate the Multivariate Normal Density and add to previous results
    if (length(center_i1) == 1 && is.null(nrow(sigma_i1))) {
      sum_H <- sum_H + B_i1*dnorm(x = parm_mat[, is_calib], mean = center_i1, sd = sigma_i1)
    } else {
      sum_H <- sum_H + B_i1*dMvn(X = parm_mat[, is_calib], mu = center_i1, Sigma = sigma_i1)
    }
  }

  return(sum_H)
}
