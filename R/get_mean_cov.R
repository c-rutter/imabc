get_mean_cov <- function(iter, mu, sd, center, B_in, parm_names) {
  step <- NULL

  # Convert mu to a matrix if it is currently a vector
  if (is.vector(mu) & length(parm_names) > 1) {
    save_names <- names(mu)
    mu <- matrix(mu, byrow = TRUE, nrow = 1)
    sd <- matrix(sd, byrow = TRUE, nrow = 1)
    colnames(mu) <- save_names
  } else if (is.vector(mu) & length(parm_names) == 1) {
    mu <- matrix(mu, byrow = TRUE, ncol = 1)
    colnames(mu) <- parm_names
  }
  mu <- data.frame(mu)
  sd <- data.frame(sd)

  # Get data size
  n_parm <- length(parm_names)
  n_centers <- nrow(mu)

  # Only use means that are being calibrated
  mu_use <- mu[, which(names(mu) %in% parm_names), drop = FALSE]

  # Handle standard deviation in similar manner to means
  names(sd) <- names(mu)
  sd_use <- sd[, which(names(mu) %in% parm_names), drop = FALSE]

  # Initialize results holder
  x_out <- setnames(
    data.table(matrix(
      NA_real_,
      nrow = n_centers*(n_parm + 1),
      ncol = n_parm)),
    parm_names
  )

  # Store information in appropriate rows
  for (i1 in 1:n_centers) {
    x_out[((i1 - 1)*(n_parm + 1) + 1), ] <- mu_use[i1, ]
    if (n_parm > 1) {
      x_out[((i1 - 1)*(n_parm + 1) + 2):(i1*(n_parm + 1)), ] <- setnames(data.frame(diag(sd_use[i1, ])), parm_names)
    } else {
      x_out[((i1 - 1)*(n_parm + 1) + 2):(i1*(n_parm + 1)), ] <- setnames(data.frame(sd_use[i1, ]), parm_names)
    }
  }

  # Finish rest of tracking information
  x_out$iter <- iter
  x_out$step <- rep(1:n_centers, each = (1 + n_parm))
  x_out$center <- rep(center, each = (1 + n_parm))
  x_out$parm <- rep(0:(n_parm), n_centers)

  # Join with in_range rows
  setkey(x_out, iter, step)
  setkey(B_in, iter, step)
  x_withB <- x_out[B_in]
  setcolorder(x_withB, c("iter", "step", "center", "B.in", "parm", parm_names))

  return(x_withB)
}
