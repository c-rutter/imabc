get_mix_dist <- function(parm_names, mixture_file) {
  iter <- step <- center <- parm_num <- NULL
  # CM NOTE: At the moment, imabc is only able to handle an object being passed, which means this is just returning
  #  mean_cov which is passed to it. Should clarify that all mean_cov needs to be row bound together prior to a continuing
  #  run in order for them to all be used.
  if (is.character(mixture_file)) {
    non_parm_vars <- c("iter", "step", "center", "B.in", "parm")
    n_parms_now <- length(parm_names)
    for (i1 in 1:length(mixture_file)) {
      # Read i1'th mixture file
      dta_i1 <- data.table(read.table(
        file = mixture_file[i1],
        header = TRUE,
        sep = ",",
        na.strings = "NA"
      ))
      n_parms_i1 <- ncol(dta_i1) - length(non_parm_vars)
      # Pull parms in file
      parm_order_i1 <- names(dta_i1)[!names(dta_i1) %in% non_parm_vars]

      # If parameters from previous run are not being used now, error out
      # CM NOTE: This was previously just a "break" but I think it should be an error that stops the entire run
      if (!(all(parm_order_i1 %in% parm_names))) {
        stop(paste0(
          "error, parameters varied in previous calibration are not included: ",
          paste(parm_order_i1[!(parm_order_i1 %in% parm_names)], collapse = ", ")
        ))

      }

      # If there are new parameters in current run, set their values to NA in old mixture files
      if (any(!(parm_names %in% parm_order_i1))) {
        # CM NOTE: From original code. Not used anywhere else so not running. Should remove
        # parm_names_in <- parm_names[parm_names %in% parm_order_i1]
        parm_names_out <- parm_names[!(parm_names %in% parm_order_i1)]

        # Set parms not included to missing: allows stacking of mean_cov
        dta_i1[, (parm_names_out) := NA]

      }

      # Find the correct position of each variable relative to the new order
      # CM NOTE: This probably isn't necessary. Could do it based on column name rather than column position or use
      #   match instead of which
      new_parm_num <- rep(0, n_parms_i1)
      for (i2 in 1:n_parms_i1) {
        new_parm_num[i2] <- which(parm_names == parm_order_i1[i2])

      }
      setcolorder(dta_i1, c(non_parm_vars, parm_names))

      # CM NOTE: Need to work this, but if this is doing what I think it is, it is a weird/inefficient way
      # Give each parm a unique ID
      n_mix <- length(unique(10*max(dta_i1$step)*dta_i1$iter + dta_i1$step))
      dta_i1$parm_num <- rep(c(0, new_parm_num), n_mix)
      setkey(dta_i1, iter, step, center, parm_num)
      dta_i1$parm <- dta_i1$parm_num
      dta_i1$parm_num <- NULL

      # If this is the first file being loaded store it, else append it
      if (i1 == 1) {
        mean_cov <- dta_i1

      } else { # i1 == 1
        # keep info up to first iter in next file
        first_iter <- dta_i1$iter[1]
        n_keep <- (n_parms_now + 1)*(first_iter - 1)
        mean_cov <- mean_cov[iter <= n_keep, ]
        mean_cov <- rbind(mean_cov, dta_i1)

      } # ! i1 == 1

    } # i1 in 1:length(mixture_file)
  } else { # is.character(mixture_file)
    # CM NOTE: This is not set up to handle multiple mixture_file as might be done above
    mean_cov <- mixture_file

  }
  return(mean_cov)
}
