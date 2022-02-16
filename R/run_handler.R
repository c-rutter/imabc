run_handler <- function(parms_to_run, all_parm_names, target_fun, custom_function = NULL, verbose = TRUE, ...) {
  i1 <- NULL

  # Extra inputs, currently includes targets and priors object
  other_inputs <- list(...)

  # If no custom backend function is provided run the basic method using whatever parallel backend is provided
  if (is.null(custom_function)) {
    iter_n <- nrow(parms_to_run)
    res <- foreach(i1 = 1:iter_n, .combine = combine_results(iter_n, verbose = verbose), .packages = "data.table") %dopar% {
      inp <- unlist(parms_to_run[i1, all_parm_names, with = FALSE])
      new_seed <- as.integer(unlist(strsplit(parms_to_run[["seed"]][i1], "_")))
      sim_target <- target_fun(
        inp,
        seed = new_seed,
        targets = other_inputs$targets,
        priors = other_inputs$priors
      )

      return(sim_target)
    }
    if (verbose) { cat("\n") }
  } else {
    res <- custom_function(parms_to_run, all_parm_names, target_fun, other_inputs)
  }

  return(res)
}
