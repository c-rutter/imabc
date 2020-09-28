run_handler <- function(parms_to_run, all_parm_names, target_fun, custom_function = NULL, ...) {
  other_inputs <- list(...)

  if (is.null(custom_function)) {
    res <- foreach(i1 = 1:nrow(parms_to_run), .combine = combine_results) %dopar% {
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
  } else {
    res <- custom_function(parms_to_run, all_parm_names, target_fun, other_inputs)
  }

  return(res)
}
