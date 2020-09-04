run_handler <- function(parms_to_run, target_fun, custom_function = NULL, ...) {
  other_inputs <- list(...)

  if (is.null(custom_function)) {
    res <- foreach(i1 = 1:nrow(parms_to_run), .combine = combine_results) %dopar% {
      inp <- unlist(parms_to_run[i1, other_inputs$all_parm_names, with = FALSE])
      sim_target <- target_fun(inp, lower_bounds = other_inputs$lower_bounds, upper_bounds = other_inputs$upper_bounds)

      return(sim_target)
    }
  } else {
    res <- custom_function(parms_to_run, target_fun, other_inputs)
  }

  return(res)
}
