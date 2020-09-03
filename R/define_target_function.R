define_target_function <- function(targets, priors, FUN = NULL, use_seed = TRUE) {
  # Prepare inputs
  target_funs <- attr(targets, "target_functions")

  # Target IDs that can be called
  expected_names <- attr(targets, "target_ids")

  # Handle different ways target functions can be specified
  if (length(target_funs) > 0 & is.null(FUN)) {
    ### Target functions are added separately using add_targets()
    # Check for proper number of functions
    stopifnot(
      "Number of target functions must match the number of targets" = length(target_funs) == length(expected_names)
    )
    # Function applies inputs passed as a named vector to the target functions
    final_function <- function(x, seed = NULL, lower_bounds = NULL, upper_bounds = NULL) {
      if (use_seed && !is.null(seed)) {
        set.seed(seed)
      }
      res <- unlist(lapply(names(target_funs), function(idx, y, x) {
        # Which special functions have been defined
        internal_opts <- c("seed", "lower_bound", "upper_bound")
        lower_bound <- if (is.null(lower_bounds)) as.null() else lower_bounds[[idx]]
        upper_bound <- if (is.null(upper_bounds)) as.null() else upper_bounds[[idx]]
        # All inputs into a given function
        reg_inp <- formalArgs(y[[idx]])
        # The ones that are generated internally
        special <- c(seed = seed, lower_bound = lower_bound, upper_bound = upper_bound)
        special <- special[names(special) %in% reg_inp]
        # The ones that the user defined
        reg_inp <- reg_inp[!reg_inp %in% internal_opts]
        if (length(reg_inp) > 1) {
          # Check names of inputs are parameters
          stopifnot(
            "All target function inputs except for seed, lower_bound, and upper_bound must have defined values in the priors object." = all(reg_inp %in% names(priors))
          )

          # If target function has parameters as inputs
          ans <- do.call(y[[idx]], as.list(c(x[reg_inp], special)))
        } else if (length(reg_inp) == 1) {
          # If target function expects a single vector
          ans <- as.numeric(do.call(y[[idx]], as.list(c(list(x), special))))
        } else {
          # If target is fixed
          ans <- y[[idx]]()
        }

        return(ans)
      }, y = target_funs, x = x))

      return(res)
    }

  } else if (length(target_funs) == 0 & !is.null(FUN)) { # ! length(target_funs) > 0 & is.null(FUN)
    ### One large target function is added
    final_function <- function(x, seed = NULL, lower_bounds = NULL, upper_bounds = NULL) {
      if (use_seed && !is.null(seed)) {
        set.seed(seed)
      }

      # Which special functions have been defined
      internal_opts <- c("seed", "lower_bounds", "upper_bounds")
      # All inputs into a given function
      reg_inp <- formalArgs(FUN)
      # The ones that are generated internally
      special <- list(seed = seed, lower_bounds = lower_bounds, upper_bounds = upper_bounds)
      special <- special[names(special) %in% reg_inp]
      # The ones that the user defined
      reg_inp <- reg_inp[!reg_inp %in% internal_opts]

      # The ones that the user defined
      if (length(reg_inp) > 1) {
        # Check names of inputs are parameters
        stopifnot(
          "All target function inputs except for seed, lower_bounds, and upper_bounds must have defined values in the priors object." = all(reg_inp %in% names(priors))
        )

        # If target function has parameters as inputs
        ans <- do.call(FUN, as.list(c(x[reg_inp], special)))
      } else if (length(reg_inp) == 1) {
        # If target function expects a single vector
        ans <- as.numeric(do.call(FUN, as.list(c(list(x), special))))
      } else {
        # If targets are fixed
        ans <- FUN()
      }

      return(ans)
    }


  } else if (length(target_funs) > 0 & !is.null(FUN)) { # ! length(target_funs) == 0 & !is.null(FUN)
    ### One large target function and multiple target functions are added
    stop("Not worked on yet")

  } # ! length(target_funs) > 0 & !is.null(FUN)

  return(final_function)
}
