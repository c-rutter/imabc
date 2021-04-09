#' @title Define Target Function(s)
#'
#' @description Helps the user build a target function that applies parameters to a function or set of functions. The
#' results of this function are then compared to the target goals to determine the goodness of fit of the parameters.
#'
#' @param targets targets object. Created using the define_targets function. Available to use within the target function(s)
#' See Details.
#' @param priors priors object. Created using the define_priors function. Available to use within the target function(s)
#' See Details.
#' @param FUN Optional function. If the user does not define target functions using define_targets, they can specify a
#' single function here. See Details.
#' @param use_seed logical. Should the algorithm set a seed before each set of parameters is sent to the target function(s).
#' The seed is set once for each set of parameters.
#'
#' @details
#' FUN:
#' While the user can define a function for each target they create using add_target, there may be times when the
#' user wants to have more control over how the functions are evaluated. For example, one target may be a function of
#' another target and a parameter. If the target functions are created using define_targets, the first function would have
#' to be evaluated twice. However, by using FUN, the user can create a function where `T1 = f(x1)` and `T2 = g(T1, x2)`.
#' This is especially helpful if the target functions take a long time to run.
#'
#' Specifying Parameters as Target Function Inputs:
#' Whether specifying target functions individually or through the FUN input, the inputs must follow a certain set of rules
#' in order to be applied correctly. It is important to remember that the input(s) are ultimately based on the values
#' specified in the priors object. Thus the target function(s) inputs will have to reference those parameters. This can
#' be done either as a single vector of values (e.g. `function(x) { ... }`), or individual inputs for each parameter
#' (e.g. `function(x1, x2) { ... }`). If a single vector is used, all parameters will be passed to the target function as
#' a named vector and the user can reference that vector by either using the parameter names (e.g. `x["x1"]`) or by using
#' the order a parameter was added in define_priors as the index number (e.g. `x[1]`). If individual inputs are given for
#' each parameter, then only the ones specified as inputs will be sent to the given target function. If this route is
#' taken the inputs into the target function must match the name(s) of the parameters being used. The single vector method
#' will be most useful when creating a single target function with FUN, while the individual inputs method is nice for
#' simple target functions added via defined_targets.
#'
#' Special Target Function Inputs:
#' Beyond specifying the parameters, the user may optionally choose to include the targets and priors objects as inputs
#' into the target function(s). These inputs must be specified as targets and priors respectively
#' (e.g. `function(x, targets, priors)`). They give you access to all the values defined and updated over the course of a
#' calibration. When using FUN, this can be especially useful if one target calculation is extremely fast while another
#' one is extremely slow; the user can simulate the first, check it against the target bounds, and choose to sidestep the
#' slower target function in order to get a speed boost for the entire calibration. To see what values are available for
#' each object use the names function on a recently created object for each class.
#'
#' Target Function Outputs:
#' While define_targets will handle the outputs appropriately for imabc, the user must be careful to do the same when
#' specifying a target function through FUN. It is required that the results of FUN is a vector whose length is equal
#' to the number of targets defined. If the vector is named, imabc will ensure that the order of the vector is correct
#' before performing any calculations with the results. The names must match the names given to targets in define_targets;
#' if you didn't provide names, you can find the generated names using `attr(targets, "target_names")`. If the vector is
#' not named, the order of the results in the vector must match the order the targets were added in define_targets.
#'
#' @return An imabc ready function.
#'
#' @examples
#' priors <- define_priors(
#'   x1 = add_prior(dist_base_name = "unif"),
#'   x2 = add_prior(density_fn = "dnorm", mean = 0.5, sd = 0.25)
#' )
#' targets <- define_targets(
#'   T1 = add_target(target = 0.5, starting_range = c(0.2, 0.9), stopping_range = c(0.48, 0.51)),
#'   add_target(
#'     target_name = "T2",
#'     target = 1.5, starting_range = c(1.0, 2.0), stopping_range = c(1.49, 1.51)
#'   )
#' )
#' fn1 <- function(x1, x2) { x1 + x2 + sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
#' fn2 <- function(x1, x2) { x1 * x2 + sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
#' fn <- function(x1, x2) {
#'   res <- c()
#'   res["T2"] <- fn2(x1, x2)
#'   res["T1"] <- fn1(x1, x2)
#'   return(res)
#' }
#' target_fun <- define_target_function(targets, priors, FUN = fn, use_seed = FALSE)
#'
#' @export
define_target_function <- function(targets, priors, FUN = NULL, use_seed = FALSE) {
  # Prepare inputs
  target_funs <- targets$target_functions

  # Target IDs that can be called
  expected_names <- attr(targets, "target_names")

  # Handle different ways target functions can be specified
  if (length(target_funs) > 0 & is.null(FUN)) {
    # Target functions added with just add_targets() --------------------------------------------------------------------
    # Check that all targets have a function
    stopifnot(
      "All targets must have a target function, or you must provide a function to define_target_function." =
        length(target_funs) == length(expected_names)
    )

    # Function applies inputs passed as a named vector to the target functions
    final_function1 <- function(x, seed = NULL, targets = NULL, priors = NULL) {
      if (use_seed && !is.null(seed)) {
        og_seed <- .GlobalEnv$.Random.seed
        assign(".Random.seed", seed, envir = .GlobalEnv, inherits = FALSE)
        if (is.null(og_seed)) {
          on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
        } else {
          on.exit(assign('.Random.seed', og_seed, envir = .GlobalEnv), add = TRUE)
        }
      }

      res <- unlist(lapply(expected_names, function(idx, y, x) {
        # Which special functions have been defined
        internal_opts <- c("targets", "priors")
        # All inputs into a given function
        reg_inp <- formalArgs(y[[idx]])
        # The ones that are generated internally
        special <- list(targets = targets, priors = priors)
        special <- special[names(special) %in% reg_inp]
        # The ones that the user defined
        reg_inp <- reg_inp[!reg_inp %in% internal_opts]
        if (length(reg_inp) > 1) {
          # Check names of inputs are parameters
          stopifnot(
            "All target function inputs except for targets and priors must have defined values in the priors object." =
              all(reg_inp %in% names(priors))
          )

          # If target function has parameters as inputs
          ans <- do.call(y[[idx]], as.list(c(x[reg_inp], special)))
        } else if (length(reg_inp) == 1) {
          # If target function expects a single vector
          ans <- do.call(y[[idx]], as.list(c(list(x), special)))
        } else {
          # If target is fixed
          ans <- y[[idx]]()
        }

        return(ans)
      }, y = target_funs, x = x))
      names(res) <- expected_names

      return(res)
    }
    final_function <- final_function1
  } else if (length(target_funs) == 0 & !is.null(FUN)) { # ! length(target_funs) > 0 & is.null(FUN)
    # Target functions added with just FUN ------------------------------------------------------------------------------
    final_function2 <- function(x, seed = NULL, targets = NULL, priors = NULL) {
      if (use_seed && !is.null(seed)) {
        og_seed <- .GlobalEnv$.Random.seed
        assign(".Random.seed", seed, envir = .GlobalEnv, inherits = FALSE)
        if (is.null(og_seed)) {
          on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
        } else {
          on.exit(assign('.Random.seed', og_seed, envir = .GlobalEnv), add = TRUE)
        }
      }
      # Which special functions have been defined
      internal_opts <- c("targets", "priors")
      # All inputs into a given function
      reg_inp <- formalArgs(FUN)
      # The ones that are generated internally
      special <- list(targets = targets, priors = priors)
      special <- special[names(special) %in% reg_inp]
      # The ones that the user defined
      reg_inp <- reg_inp[!reg_inp %in% internal_opts]

      # The ones that the user defined
      if (length(reg_inp) > 1) {
        # Check names of inputs are parameters
        stopifnot(
          "All target function inputs except for targets and priors must have defined values in the priors object." =
            all(reg_inp %in% names(priors))
        )

        # If target function has parameters as inputs
        ans <- do.call(FUN, as.list(c(x[reg_inp], special)))
      } else if (length(reg_inp) == 1) {
        # If target function expects a single vector
        ans <- do.call(FUN, as.list(c(list(x), special)))
      } else {
        # If targets are fixed
        ans <- FUN()
      }

      return(ans)
    }
    final_function <- final_function2
  } else if (length(target_funs) > 0 & !is.null(FUN)) { # ! length(target_funs) == 0 & !is.null(FUN)
    # Target functions added through add_targets and FUN ----------------------------------------------------------------
    stop("Adding individual target functions and a main target function is not supported.")

  } # ! length(target_funs) > 0 & !is.null(FUN)

  # Add imabc object class to function
  final_function <- structure(final_function, class = c(class(final_function), "imabc"))

  return(final_function)
}
