#' @title Add Target Information
#'
#' @description Creates a target object.
#'
#' @param target numeric(1). The value a target function is aiming for.
#' @param starting_range numeric(2). The initial range of values imabc will consider as good when testing simulated parameters.
#' @param stopping_range numeric(2). The range of values a target function's simulated value must be within to be considered calibrated.
#' @param target_name character(1) Optional. The name of the target.
#' @param FUN function Optional. The function that takes parameters and calculated the target value. See Details.
#'
#' @details
#' ## Target values:
#' When specifying values the following condition must always hold true:
#' ```
#' starting_range[1] <= stopping_range[1] <= target <= stopping_range[2] <= starting_range[2]
#' ```
#' As imabc simulates parameters, it will test them using the target function(s) against the starting range. Parameters whose
#' values fall within the starting range will be kept through to the next iteration and will be used to generate new parameters
#' for testing. As the parameters get better at falling withing the initial range, imabc will reduce the valid range of
#' targets to be considered. Once the current valid range matches the stopping range the algorithm will no longer reduce
#' the valid range of target values.
#'
#' ## Target function:
#' There are multiple ways to specify a target function. One way is to attach it to the target object using the FUN input.
#' The inputs to the target function can either be a single object (e.g. function(x)) or several objects whose name is
#' equal to the parameter they represent (e.g. function(x1, x2)). If a single object is used, the user can assume that
#' a name vector with all parameters specified in the priors object will be passed to the function and the order of the
#' vector will be the same as the order in which they were specified with define_priors. For example, if someone specified
#' three parameters named x1, x3, and x2 respectively then the following specifications would all be equivalent:
#' ```
#' function(x1, x3) { x1 + x3 } == function(x) { x["x1"] + x["x3"] } == function(x) { x[1] + x[2] }
#' ```
#'
#' Additionally, for more complex situations the user may also reference the targets object and priors object within a
#' target function but they must specify them as inputs (e.g. function(x, targets, priors)) and use the objects as they
#' exist within those objects. See define_target_function for more details and other ways to specify the target function.
#' @md
#'
#' @return A target imabc object that can be passed to group_targets or define_targets.
#' @export
#'
#' @examples
#' add_target(target = 0.5, starting_range = c(0.2, 0.9), stopping_range = c(0.48, 0.51))
#' add_target(
#'   target_name = "target_1",
#'   target = 1.5, starting_range = c(1.0, 2.0), stopping_range = c(1.49, 1.51),
#'   FUN = function(x1, x2) { x1 + x2 + rnorm(1, 0, 0.01) }
#' )
add_target <- function(target, starting_range, stopping_range, target_name = NULL, FUN = NULL) {
  # Check inputs are appropriate ----------------------------------------------------------------------------------------
  stopifnot(
    "target must be numeric" = is.numeric(target),
    "starting_range must be numeric" = is.numeric(starting_range),
    "stopping_range must be numeric" = is.numeric(stopping_range),
    "starting_range must be vector of length 2" = length(starting_range) == 2,
    "stopping_range must be vector of length 2" = length(stopping_range) == 2,
    "starting_range must be of the form c(min, max) and must satisfy min < max." = starting_range[1] <= starting_range[2],
    "stopping_range must be of the form c(min, max) and must satisfy min < max." = stopping_range[1] <= stopping_range[2],
    "starting_range should contain stopping_range" = {
      starting_range[1] <= stopping_range[1] &
      starting_range[2] >= stopping_range[2]
    },
    "stopping_range should contain target" = stopping_range[1] <= target & target <= stopping_range[2],
    "FUN must be a function if provided" = is.null(FUN) || is.function(FUN)
  )

  target <- structure(
    list(
      target_name = target_name,
      target = target,
      current_lower_bound = starting_range[1],
      current_upper_bound = starting_range[2],
      stopping_lower_bound = stopping_range[1],
      stopping_upper_bound = stopping_range[2],
      FUN = FUN
    ),
    class = c("target")
  )

  return(target)
}
