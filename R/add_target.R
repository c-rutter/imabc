add_target <- function(target, starting_range, stopping_range, FUN = NULL, target_name = NULL) {
  # Check inputs are appropriate ----------------------------------------------------------------------------------------
  stopifnot(
    "target must be numeric" = is.numeric(target),
    "starting_range must be numeric" = is.numeric(starting_range),
    "stopping_range must be numeric" = is.numeric(stopping_range),
    "starting_range must be vector of length 2" = length(starting_range) == 2,
    "stopping_range must be vector of length 2" = length(stopping_range) == 2,
    "starting_range must be of the form c(min, max) and must satisfy min < max." = starting_range[1] < starting_range[2],
    "stopping_range must be of the form c(min, max) and must satisfy min < max." = stopping_range[1] < stopping_range[2],
    "starting_range should contain stopping_range" = {
      starting_range[1] <= stopping_range[1] &
      starting_range[2] >= stopping_range[2]
    },
    "stopping_range should contain target" = in_range(target, stopping_range[1], stopping_range[2]),
    "FUN must be a function if provided" = is.null(FUN) || is.function(FUN)
  )

  return(
    list(
      target_name = target_name,
      target = target,
      low_bound_start = starting_range[1],
      up_bound_start = starting_range[2],
      low_bound_stop = stopping_range[1],
      up_bound_stop = stopping_range[2],
      FUN = FUN
    )
  )
}
