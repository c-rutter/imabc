.run_info_to_df <- function(targets, priors, iter, draw) {
  ##### Target Information #####
  # Target IDs
  target_ids <- attr(targets, which = "target_ids")
  target_ids <- unlist(lapply(attr(targets, "target_groups"), FUN = function(x) {
    c(rep(x, 2), rep(attr(targets[[x]], "target_ids"), times = 6))
  }), use.names = FALSE)
  # Target information
  target_info <- c(
    "name", "target", "lower_bound_current", "upper_bound_current", "lower_bound_stop", "upper_bound_stop"
  )
  target_info <- unlist(lapply(targets, FUN = function(x) {
    c("update", "group", rep(target_info, each = length(x[["names"]])))
  }))
  # Target values
  target_values <- unlist(lapply(attr(targets, "target_groups"), FUN = function(x) {
    c(
      attr(targets, "update")[x], x,
      targets[[x]][c("names", "targets", "lower_bounds_start", "upper_bounds_start", "lower_bounds_stop", "upper_bounds_stop")]
    )
  }), use.names = FALSE)

  ##### Prior Information #####
  # Prior IDs
  prior_ids <- names(priors)
  # Prior information
  prior_info <- c("imabc_min", "imabc_max", "empirical_sd", "distribution")
  prior_info <- unlist(lapply(attr(priors, "fun_inputs"), FUN = function(x) {
    c(prior_info, paste("fun", names(x), sep = "_"))
  }))
  # Prior values
  prior_values <- unlist(lapply(names(priors), FUN = function(x) {
    c(attr(priors, "mins")[x], attr(priors, "maxs")[x], attr(priors, "sds")[x], attr(priors, "distributions")[x],
      attr(priors, "fun_inputs")[x])
  }), use.names = FALSE)
  # Number of rows
  n_priors <- length(prior_ids)
  n_prior_info <- unlist(lapply(attr(priors, "fun_inputs"), FUN = function(x) { length(x) })) + 4
  n_prior_rows <- length(prior_info)

  # Final data
  IMABC <- c(rep("Run", 2), rep("Target", length(target_ids)), rep("Prior", n_prior_rows))
  ID <- c("current_iter", "last_draw", target_ids, rep(prior_ids, times = n_prior_info))
  INFO <- c("current_iter", "last_draw", target_info, prior_info)
  VALUE <- c(iter, draw, target_values, prior_values)

  return(data.frame(IMABC, ID, INFO, VALUE))
}
