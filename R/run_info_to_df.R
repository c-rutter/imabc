#' @export
.run_info_to_df <- function(targets, priors, iter, draw) {
  ##### Target Information #####
  # Pull values
  target_values <- .list_vector_to_df(targets, "targets")
  current_lower_bounds <- .list_vector_to_df(targets, "current_lower_bounds")
  current_upper_bounds <- .list_vector_to_df(targets, "current_upper_bounds")
  stopping_lower_bounds <- .list_vector_to_df(targets, "stopping_lower_bounds")
  stopping_upper_bounds <- .list_vector_to_df(targets, "stopping_upper_bounds")
  target_groups <- .list_vector_to_df(targets, "target_groups", TRUE)
  target_groups$VALUE[!attr(targets, "grouped_targets")] <- NA
  update <- .list_vector_to_df(targets, "update", TRUE)
  update$VALUE <- TRUE
  # Join into one data.frame
  targ_df <- rbind(
    target_groups, update, target_values,
    current_lower_bounds, current_upper_bounds,
    stopping_lower_bounds, stopping_upper_bounds
  )
  # Order
  targ_df$INFO <- factor(targ_df$INFO, levels = c(
    "target_groups", "update", "targets",
    "current_lower_bounds", "current_upper_bounds", "stopping_lower_bounds", "stopping_upper_bounds"
    ))
  targ_df <- targ_df[order(targ_df$ID, targ_df$INFO), ]

  ##### Prior Information #####
  # Pull values
  imabc_min <- .list_vector_to_df(priors, "mins", TRUE)
  imabc_min$INFO <- paste("imabc", imabc_min$INFO, sep = "_")
  imabc_max <- .list_vector_to_df(priors, "maxs", TRUE)
  imabc_max$INFO <- paste("imabc", imabc_max$INFO, sep = "_")
  empirical_sd <- .list_vector_to_df(priors, "sds", TRUE)
  empirical_sd$INFO <- paste("imabc", empirical_sd$INFO, sep = "_")
  distribution <- .list_vector_to_df(priors, "distribution", TRUE)
  fun_in <- attr(priors, "fun_inputs")
  fun_input <- do.call(rbind, lapply(seq_along(fun_in), FUN = function(x, fun_in, ids) {
    if (is.null(names(fun_in[[x]]$fun_inputs))) {
      data.frame(ID = ids[x], INFO = NA)
    } else {
      data.frame(ID = ids[x], INFO = paste("fun", names(fun_in[[x]]$fun_inputs), sep = "_"))
    }
  }, fun_in = fun_in, ids = names(fun_in)))
  fun_values <- do.call(rbind, lapply(seq_along(fun_in), FUN = function(x, fun_in, ids) {
    if (is.null(names(fun_in[[x]]$fun_inputs))) {
      data.frame(VALUE = NA)
    } else {
      data.frame(VALUE = fun_in[[x]]$fun_inputs)
    }
  }, fun_in = fun_in, ids = names(fun_in)))
  fun_inputs <- cbind(TYPE = "priors", fun_input, fun_values)
  fun_inputs <- fun_inputs[!is.na(fun_inputs$INFO), ]
  # Join into one data.frame
  prio_df <- rbind(
    imabc_min, imabc_max, empirical_sd,
    distribution, fun_inputs
  )
  # Order
  main_items <- c("imabc_mins", "imabc_maxs", "imabc_sds", "distribution")
  other_items <- setdiff(unique(prio_df$INFO), main_items)
  prio_df$ID <- factor(prio_df$ID, levels = names(priors))
  prio_df$INFO <- factor(prio_df$INFO, levels = c(main_items, other_items))
  prio_df <- prio_df[order(prio_df$ID, prio_df$INFO), ]

  ##### Admin Information #####
  admi_df <- data.frame(
    TYPE = "Run",
    ID = c("current_iteration", "last_draw"),
    INFO = c("current_iteration", "last_draw"),
    VALUE = c(iter, draw)
  )

  # Final data
  final_dta <- rbind(admi_df, targ_df, prio_df)
  rownames(final_dta) <- NULL

  return(final_dta)
}

#' @export
.list_vector_to_df <- function(list_vector, info, attribute = FALSE) {
  na <- deparse(substitute(list_vector))
  if (attribute) {
    vec <- attr(list_vector, info)
    data.frame(TYPE = na, ID = names(vec), INFO = info, VALUE = vec)
  } else {
    data.frame(TYPE = na, ID = names(list_vector[[info]]), INFO = info, VALUE = list_vector[[info]])
  }
}
