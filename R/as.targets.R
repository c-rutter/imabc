#' @export
as.targets <- function(
  df,
  groups_var = "target_groups", names_var = "target_names", targets_var = "targets",
  current_lower_bounds_var = "current_lower_bounds", current_upper_bounds_var = "current_upper_bounds",
  stopping_lower_bounds_var = "stopping_lower_bounds", stopping_upper_bounds_var = "stopping_upper_bounds") {

  # Check column names are length one and found in data.frame


  # Add targets
  indi_targs <- lapply(1:nrow(df), FUN = function(i1, dta) {
    # add target information
    add_target(
      target_name = dta[[names_var]][i1],
      target = dta[[targets_var]][i1],
      starting_range = c(dta[[current_lower_bounds_var]][i1], dta[[current_upper_bounds_var]][i1]),
      stopping_range = c(dta[[stopping_lower_bounds_var]][i1], dta[[stopping_upper_bounds_var]][i1])
    )
  }, dta = df)

  # Create groups if they exist or rename indi_targs to group_targs
  if (groups_var %in% colnames(df) && !all(is.na(df[[groups_var]]))) {
    # Get the indeces of all the groupped targets and individual targets into lists
    groupings <- split(1:nrow(df), df[[groups_var]])
    individuals <- as.list(which(is.na(df[[groups_var]])))
    # Create a final list in the proper order for all groups and individual targets
    group_targs <- c(groupings, individuals)
    group_targs <- group_targs[order(do.call(c, lapply(group_targs, min)))]

    # For each component of the final list, either group the targets are simply add them to the list
    for (i1 in 1:length(group_targs)) {
      # Iter info
      group_name <- names(group_targs[i1])
      group_idx <- group_targs[[i1]]
      if (group_name != "") { # Group targets
        group_targs[[i1]] <- do.call(group_targets, c(indi_targs[group_idx], group_name = group_name))
      } else { # Add individual targets
        group_targs[[i1]] <- indi_targs[[group_idx]]
      }
    }
  } else {
    group_targs <- indi_targs
  }

  # Define all targets
  final_targ_list <- do.call(define_targets, group_targs)

  return(final_targ_list)
}
