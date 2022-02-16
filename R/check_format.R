check_format <- function(imabc_obj, tracking_dt, dist_dt = NULL) {
  UseMethod("check_format")
}

check_format.targets <- function(imabc_obj, tracking_dt, dist_dt = NULL) {
  # Pull names:
  #   expected based on obj vs actual names found in good_*
  #   target names and target distance names
  exp_tnames_names <- attr(imabc_obj, which = "target_names")
  act_tnames_names <- colnames(tracking_dt)
  exp_gnames_names <- unique(attr(imabc_obj, which = "target_groups"))
  act_gnames_names <- colnames(dist_dt)

  # Remove non-target/distance names from data.tables
  standard_names <- c("iter", "draw", "step", "tot_dist", "n_good")
  act_tnames_names <- act_tnames_names[!act_tnames_names %in% standard_names]
  act_gnames_names <- act_gnames_names[!act_gnames_names %in% standard_names]

  # Errors
  stopifnot(
    # Make sure all object names appear in data.tables
    "Targets object has target names not found in previous results" =
      all(exp_tnames_names %in% act_tnames_names),
    # Make sure all data.table names appear in object
    "Previous results has target names not found in Targets" =
      all(act_tnames_names %in% exp_tnames_names),

    # Make sure all object names appear in data.tables
    "Targets object has distance names not found in previous results" =
      all(exp_gnames_names %in% act_gnames_names),
    # Make sure all data.table names appear in object
    "Previous results has distance names not found in Targets" =
      all(act_gnames_names %in% exp_gnames_names)
  )
}

check_format.priors <- function(imabc_obj, tracking_dt) {
  # Pull names:
  #   expected based on obj vs actual names found in good_*
  exp_pnames_names <- names(imabc_obj)
  act_pnames_names <- colnames(tracking_dt)

  # Remove non-target/distance names from data.tables
  standard_names <- c("iter", "draw", "step", "seed", "scaled_dist", "sample_wt")
  act_pnames_names <- act_pnames_names[!act_pnames_names %in% standard_names]

  # Errors
  stopifnot(
    # Make sure all object names appear in data.tables
    "Priors object has names not found in previous results" =
      all(exp_pnames_names %in% act_pnames_names),
    # Make sure all data.table names appear in object
    "Previous results has names not found in Priors" =
      all(act_pnames_names %in% exp_pnames_names)
  )
}
