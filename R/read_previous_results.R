#' @title Read Previous Results
#'
#' @description Searches the files found in path for the files saved by an imabc run and reads them into the current environment.
#'
#' @param path character(1). The location of files saved during a previous run.
#' @param tag Optional character(1). If multiple runs have been saved to a single path, provide the tag that differentiates them.
#'
#' @note tag is required if multiple sets of results are stored in a single location.
#' @note While the output of this function are necessary for a restart, the user does not need to use this function for
#'   restarting a calibration. imabc() handles this function for the user via the previous_results_* input options.
#'
#' @return A list with a priors object, a targets object, and a list of data.frames needed to continue a calibration with
#'   imabc().
#'
#' @export
read_previous_results <- function(path, tag = NULL) {
  old_saf <- getOption("stringsAsFactors")
  if (!is.null(old_saf)) {
    options(stringsAsFactors = FALSE)
    on.exit(options(stringsAsFactors = old_saf), add = TRUE)
  }
  # Get files in path
  files_to_read <- sort(list.files(path))
  metadata <- grep("RunMetadata", files_to_read, value = TRUE)
  priodata <- grep("CurrentPriors", files_to_read, value = TRUE)
  targdata <- grep("CurrentTarget", files_to_read, value = TRUE)
  gooddata <- grep("Good_Simulated", files_to_read, value = TRUE)
  meandata <- grep("MeanCovariance", files_to_read, value = TRUE)

  # Read the appropriate files in
  if ((length(metadata) > 1 | length(priodata) > 1 | length(targdata) > 1 | length(gooddata) > 3 | length(meandata) > 1)) {
    # Check that we can handle the data files
    stopifnot(
      "Multiple run files appear in the directory. Please specify which run you wish to use by specifying a tag." = !is.null(tag)
    )

    # Read in inputs needed for run
    prev_run_meta <- tryCatch(
      read.csv(paste(path, grep(tag, metadata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find RunMetadata_%s.csv in previous_results_dir = %s", tag, path))
      }
    )
    prev_priors <- tryCatch(
      read.csv(paste(path, grep(tag, priodata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find CurrentPriors_%s.csv in previous_results_dir = %s", tag, path))
      }
    )
    prev_targs <- tryCatch(
      read.csv(paste(path, grep(tag, targdata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find CurrentTargets_%s.csv in previous_results_dir = %s", tag, path))
      }
    )

    # Read in Good_* files
    gooddata <- grep(tag, gooddata, value = TRUE)
    good_parm_draws <- tryCatch(
      read.csv(paste(path, grep("Parameters", gooddata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find Good_SimulatedParameters_%s.csv in previous_results_dir = %s", tag, path))
      }
    )
    good_sim_target <- tryCatch(
      read.csv(paste(path, grep("Targets", gooddata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find Good_SimulatedTargets_%s.csv in previous_results_dir = %s", tag, path))
      }
    )
    good_target_dist <- tryCatch(
      read.csv(paste(path, grep("Distances", gooddata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find Good_SimulatedDistances_%s.csv in previous_results_dir = %s", tag, path))
      }
    )

    # Read in Mean Covariance
    mean_cov <- tryCatch(
      read.csv(paste(path, grep(tag, meandata, value = TRUE)[1], sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find MeanCovariance_%s.csv in previous_results_dir = %s", tag, path))
      }
    )

  } else {
    # Read in inputs needed for run
    prev_run_meta <- tryCatch(
      read.csv(paste(path, metadata, sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "RunMetadata_*.csv", path))
      }
    )
    prev_priors <- tryCatch(
      read.csv(paste(path, priodata, sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "CurrentPriors_*.csv", path))
      }
    )
    prev_targs <- tryCatch(
      read.csv(paste(path, targdata, sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "CurrentTargets_*.csv", path))
      }
    )

    # Read in Good_* files
    good_parm_draws <- tryCatch(
      read.csv(paste(path, grep("Parameters", gooddata, value = TRUE), sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "Good_SimulatedParameters_*.csv", path))
      }
    )
    good_sim_target <- tryCatch(
      read.csv(paste(path, grep("Targets", gooddata, value = TRUE), sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "Good_SimulatedTargets_*.csv", path))
      }
    )
    good_target_dist <- tryCatch(
      read.csv(paste(path, grep("Distances", gooddata, value = TRUE), sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "Good_SimulatedDistances_*.csv", path))
      }
    )

    # Read in Mean Covariance
    mean_cov <- tryCatch(
      read.csv(paste(path, meandata, sep = "/")),
      warning = function(w) {
        stop(sprintf("Can't find %s in previous_results_dir = %s", "MeanCovariance_*.csv", path))
      }
    )
  }

  # Create Target and prior objects
  new_priors <- define_priors(prior_df = prev_priors)
  new_targets <- define_targets(target_df = prev_targs)

  # Previous results object
  previous_results <- list(
    prev_run_meta = prev_run_meta,
    good_parm_draws = good_parm_draws,
    good_sim_target = good_sim_target,
    good_target_dist = good_target_dist,
    mean_cov = mean_cov
  )

  return(list(
    new_targets = new_targets,
    new_priors = new_priors,
    previous_results = previous_results
  ))
}
