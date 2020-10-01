#' @export
read_previous_results <- function(path, tag = NULL) {
  # Get files in path
  files_to_read <- list.files(path)
  metadata <- grep("RunMetadata", files_to_read, value = TRUE)
  gooddata <- grep("Good_Simulated", files_to_read, value = TRUE)
  meandata <- grep("MeanCovariance", files_to_read, value = TRUE)

  if ((length(metadata) > 1 | length(gooddata) > 3 | length(meandata) > 1)) {
    stopifnot(
      "Multiple run files appear in the directory. Please specify which run you wish to use by specifying a tag." = !is.null(tag)
    )

    # Read in metadata
    prev_run_meta <- read.csv(paste(path, grep(tag, metadata, value = TRUE), sep = "/"))

    # Read in Good_* files
    gooddata <- grep(tag, gooddata, value = TRUE)
    good_parm_draws <- read.csv(paste(path, grep("Parameters", gooddata, value = TRUE), sep = "/"))
    good_sim_parm <- read.csv(paste(path, grep("Targets", gooddata, value = TRUE), sep = "/"))
    good_target_dist <- read.csv(paste(path, grep("Distances", gooddata, value = TRUE), sep = "/"))

    # Read in Mean Covariance
    mean_cov <- read.csv(paste(path, grep(tag, meandata, value = TRUE), sep = "/"))
  } else {

    # Read in metadata
    prev_run_meta <- read.csv(paste(path, metadata, sep = "/"))

    # Read in Good_* files
    good_parm_draws <- read.csv(paste(path, grep("Parameters", gooddata, value = TRUE), sep = "/"))
    good_sim_parm <- read.csv(paste(path, grep("Targets", gooddata, value = TRUE), sep = "/"))
    good_target_dist <- read.csv(paste(path, grep("Distances", gooddata, value = TRUE), sep = "/"))

    # Read in Mean Covariance
    mean_cov <- read.csv(paste(path, meandata, sep = "/"))
  }

  new_targets <- define_targets(previous_run_targets = prev_run_meta)
  new_priors <- define_priors(previous_run_priors = prev_run_meta)

  previous_results <- list(
    prev_run_meta = prev_run_meta,
    good_parm_draws = good_parm_draws,
    good_sim_parm = good_sim_parm,
    good_target_dist = good_target_dist,
    mean_cov = mean_cov
  )

  return(list(
    new_targets = new_targets,
    new_priors = new_priors,
    previous_results = previous_results
  ))
}
