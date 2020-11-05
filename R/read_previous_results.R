#' @export
read_previous_results <- function(path, tag = NULL) {
  # Get files in path
  files_to_read <- sort(list.files(path))
  metadata <- grep("RunMetadata", files_to_read, value = TRUE)

  priodata <- grep("CurrentPriors", files_to_read, value = TRUE)
  targdata <- grep("CurrentTarget", files_to_read, value = TRUE)

  gooddata <- grep("Good_Simulated", files_to_read, value = TRUE)
  meandata <- grep("MeanCovariance", files_to_read, value = TRUE)

  if ((length(metadata) > 1 | length(priodata) > 1 | length(targdata) > 1 | length(gooddata) > 3 | length(meandata) > 1)) {
    stopifnot(
      "Multiple run files appear in the directory. Please specify which run you wish to use by specifying a tag." = !is.null(tag)
    )

    # Read in metadata
    prev_run_meta <- read.csv(paste(path, grep(tag, metadata, value = TRUE)[1], sep = "/"))

    prev_priors <- read.csv(paste(path, grep(tag, priodata, value = TRUE)[1], sep = "/"))
    prev_targs <- read.csv(paste(path, grep(tag, targdata, value = TRUE)[1], sep = "/"))

    # Read in Good_* files
    gooddata <- grep(tag, gooddata, value = TRUE)
    good_parm_draws <- read.csv(paste(path, grep("Parameters", gooddata, value = TRUE)[1], sep = "/"))
    good_sim_target <- read.csv(paste(path, grep("Targets", gooddata, value = TRUE)[1], sep = "/"))
    good_target_dist <- read.csv(paste(path, grep("Distances", gooddata, value = TRUE)[1], sep = "/"))

    # Read in Mean Covariance
    mean_cov <- read.csv(paste(path, grep(tag, meandata, value = TRUE)[1], sep = "/"))
  } else {

    # Read in metadata
    prev_run_meta <- read.csv(paste(path, metadata, sep = "/"))

    prev_priors <- read.csv(paste(path, priodata, sep = "/"))
    prev_targs <- read.csv(paste(path, targdata, sep = "/"))

    # Read in Good_* files
    good_parm_draws <- read.csv(paste(path, grep("Parameters", gooddata, value = TRUE), sep = "/"))
    good_sim_target <- read.csv(paste(path, grep("Targets", gooddata, value = TRUE), sep = "/"))
    good_target_dist <- read.csv(paste(path, grep("Distances", gooddata, value = TRUE), sep = "/"))

    # Read in Mean Covariance
    mean_cov <- read.csv(paste(path, meandata, sep = "/"))
  }

  new_targets <- define_targets(previous_run_targets = prev_targs)
  new_priors <- define_priors(previous_run_priors = prev_priors)

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
