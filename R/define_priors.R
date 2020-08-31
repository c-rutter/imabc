define_priors <- function(..., previous_run_priors = NULL) {
  fs <- list(...)

  # When new priors are defined
  if (length(fs) > 0) {
    # Store added functions
    f <- lapply(fs, FUN = function(x) {
      list(
        density_function = x[["density_function"]],
        quantile_function = x[["quantile_function"]]
      )
    })

    # Pull all important attributes of the priors into lists
    sd <- rep.int(0L, length(fs)) # empirically calculated in imabc
    min <- unlist(lapply(fs, FUN = function(x) { x[["min"]] })) # if not set or given default by distribution functions, defaults to -Inf
    max <- unlist(lapply(fs, FUN = function(x) { x[["max"]] })) # if not set or given default by distribution functions, defaults to Inf
    distribution <- unlist(lapply(fs, FUN = function(x) { x[["distribution"]] }))
    fun_inputs <- lapply(fs, FUN = function(x) { x[["fun_inputs"]] })

    # Pull the names of the parameters. Check for uniqueness and add unique names where no names are provided.
    prior_names <- .unique_names(things_list = fs, thing = "parameter")
    names(f) <- prior_names
    names(min) <- prior_names
    names(max) <- prior_names
    names(sd) <- prior_names
    names(distribution) <- prior_names
    names(fun_inputs) <- prior_names

    # Store the vectors as attributes for the entire prior object
    attributes(f)$mins <- min
    attributes(f)$maxs <- max
    attributes(f)$sds <- sd
    attributes(f)$distributions <- distribution
    attributes(f)$fun_inputs <- fun_inputs
  }

  if (!is.null(previous_run_priors)) {
    # Pull Prior Specific Information
    previous_run_priors <- previous_run_priors[previous_run_priors$IMABC == "Prior", ]
    previous_run_priors$IMABC <- NULL

    # Unique priors
    prior_ids <- unique(previous_run_priors$ID)

    f <- list()
    # For each parameter
    for (i1 in prior_ids) {
      # Find appropriate paramter
      prior_tmp <- previous_run_priors[previous_run_priors$ID == i1, ]

      # Get user defined inputs
      fn_inputs <- prior_tmp[grep("^fun_", prior_tmp$INFO), ]
      fun_in <- as.list(as.numeric(fn_inputs$VALUE))
      names(fun_in) <- gsub("^fun_", "", fn_inputs$INFO)
      prior_args <- c(list(dist_base_name = prior_tmp$VALUE[prior_tmp$INFO == "distribution"], parameter_name = i1), fun_in)

      # Get Min/Max values from IMABC if not defined by function inputs
      minmax <- setdiff(c("min", "max"), names(fun_in))
      if (length(minmax) > 0) {
        imabc_minmax <- as.list(as.numeric(prior_tmp$VALUE[prior_tmp$INFO %in% paste0("imabc_", minmax)]))
        names(imabc_minmax) <- minmax
      } else {
        imabc_minmax <- as.null()
      }
      prior_args <- c(prior_args, imabc_minmax)

      # Add priors to a list
      f[[i1]] <- do.call(add_prior, arg = prior_args)
    }
    # Create a prior object
    f <- do.call(define_priors, args = f)

    # Update the standard deviation attribute
    sd <- as.numeric(previous_run_priors$VALUE[previous_run_priors$INFO == "empirical_sd"])
    names(sd) <- previous_run_priors$ID[previous_run_priors$INFO == "empirical_sd"]
    attributes(f)$sds <- sd
  }

  if (length(fs) > 0 & !is.null(previous_run_priors)) {
    stop("Currently does not support adding new parameters to an old set of runs")
  }

  return(f)
}
