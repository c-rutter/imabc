#' @export
define_priors <- function(..., previous_run_priors = NULL) {
  priors_list <- list(...)

  # When new priors are defined
  if (length(priors_list) > 0) {
    # Check that only imabc_priors have been added
    good <- unlist(lapply(priors_list, function(x) { all(inherits(x, c("prior", "imabc"), TRUE) != 0) }))
    if (!all(good)) {
      bad_noname <- which(names(good) == "" & !good)
      bad_winame <- which(names(good) != "" & !good)
      txt1 <- txt2 <- c()
      if (length(bad_noname) > 0) {
        txt1 <- paste("prior's at position(s)", paste(bad_noname, collapse = ", "), sep = ": ")
      }
      if (length(bad_winame) > 0) {
        txt2 <- paste("prior's named", paste(names(bad_winame), collapse = ", "), sep = ": ")
      }
      if (!is.null(txt1) && !is.null(txt2)) {
        txt <- paste(txt1, "and", txt2, sep = " ")
      } else if (!is.null(txt1)) {
        txt <- txt1
      } else if (!is.null(txt2)) {
        txt <- txt2
      }
      stop(sprintf("Inputs must be imabc priors. Use add_prior() to create prior objects for %s.", txt))
    }

    # Store added functions
    return_priors <- get_list_element(priors_list, name = c("density_function", "quantile_function"), unlist = FALSE)
    return_priors <- structure(return_priors, class = c("priors", "imabc"))

    # Pull all important attributes of the priors into lists
    name_vec <- get_list_element(priors_list, "parameter", unlist = TRUE)
    sd <- rep.int(0L, length(priors_list)) # empirically calculated in imabc
    min <- get_list_element(priors_list, "min", unlist = TRUE) # if not set or given default by distribution functions, defaults to -Inf
    max <- get_list_element(priors_list, "max", unlist = TRUE) # if not set or given default by distribution functions, defaults to Inf
    distribution <- get_list_element(priors_list, "distribution", unlist = TRUE)
    fun_inputs <- get_list_element(priors_list, "fun_inputs", unlist = FALSE)

    # Pull the names of the parameters. Check for uniqueness and add unique names where no names are provided.
    prior_names <- unique_names(return_priors, name_vec)
    names(return_priors) <- prior_names
    names(min) <- prior_names
    names(max) <- prior_names
    names(sd) <- prior_names
    names(distribution) <- prior_names
    names(fun_inputs) <- prior_names

    # Store the vectors as attributes for the entire prior object
    attributes(return_priors)$mins <- min
    attributes(return_priors)$maxs <- max
    attributes(return_priors)$sds <- sd
    attributes(return_priors)$distributions <- distribution
    attributes(return_priors)$fun_inputs <- fun_inputs
  }

  if (!is.null(previous_run_priors)) {
    # Pull Prior Specific Information
    previous_run_priors <- previous_run_priors[previous_run_priors$TYPE == "priors", ]
    previous_run_priors$TYPE <- NULL

    # Unique priors
    prior_ids <- unique(previous_run_priors$ID)

    return_priors <- list()
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
      minmax <- setdiff(c("mins", "maxs"), names(fun_in))
      if (length(minmax) > 0) {
        imabc_minmax <- as.list(as.numeric(prior_tmp$VALUE[prior_tmp$INFO %in% paste0("imabc_", minmax)]))
        names(imabc_minmax) <- minmax
      } else {
        imabc_minmax <- as.null()
      }
      prior_args <- c(prior_args, imabc_minmax)

      # Add priors to a list
      return_priors[[i1]] <- do.call(add_prior, arg = prior_args)
    }
    # Create a prior object
    return_priors <- do.call(define_priors, args = return_priors)

    # Update the standard deviation attribute
    sd <- as.numeric(previous_run_priors$VALUE[previous_run_priors$INFO == "imabc_sds"])
    names(sd) <- previous_run_priors$ID[previous_run_priors$INFO == "imabc_sds"]
    attributes(return_priors)$sds <- sd
    return_priors <- structure(return_priors, class = c("priors", "imabc"))
  }

  if (length(priors_list) > 0 & !is.null(previous_run_priors)) {
    stop("Currently does not support adding new parameters to an old set of runs")
  }

  return(return_priors)
}


#' @export
print.priors <- function(p, digits = getOption("digits"), detail = FALSE) {
  n_parms <- length(p)
  parm_names <- names(p)

  cat(sprintf("There are %s defined parameters.\n", n_parms))
  if (!detail) {
    cat("Use print(..., detail = TRUE) to see the prior information defined for each parameter.\n")
  } else {
    p_text <- lapply(parm_names, FUN = function(x, p) {
      dist_text <- sprintf("Distribution base name: %s", attr(p, "distributions")[x])
      fun_text <- sprintf("User specified inputs: %s", attr(p, "fun_inputs")[x])
      fun_text <- sub("(.*)(list\\(fun_inputs = c*\\(*)(.*)(\\))", "\\1\\3", fun_text, perl = T)
      fun_text <- sub("\\)", "", fun_text)
      range_text <- sprintf("Allowable range: %s - %s", attr(p, "mins")[x], attr(p, "maxs")[x])
      sd_text <- sprintf("Empirical Standard Deviation: %s", format(attr(p, "sds")[x], digits = digits))

      full_text <- paste(dist_text, fun_text, range_text, sd_text, sep = "\n")
    }, p = p)
    cat(
      paste(sprintf("\nParameter %s has the following specifications:", parm_names), p_text, sep = "\n", collapse = "\n")
    )
  }

  invisible(p)
}
