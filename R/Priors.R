#' @name PriorsSpecification
#' @title Specify the Prior Distributions for All Parameters
#'
#' @param ... Optional. In add_prior: Named inputs to be passed to the RNG functions. In define_priors: The results of
#' add_prior calls - one for each parameter that is being calibrated.
#'
#' @description Helper functions that can be used to create an imabc priors object used by imabc().
#'
#' @section Distribution Specifications:
#' If the user does not provide any RNG functions specifications, they must provide a single value in order to
#' create a fixed parameter. This is not the most efficient method for using a fixed parameter in a model. However, in
#' the future, the user will be able to use previous results to recalibrate a model using updated parameter specifications
#' (one requirement may be that the parameter must have been specified in a previous run).
#'
#' If the user only provides one of the RNG functions specifications, these functions will search for the most logical
#' names for the other functions. I.e. if dist_base_name is provided (e.g. unif), these will assume that the user wishes
#' to use paste0("d", dist_base_name) for the density function and paste0("q", dist_base_name) for the quantile function.
#' These functions will make the corresponding guesses if the user provides density_fn or quantile_fn. If density_fn or
#' quantile_fn are provided, they will assume those functions are preferred over any calculated function names.
#' @md
#'
#' @section RNG Input Specifications:
#' These functions will attempt to pass any extra arguments to the RNG functions. These must be named to not create errors.
#' If a value's name cannot be matched to an RNG function input, it will be ignored.
#'
#' min/max are important values to imabc and will always be defined for each parameter. They are used to evaluate whether
#' any simulated parameters are valid. The user can specify values for them if they want. If the user does not specify
#' them they will look at the RNG function and if the RNG has default values for min/max it will use them, otherwise
#' it will use -Inf/Inf respectively. **Warning**: This behavior depends on the RNG functions using min and max as the input
#' names for the min and max values. If the RNG functions use an alternate name for these concepts they will treat
#' them as separate values. An example of this can be found in the truncnorm package which uses a and b for the min and
#' max respectively. For those functions the user would need to specify inputs for a, b, min, and max in order to get a
#' consistent result.
#' @md
#'
#' @section Parameter Names:
#' The user can specify names by either specifying the input parameter_name in add_prior or by setting the result of an
#' add_prior call to a object in define_priors (e.g. define_priors(x1 = add_prior(...))). If the user specifies the
#' parameter_name input and sets add_prior to an object, the parameter_name value will be used. If no name is specified
#' a unique name will be generated automatically.
#' @md
#'
#' @return A priors imabc object.
NULL

#' @rdname PriorsSpecification
#'
#' @param dist_base_name Optional character(1). The base name of the RNG function set (or the column with the dist_base_name
#' info in as.priors) for the prior distribution.
#' @param density_fn Optional character(1). The name of the RNG density function (or the column with the density_fn
#' info in as.priors) for the prior distribution.
#' @param quantile_fn Optional character(1). The name of the RNG quantile function (or the column with the quantile_fn
#' info in as.priors) for the prior distribution.
#' @param parameter_name Optional character(1). The name of the parameter (or the column with the parameter_name info in
#' as.priors).
#'
#' @examples
#' x1 <- add_prior(dist_base_name = "norm")
#' x2 <- add_prior(density_fn = "dnorm", mean = 50, sd = 10)
#' x3 <- add_prior(quantile_fn = "qnorm", min = 0, max = 1)
#'
#' @export
add_prior <- function(..., dist_base_name = NULL, density_fn = NULL, quantile_fn = NULL, parameter_name = NULL) {
  inputs <- list(...)

  # if dist_base_name, density_fn, and quantile_fn are NULL, assume a fixed parameter
  if (is.null(dist_base_name) & is.null(density_fn) & is.null(quantile_fn)) {
    # Check appropriateness of input(s)
    stopifnot(
      "For a Fixed Parameter only supply a single numeric value of length 1." = length(inputs) == 1,
      "Input must be numeric." = !is.na(suppressWarnings(as.numeric(inputs)))
    )

    # Define outputs
    inputs <- as.numeric(inputs)
    density_function <- quantile_function <- function(n) {
      sims <- rep(inputs, length(n))

      return(sims)
    }
    # Define min/max for attributes
    min <- inputs
    max <- inputs
    distribution <- "fixed"
    fun_inputs <- inputs
  } else { # is.null(dist_base_name) & is.null(density_fn) & is.null(quantile_fn)
    if (is.null(dist_base_name)) {
      if (!is.null(density_fn) & is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #           NULL    provided         NULL
        # Check that density function provided exists
        density_fn <- .validate_prior_function(density_fn)
        # Check that best guess quantile function exists
        quantile_fn <- .validate_prior_function(gsub("^d", "q", density_fn))

      } else if (is.null(density_fn) & !is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #           NULL        NULL     provided
        # Check that quantile function provided exists
        quantile_fn <- .validate_prior_function(quantile_fn)
        # Check that best guess density function exists
        density_fn <- .validate_prior_function(gsub("^q", "d", quantile_fn))

      } else if (!is.null(density_fn) & !is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #           NULL    provided     provided
        # Check that density function provided exists
        density_fn <- .validate_prior_function(density_fn)
        # Check that quantile function provided exists
        quantile_fn <- .validate_prior_function(quantile_fn)
      }
      # Distribution shorthand
      dist_base_name <- gsub("^d", "", density_fn)

    } else if (!is.null(dist_base_name)) {
      # Check that base name converts to existing function
      density_fn_guess <- .validate_prior_function(paste0("d", dist_base_name))
      # Check that quantile function provided exists
      quantile_fn_guess <- .validate_prior_function(paste0("q", dist_base_name))

      if (is.null(density_fn) & is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #       provided        NULL         NULL
        density_fn <- density_fn_guess
        quantile_fn <- quantile_fn_guess

      } else if (!is.null(density_fn) & is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #       provided    provided         NULL
        density_fn <- .validate_prior_function(density_fn)
        # Warn if density function from dist_base_name != density_fn
        if (density_fn_guess != density_fn) {
          warning("Base distribution name does not convert to provided density function. Using density_fn.")
        }
        quantile_fn <- quantile_fn_guess

      } else if (is.null(density_fn) & !is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #       provided        NULL     provided
        quantile_fn <- .validate_prior_function(quantile_fn)
        # Warn if quantile function from dist_base_name != quantile_fn
        if (quantile_fn_guess != quantile_fn) {
          warning("Base distribution name does not convert to provided quantile function. Using quantile_fn.")
        }
        density_fn <- density_fn_guess

      } else if (!is.null(density_fn) & !is.null(quantile_fn)) {
        # dist_base_name  density_fn  quantile_fn
        #       provided    provided     provided
        density_fn <- .validate_prior_function(density_fn)
        quantile_fn <- .validate_prior_function(quantile_fn)

        # Warn if density function from dist_base_name != density_fn
        if (density_fn_guess != density_fn) {
          warning("Base distribution name does not convert to provided density function. Using density_fn.")
        }
        # Warn if quantile function from dist_base_name != quantile_fn
        if (quantile_fn_guess != quantile_fn) {
          warning("Base distribution name does not convert to provided quantile function. Using quantile_fn.")
        }

      }
    } # !is.null(dist_base_name)

    # Defaults for min and max values of prior distribution: inputs -> formals -> -Inf/Inf
    min <- ifelse(
      "min" %in% names(inputs), inputs$min, ifelse(
        "min" %in% names(formals(density_fn)), formals(density_fn)["min"], -Inf
      ))
    max <- ifelse("max" %in% names(inputs), inputs$max, ifelse(
      "max" %in% names(formals(density_fn)), formals(density_fn)["max"], Inf
    ))

    # Define the density function call
    density_inputs <- inputs[intersect(names(inputs), formalArgs(density_fn)[-1])]
    density_function <- function(n) {
      sims <- mapply(match.fun(density_fn), n, MoreArgs = density_inputs)

      return(sims)
    }

    # Define the density function call
    quantile_inputs <- inputs[intersect(names(inputs), formalArgs(density_fn)[-1])]
    quantile_function <- function(n) {
      sims <- mapply(match.fun(quantile_fn), n, MoreArgs = quantile_inputs)

      return(sims)
    }

    distribution <- dist_base_name
    fun_inputs <- unlist(quantile_inputs)
  } # ! is.null(dist_base_name) & is.null(density_fn) & is.null(quantile_fn)

  # Create the prior object
  prior <- structure(
    list(
      parameter = parameter_name,
      density_function = density_function,
      quantile_function = quantile_function,
      min = min,
      max = max,
      distribution = distribution,
      fun_inputs = fun_inputs
    ),
    class = c("prior", "imabc")
  )

  return(prior)
}

#' @rdname PriorsSpecification
#'
#' @param previous_run_priors Optional data.frame. The data.frame read into R from the Metadata file saved from a previous
#' imabc run.
#'
#' @examples
#' define_priors(x1, x2, x3)
#'
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

#' @rdname PriorsSpecification
#'
#' @param df data.frame. Each parameter should be a row and each column is an input into add_prior. If a given column doesn't
#' relate to a parameter, set its value to NA.
#'
#' @examples
#' df <- data.frame(
#'   parameter_name = c("x1", "x2", "x3"), dist_base_name = c("unif", NA, NA), density_fn = c(NA, "dtruncnorm", NA),
#'   quantile_fn = c(NA, NA, "qnorm"), mean = c(NA, 0.75, NA), sd = c(NA, 0.05, NA), min = c(x1_min, x2_min, NA),
#'   max = c(x1_max, x2_max, NA), a = c(NA, x2_min, NA), b = c(NA, x2_max, NA)
#' )
#' as.priors(df, name_var = "parameter_name", dist_var = "dist_base_name", density_var = "density_fn", quantile_var = "quantile_fn")
#'
#' @export
as.priors <- function(df, dist_base_name = NULL, density_fn = NULL, quantile_fn = NULL, parameter_name = NULL) {
  # Make sure at least one of dist_base_name, density_fn, or quantile_fn are found
  cols <- colnames(df)
  provided_names <- c(dist_base_name, density_fn, quantile_fn)
  if (!any(c(!is.null(provided_names)))) {
    stop(paste(
      "Must specify the name of a column that represents one of the following:", "distribution base name (dist_base_name),",
      "the density function (density_fn),", "or", "the quantile function (quantile_fn)."
    ))
  }
  if (!any(provided_names %in% cols)) {
    ins <- c("dist_base_name", "density_fn", "quantile_fn")[which(!c(is.null(dist_base_name), is.null(density_fn), is.null(quantile_fn)))]
    "%s = %s. Column %s not found in the df"
    stop(sprintf(
      "%s = %s. Column %s not found in the data.frame; ", ins, provided_names, provided_names
    ))
  }

  # Make sure the names we can control match the expected name for add_prior()
  if (!is.null(parameter_name)) {
    cols[cols == parameter_name] <- "parameter_name"
  }
  if (!is.null(dist_base_name)) {
    cols[cols == dist_base_name] <- "dist_base_name"
  }
  if (!is.null(density_fn)) {
    cols[cols == density_fn] <- "density_fn"
  }
  if (!is.null(quantile_fn)) {
    cols[cols == quantile_fn] <- "quantile_fn"
  }
  colnames(df) <- cols

  # For each parameter, create a prior object
  prio_list <- lapply(1:nrow(df), FUN = function(i1, dta) {
    tmp <- dta[i1, , drop = TRUE]
    tmp <- tmp[!is.na(tmp)]

    do.call(add_prior, tmp)
  }, dta = df)

  # Take all prior objects and add them into a final imabc priors object
  final_prio_list <- do.call(define_priors, prio_list)

  return(final_prio_list)
}

#' @export
print.priors <- function(p, digits = getOption("digits")) {
  n_parms <- length(p)
  parm_names <- names(p)

  cat(sprintf("There are %s defined parameters.\n", n_parms))
  p_text <- lapply(parm_names, FUN = function(x, p) {
    dist_text <- sprintf("Distribution base name: %s", attr(p, "distributions")[x])
    fun_text <- sprintf("User specified inputs: %s", attr(p, "fun_inputs")[x])
    fun_text <- sub("(.*)(list\\(fun_inputs = c*\\(*)(.*)(\\))", "\\1\\3", fun_text, perl = T)
    fun_text <- sub("\\)", "", fun_text)
    range_text <- sprintf("Allowable range: %s - %s",
                          format(attr(p, "mins")[x], digits = digits),
                          format(attr(p, "maxs")[x], digits = digits))
    sd_text <- sprintf("Empirical Standard Deviation: %s", format(attr(p, "sds")[x], digits = digits))

    full_text <- paste(dist_text, fun_text, range_text, sd_text, sep = "\n")
  }, p = p)
  cat(
    paste(sprintf("\nParameter %s has the following specifications:", parm_names), p_text, sep = "\n", collapse = "\n")
  )

  invisible(p)
}
