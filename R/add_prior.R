#' Add Prior Distribution Information
#'
#' @param ... Parameters read by prior generating function
#' @param density_fn
#' @param quantile_fn
#'
#' @details If the user doesn't provide any RNG functions, imabc will create a parameter that doesn't vary (in the future).
#' If the user only provides one of the required RNG functions, add_prior will search for the most logical name of the other
#' function. I.e. if a density function is provided (such as dunif), add_prior will look for the quantile function (qunif)
#' If the user selects both functions independently or create their own functions it is on them to make sure they work
#' appropriately.
#'
#' @return
#' @export
#'
#' @examples
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
    density_fn <- quantile_fn <- function(n) {
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
    }

    # Defaults for min and max values of prior distribution
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

  return(list(
    parameter = parameter_name,
    density_function = density_function,
    quantile_function = quantile_function,
    min = min,
    max = max,
    distribution = distribution,
    fun_inputs = fun_inputs
  ))
}
