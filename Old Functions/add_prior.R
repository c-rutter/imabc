#' @title Add Prior Distribution Information
#'
#' @description  Defines the prior distribution for a parameter as well as the range a parameter must stay within as the calibration proceeds.
#'
#' @param dist_base_name character(1) Optional. The base name of the RNG function set for the prior distribution.
#' @param density_fn character(1) Optional. The name of the RNG density function for the prior distribution.
#' @param quantile_fn character(1) Optional. The name of the RNG quantile function for the prior distribution.
#' @param parameter_name character(1) Optional. The name of the parameter.
#' @param ... Optional. Named inputs to be passed to the RNG functions
#'
#' @details
#' ## Distribution Specifications:
#' If the user does not provide any RNG functions specifications, they must provide a single value in order to
#' create a fixed parameter. This is not the most efficient method for using a fixed parameter in a model. However, in
#' the future, the user will be able to use previous results to recalibrate a model using updated parameter specifications
#' (one requirement may be that the parameter must have been specified in a previous run.)
#'
#' If the user only provides one of the RNG functions specifications, add_prior will search for the most logical names
#' for the other functions. I.e. if dist_base_name is provided (e.g. unif), add_prior will assume that the user wishes
#' to use paste0("d", dist_base_name) for the density function and paste0("q", dist_base_name) for the quantile function.
#' add_prior will make the corresponding guesses if the user provides density_fn or quantile_fn. If density_fn or
#' quantile_fn are provided, add_prior will assume those functions are preferred over any calculated function names.
#'
#' ## ... Specifications:
#' add_prior will attempt to pass any extra arguments to the RNG functions. These must be named to not create errors. If
#' a value's name cannot be matched to an RNG function input, it will be ignored.
#'
#' min/max are important values to imabc and will always be defined for each parameter. They are used to evaluate whether
#' any simulated parameters are valid. The user can specify values for them if they want. If the user does not specify
#' them add_prior will look at the RNG function and if the RNG has default values for min/max it will use them, otherwise
#' it will use -Inf/Inf respectively. **Warning**: This behavior depends on the RNG functions using min and max as the input
#' names for the min and max values. If the RNG functions use an alternate name for these concepts add_prior will treat
#' them as separate values. An example of this can be found in the truncnorm package which uses a and b for the min and
#' max respectively. For those functions the user would need to specify inputs for a, b, min, and max in order to get a
#' consistent result
#' @md
#'
#' @return A prior imabc object that can be passed to define_priors.
#' @export
#'
#' @examples
#' add_prior(dist_base_name = "norm")
#' add_prior(density_fn = "dnorm", mean = 50, sd = 10)
#' add_prior(quantile_fn = "qnorm", min = 0, max = 1)
add_prior <- function(dist_base_name = NULL, density_fn = NULL, quantile_fn = NULL, parameter_name = NULL, ...) {
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
