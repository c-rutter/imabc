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
add_prior <- function(..., density_fn = NULL, quantile_fn = NULL) {
  inputs <- list(...)

  # if FUN is NULL, assume a fixed parameter
  if (is.null(density_fn) & is.null(quantile_fn)) {
    if (length(inputs) != 1) {
      stop("add_prior: For a Fixed Parameter only supply a single numeric value of length 1")
    }
    inputs <- as.numeric(inputs)
    density_fn <- quantile_fn <- function(n) {
      sims <- rep(inputs, length(n))

      return(sims)
    }
    # Define min/max for attributes
    min <- inputs
    max <- inputs
    distribution <- "fixed"
  } else { # is.null(density_fn) & is.null(quantile_fn)
    # Find and use the appropriate distribution functions based on user input
    if (!is.null(density_fn) & is.null(quantile_fn)) {
      # Check that density function provided exists
      fn_name <- eval(density_fn)
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(fn_name))
      if (!exists(fn_name, mode = 'function')) { stop(e) }

      # Check that best guess quantile function exists
      quantile_fn <- gsub("^d", "q", fn_name)
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(quantile_fn))
      if (!exists(quantile_fn, mode = 'function')) { stop(e) }
    } else if (is.null(density_fn) & !is.null(quantile_fn)) { # !is.null(density_fn) & is.null(quantile_fn)
      # Check that quantile function provided exists
      fn_name <- eval(quantile_fn)
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(fn_name))
      if (!exists(fn_name, mode = 'function')) { stop(e) }

      # Check that best guess density function exists
      density_fn <- gsub("^q", "d", fn_name)
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(density_fn))
      if (!exists(density_fn, mode = 'function')) { stop(e) }
    } else { # is.null(density_fn) & !is.null(quantile_fn)
      # Check that density function provided exists
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(density_fn))
      if (!exists(density_fn, mode = 'function')) { stop(e) }

      # Check that quantile function provided exists
      e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(quantile_fn))
      if (!exists(quantile_fn, mode = 'function')) { stop(e) }
    } # !is.null(density_fn) & !is.null(quantile_fn)
    # Distribution shorthand
    dist_shorthand <- gsub("^d", "", density_fn)

    # Check for min and max which are required
    check_names <- names(inputs)
    # min, and max are all required
    e <- sprintf("The user must provide min and max to the prior function in addition to the inputs needed by %s functions", eval(dist_shorthand))
    if (!(all(c("min", "max") %in% check_names))) { stop(e) }

    # Define the density function call
    intersect(names(inputs), formalArgs(density_fn)[-1])

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

    min <- inputs$min
    max <- inputs$max
    distribution <- dist_shorthand
  } # ! is.null(density_fn) & is.null(quantile_fn)

  return(list(
    density_function = density_function,
    quantile_function = quantile_function,
    min = min,
    max = max,
    distribution = distribution
  ))
}
