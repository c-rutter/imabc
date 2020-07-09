#' Add Prior Distribution Information
#'
#' @param ... Parameters read by prior generating function
#' @param FUN prior generating function
#' @param use_length Does prior generating function expect a scalar? See Details
#'
#' @details imabc passes a vector of inputs to the prior distribution function in order to generate a sampling of parameters.
#' If the prior generating function takes in a scalar (n) and returns a vector, set use_length = TRUE
#'
#' @return
#' @export
#'
#' @examples
add_prior <- function(..., FUN = NULL, dtruncnorm = TRUE, use_length = TRUE) { # New
  # mean = NULL, sd, min, max
  dots <- list(...)

  # if FUN is NULL, assume a fixed parameter
  if (is.null(FUN)) {
    if (length(dots) != 1) {
      stop("add_prior: For a Fixed Parameter only supply a single numeric value of length 1")
    }
    dots <- as.numeric(dots)
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        res <- rep(dots, length(n))
      } else {
        res <- rep(dots, n)
      }

      return(res)
    }
    # Define min/max for attributes
    min <- dots
    max <- dots
    mean <- dots
    sd <- 0
    fixed <- TRUE
  } else { # is.null(FUN)
    # Check for the appropriate inputs
    check_names <- names(dots)
    if (!all(c("mean", "sd", "min", "max") %in% check_names)) {
      # If mean is missing, we may not need it if the user is not using dtruncnorm
      if (dtruncnorm && !("mean" %in% check_names)) {
        stop("mean is a required input when dtruncnorm == TRUE")
      } else if (!dtruncnorm && !("mean" %in% check_names)) {
        dots$mean <- NA
      }
      # sd, min, and max are all required
      if (!(all(c("sd", "min", "max") %in% check_names))) {
        stop("The user must provide sd, min, and max to the prior function in addition to the inputs needed by FUN")
      }
    }

    new_dots <- dots[formalArgs(FUN)[-1]]
    # Create the function call for the prior
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        n <- length(n)
        res <- do.call(match.fun(FUN), c(n, new_dots))
      } else {
        res <- mapply(match.fun(FUN), n, MoreArgs = new_dots)
      }

      return(res)
    }

    min <- dots$min
    max <- dots$max
    mean <- dots$mean
    sd <- dots$sd
    fixed <- FALSE
  } # ! is.null(FUN)

  return(list(f = f, min = min, max = max, mean = mean, sd = sd, fixed = fixed))
}
