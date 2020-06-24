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
add_prior <- function(..., FUN = NULL, sd, min, max, use_length = TRUE) { # New
  dots <- list(...)

  # if FUN is NULL, assume a fixed parameter
  if (is.null(FUN)) {
    if (length(dots) != 1) {
      stop("add_prior: For a Fixed Parameter only supply a single numeric value of length 1")
    }
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        res <- rep(as.numeric(dots), length(n))
      } else {
        res <- rep(as.numeric(dots), n)
      }

      return(res)
    }
    attributes(f)$min <- dots
    attributes(f)$max <- dots
    attributes(f)$sd <- 0
    attributes(f)$fixed <- TRUE
  } else {
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        n <- length(n)
        res <- do.call(match.fun(FUN), c(n, dots))
      } else {
        res <- mapply(match.fun(FUN), n, MoreArgs = dots)
      }

      return(res)
    }
    attributes(f)$min <- min
    attributes(f)$max <- max
    attributes(f)$sd <- sd
    attributes(f)$fixed <- FALSE
  }

  return(f)
}
