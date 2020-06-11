add_prior <- function(..., FUN = NULL, use_length = TRUE) {
  dots <- list(...)

  # if FUN is NULL, assume a fixed parameter
  if (is.null(FUN)) {
    if (length(dots) != 1) {
      stop("For a Fixed Parameter only supply a constant to the prior function")
    }
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        res <- rep(as.numeric(dots), length(n))
      } else {
        res <- rep(as.numeric(dots), n)
      }

      return(res)
    }
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
  }

  return(f)
}