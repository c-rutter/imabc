#' Initialize run data.frames
#'
#' @param n
#' @param parms
#' @param type
#'
#' @return
#' @export
init_run_df <- function(n, parms, type) { # same as init.draws
  if (type == "draw") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      seed = rep(NA_character_, n),
      matrix(NA_real_, n, length(parms)),
      scaled_dist = rep(NA_real_, n),
      sample_wt = rep(NA_real_, n)
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  } else if (type == "distance") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      matrix(NA_real_, n, length(parms)),
      tot_dist = rep(NA_real_, n),
      n_good = rep.int(0, n)
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  } else if (type == "sim") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      matrix(NA_real_, n, length(parms))
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  }

  return(df)
}
