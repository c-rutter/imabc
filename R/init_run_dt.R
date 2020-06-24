# CM NOTE: This is needed for R to recognize data.table for some reason. I think this is only when using
#   devtools::load_all() to load functions but once we install the package and import/depend on data.table it should
#   be fine to remove this. Unsure though.
#' Initialize Run data.tables
#'
#' @param n
#' @param parms
#' @param type
#'
#' @return
#' @export
#'
#' @examples
.datatable.aware = TRUE
init_run_dt <- function(n, parms, type) { # same as init.draws
  if (type == "draw") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      seed = rep(NA_character_, n)
    )
    df[, (parms) := NA_real_]
    df[, scaled_dist := NA_real_]
    df[, sample_wt := rep.int(1, n)]

  } else if (type == "distance") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n)
    )
    df[, (parms) := NA_real_]
    df[, tot_dist := NA_real_]
    df[, n_good := rep.int(0, n)]

  } else if (type == "sim") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n)
    )
    df[, (parms) := NA_real_]
  }

  return(df)
}
