init_run_dt <- function(n, parms, type, out_final = FALSE) {
  if (type == "draw") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      seed = rep(NA_character_, n)
    )
    df[, (parms) := NA_real_]
    df[, scaled_dist := NA_real_]
    df[, sample_wt := rep.int(0, n)]

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
  } else if (type == "mixture") {
    df <- data.table(
      iter = NA_integer_,
      step = NA_integer_,
      center = NA_integer_,
      B.in = NA_integer_,
      parm = 0:n
    )
    df[, (parms) := NA_real_]
  }

  # Final results should not initialize with a draw ID
  if (out_final & type != "mixture") {
    df$iter <- NA_integer_
    df$draw <- NA_integer_
    df$step <- NA_integer_
  }

  return(df)
}
