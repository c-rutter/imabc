init_iter_dt <- function(n_row, cols, type = c("parm_draws", "targ_dists", "sim_targs")) {
  type <- match.arg(type)

  # Same for all types
  dt <- data.table(
    iter = 1L,
    draw = 1:n_row,
    step = 0L
  )

  # Type specific columns
  if (type == "parm_draws") {
    dt[, seed := NA_character_]
    dt[, (cols) := NA_real_]
    dt[, scaled_dist := NA_real_]
    dt[, sample_wt := 0]

  } else if (type == "targ_dists") {
    dt[, (cols) := NA_real_]
    dt[, tot_dist := NA_real_]
    dt[, n_good := 0L]

  } else if (type == "sim_targs") {
    dt[, (cols) := NA_real_]
  }

  return(dt)
}
