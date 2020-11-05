init_good_dt <- function(final_n, current_n = 0, previous_dt = NULL, cols, type = c("parm_draws", "targ_dists", "sim_targs")) {
  type <- match.arg(type)

  need_n <- final_n - current_n
  if (!is.null(previous_dt)) {
    if (need_n > 0) {
      dt <- data.table(rbind(previous_dt, init_good_dt(final_n = need_n, cols = cols, type = type)))
    } else {
      dt <- data.table(previous_dt)
    }

  } else {
    # Same for all types
    dt <- data.table(
      iter = rep.int(NA_real_, need_n),
      draw = NA_real_,
      step = NA_real_
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
  }

  return(dt)
}
