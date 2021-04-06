init_good_dt <- function(final_n, current_n = 0, previous_dt = NULL, cols, type = c("parm_draws", "targ_dists", "sim_targs")) {
  seed <- scaled_dist <- sample_wt <- tot_dist <- n_good <- NULL

  # Type to return
  type <- match.arg(type)

  # Rows to return
  need_n <- final_n - current_n

  # Handle if a previous data.table already exists
  if (!is.null(previous_dt)) {
    if (type == "parm_draws") {
      previous_dt$scaled_dist <- NA_real_
    }

    # Handle if we don't need any new rows added to a previous data.table
    if (need_n > 0) {
      dt <- data.table(rbind(previous_dt, init_good_dt(final_n = need_n, cols = cols, type = type)))
    } else {
      dt <- data.table(previous_dt)
    }

  } else {
    # Initialize columns that are the same for all types of results
    dt <- data.table(
      iter = rep.int(NA_real_, need_n),
      draw = NA_real_,
      step = NA_real_
    )

    # Initialize columns that are type specific
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
