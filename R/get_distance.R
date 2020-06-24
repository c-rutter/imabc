get_distance <- function(dt, target_names, scale = FALSE) {
  dt[, tot_dist := rowSums(.SD^2), .SDcols = target_names]

  return(dt)
}
