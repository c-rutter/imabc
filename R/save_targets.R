save_targets <- function(targets_list, filename, out_dir) {
  dta <- do.call(rbind, lapply(names(targets_list), FUN = function(x, targ) {
    dta <- do.call(cbind, targ[[x]])
    cbind(TargetGroups = x, update = attr(targ, "update")[x], dta)
  }, targ = targets_list))
  dta <- data.table(dta)
  checkcols <- c("lower_bounds_new", "upper_bounds_new")
  dontsave <- which(checkcols %in% colnames(dta))
  if (length(dontsave) > 0) {
    dta[, checkcols[dontsave] := NULL]
  }
  dta[, (colnames(dta)) := lapply(.SD, function (x) type.convert(x))]

  # Write to table
  invisible(
    write.table(x = dta, file = paste0(out_dir, "/", filename), sep = ",", row.names = FALSE)
  )
}
