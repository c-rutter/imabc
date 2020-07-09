define_targets <- function(...) {
  all_targets <- list(...)

  update <- rep_len(TRUE, length.out = length(all_targets))

  names(update) <- names(all_targets)
  sub_targets <- as.character(unlist(lapply(all_targets, FUN = function(x) { x[["names"]] })))

  attributes(all_targets)$update <- update
  attributes(all_targets)$sub_targets <- sub_targets

  return(all_targets)
}
