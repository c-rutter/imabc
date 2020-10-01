#' @export
update_target_bounds <- function(targets_list, from = c("current", "new"), to = c("new", "current")) {
  from <- match.arg(from, c("current", "new"))
  to <- match.arg(to, c("current", "new"))

  from <- c(paste0(from, "_lower_bounds"), paste0(from, "_upper_bounds"))
  to <- c(paste0(to, "_lower_bounds"), paste0(to, "_upper_bounds"))

  targets_list[[from[1]]] <- targets_list[[to[1]]]
  targets_list[[from[2]]] <- targets_list[[to[2]]]

  return(targets_list)
}
