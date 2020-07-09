update_target_bounds <- function(targets_list, from = c("start", "new"), to = c("new", "start")) {
  from <- match.arg(from, c("start", "new"))
  to <- match.arg(to, c("start", "new"))

  from <- c(paste0("lower_bounds_", from), paste0("upper_bounds_", from))
  to <- c(paste0("lower_bounds_", to), paste0("upper_bounds_", to))

  targets_list <- lapply(targets_list, FUN = function(x, from, to) {
    x[[from[1]]] <- x[[to[1]]]
    x[[from[2]]] <- x[[to[2]]]

    return(x)
  }, from = from, to = to)

  return(targets_list)
}
