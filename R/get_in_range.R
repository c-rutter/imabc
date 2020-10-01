#' @export
get_in_range <- function(compare_list, check_dt, criteria = NULL, out = "logical") {
  UseMethod("get_in_range")
}

#' @export
get_in_range.targets <- function(compare_list, check_dt, criteria, out = "logical") {
  # Determine which
  check <- sapply(attr(compare_list, which = "target_names"), FUN = function(x, dt, compare_list) {
    if (criteria == "start") {
      in_range(dt[[x]], compare_list$current_lower_bounds[x], compare_list$current_upper_bounds[x]) - 1
    } else if (criteria == "update") {
      in_range(dt[[x]], compare_list$new_lower_bounds[x], compare_list$new_upper_bounds[x]) - 1
    } else if (criteria == "stop") {
      in_range(dt[[x]], compare_list$stopping_lower_bounds[x], compare_list$stopping_upper_bounds[x]) - 1
    }
  }, dt = check_dt, compare_list = compare_list)

  # If grouped targets exist, aggregate distances
  if (inherits(compare_list, "grouped")) {
    check <- t(rowsum(t(check), attr(compare_list, which = "target_groups"), reorder = FALSE))
  }
  if (out == "logical") {
    check <- check == 0
  } else {
    check <- (check + 1)*2 - 1
  }

  return(check)
}

#' @export
get_in_range.priors <- function(compare_list, check_dt, out = "logical") {

  # All parms must be in range based on their prior information
  check <- Reduce(`&`, lapply(names(compare_list), FUN = function(y, check_dt, compare_list) {
    check_dt[[y]] >= attr(compare_list, which = "mins")[y] &
      check_dt[[y]] <= attr(compare_list, which = "maxs")[y]
  }, check_dt = check_dt, compare_list = compare_list))
  check <- as.numeric(check)
  check[is.na(check)] <- 0

  if (out == "logical") {
    check <- check == 1
  } else {
    check <- (check)*2 - 1
  }

  return(check)
}
