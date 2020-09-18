group_targets <- function(..., group_name = NULL) {
  # Convert individual targets to grouped targets class
  return_targets <- structure(list(...), class = c("target", "group", "imabc"))

  # Add assigned names to target_name element if target_name element is missing
  name_vec <- get_list_element(return_targets, "target_name", unlist = TRUE)
  give_name <- is.na(name_vec) & names(name_vec) != ""
  name_vec[give_name] <- names(name_vec)[give_name]
  for (i1 in seq_along(return_targets)) {
    if (is.na(return_targets[[i1]]$target_name) || is.null(return_targets[[i1]]$target_name)) {
      return_targets[[i1]]$target_name <- name_vec[i1]
    }
  }

  # Give group name if it has been given
  return_targets$group_name <- ifelse(is.null(group_name), NA, group_name)

  return(return_targets)
}
