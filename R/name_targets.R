name_targets <- function(target_list, grouping = TRUE) {
  # grouping == TRUE means the function is being used in group_targets()
  # grouping == FALSE means the function is being used in define_targets()
  # DO NOTE EXPORT

  expected_fmt <- ifelse(grouping, "s%s", "m%s")
  gsub_pattern <- ifelse(grouping, "(^s)([0-9]+)", "(^m)([0-9]+)")
  # Pull current names
  new_names <- names(target_list)

  if (is.null(new_names)) {
    # Generate complete set of names
    new_names <- sprintf(expected_fmt, 1:length(target_list))
  } else if (any(new_names == "")) {
    # Generate unique names for targets that need names
    PickOut <- which(new_names == "")
    newnew <- sprintf(expected_fmt, PickOut)
    # If any target names already match the format our new names
    if (any(newnew %in% new_names)) {
      # Find the highest numeric value from the names that match our format
      tmp <- gsub(gsub_pattern, "\\2", new_names)
      tmp <- suppressWarnings(as.numeric(tmp))
      tmp <- max(tmp, na.rm = TRUE)
      # Change the values we will generate new names with to start with the highest already used and move up from there
      PickOut2 <- (tmp + 1):(tmp + length(PickOut))
    } else {
      PickOut2 <- PickOut
    }
    new_names[PickOut] <- sprintf(expected_fmt, PickOut2)
  }

  return(new_names)
}
