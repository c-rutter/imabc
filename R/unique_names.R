unique_names <- function(x, name_vec) {
  UseMethod("unique_names")
}

unique_names.priors <- function(x, name_vec) {
  # Parameter Name formats
  expected_fmt <- "V%s"
  gsub_pattern <- "(^V)([0-9]+)"

  # Build names
  names <- .build_names(things_list = x, provided_names = name_vec, fmt = expected_fmt, pattern = gsub_pattern)

  return(names)
}

unique_names.targets <- function(x, name_vec) {
  # Target Name formats
  expected_fmt <- "T%s"
  gsub_pattern <- "(^T)([0-9]+)"

  # Add group name to name vec for names that appear as both group and target names
  group_names <- attr(x, "groups")
  group_targs <- attr(x, "grouped")
  if (!is.null(group_names) && any(group_targs)) {
    # Handle group names
    name_vec <- ifelse(
      !is.na(group_names) & !is.na(name_vec) & (duplicated(name_vec, fromLast = TRUE) | duplicated(name_vec, fromLast = FALSE)),
      paste(name_vec, group_names, sep = "_"), # Grouped targets
      name_vec # Non-grouped targets
    )
  }

  # Build names
  names <- .build_names(things_list = x, provided_names = name_vec, fmt = expected_fmt, pattern = gsub_pattern)

  # Don't allow target and group names to have the same value
  if (any(names %in% group_names)) { stop("Targets and Target Groups cannot share a name.") }

  return(names)
}

unique_names.groups <- function(x, name_vec) {
  # Group Name formats
  expected_fmt <- "G%s"
  gsub_pattern <- "(^G)([0-9]+)"

  # Build names
  names <- .build_names(things_list = x, provided_names = name_vec, fmt = expected_fmt, pattern = gsub_pattern)

  return(names)
}

.build_names <- function(things_list, provided_names, fmt, pattern) {
  current_names <- names(things_list)
  if (is.null(current_names)) {
    # Initialize name list
    current_names <- rep("", length(things_list))

    # Find parameters with an assigned name
    has_names <- !is.na(provided_names)
    current_names[has_names] <- provided_names[has_names]
  } else {
    # Special cases to handle when names are being used
    multi_names <- names(things_list) != "" & !is.na(provided_names)
    no_names <- names(things_list) == "" & is.na(provided_names)
    one_name <- !multi_names & !no_names

    # One name specified using list element
    current_names[one_name & current_names == ""] <- provided_names[one_name & current_names == ""]

    # Names specified using both naming methods - use name specified within function
    current_names[multi_names] <- provided_names[multi_names]
  }

  # Create final list of unique names
  if (any(current_names == "")) {
    # Generate unique names for objects that need names
    PickOut <- which(current_names == "")
    newnew <- sprintf(fmt, PickOut)
    # If any names already match the format our new names
    if (any(newnew %in% current_names)) {
      # Find the highest numeric value from the names that match our format
      tmp <- gsub(pattern, "\\2", current_names)
      tmp <- suppressWarnings(as.numeric(tmp))
      tmp <- max(tmp, na.rm = TRUE)
      # Change the values we will generate new names with to start with the highest already used and move up from there
      PickOut2 <- (tmp + 1):(tmp + length(PickOut))
    } else {
      PickOut2 <- PickOut
    }
    current_names[PickOut] <- sprintf(fmt, PickOut2)
  }

  # Check for uniqueness
  stopifnot(
    "Names must be unique." = length(current_names) == length(unique(current_names)),
    "Object names must be syntactically valid R names - see '?make.names' for more details" = all(current_names %in% make.names(current_names))
  )

  return(current_names)
}
