.unique_names <- function(things_list, thing = c("parameter", "target_name", "target_group")) {
  # DO NOT EXPORT
  thing <- match.arg(thing, choices = c("parameter", "target_name", "target_group"))
  # Get appropriate format and pattern
  expected_fmt <- ifelse(
    thing == "parameter", "V%s", ifelse(
      thing == "target_name", "T%s", ifelse(
        thing == "target_group", "G%s", NA
      )))
  gsub_pattern <- ifelse(
    thing == "parameter", "(^V)([0-9]+)", ifelse(
      thing == "target_name", "(^T)([0-9]+)", ifelse(
        thing == "target_group", "(^G)([0-9]+)", NA
      )))

  # Assign names to all vectors
  prior_names <- names(things_list)
  if (is.null(prior_names)) {
    # Initialize name list
    prior_names <- rep("", length(things_list))

    # Find parameters with an assigned name
    has_names <- unlist(lapply(things_list, FUN = function(x) { !is.null(x[[thing]]) }))
    prior_names[has_names] <- unlist(lapply(things_list[has_names], FUN = function(x) { x[[thing]] }))
  } else {
    # Special cases to handle when names are being used
    multi_names <- names(things_list) != "" & unlist(lapply(things_list, FUN = function(x) { !is.null(x[[thing]]) }))
    no_names <- names(things_list) == "" & unlist(lapply(things_list, FUN = function(x) { is.null(x[[thing]]) }))
    one_name <- !multi_names & !no_names

    # One name specified using input name
    # Already taken care of by way prior_names is initialized

    # One name specified using list element
    prior_names[one_name & prior_names == ""] <- unlist(lapply(things_list[one_name & prior_names == ""], FUN = function(x) { x[[thing]] }))

    # Names specified using both naming methods
    # Use name specified within function
    prior_names[multi_names] <- unlist(lapply(things_list[multi_names], FUN = function(x) { x[[thing]] }))
  }

  # Create final list of unique names
  if (any(prior_names == "")) {
    # Generate unique names for objects that need names
    PickOut <- which(prior_names == "")
    newnew <- sprintf(expected_fmt, PickOut)
    # If any names already match the format our new names
    if (any(newnew %in% prior_names)) {
      # Find the highest numeric value from the names that match our format
      tmp <- gsub(gsub_pattern, "\\2", prior_names)
      tmp <- suppressWarnings(as.numeric(tmp))
      tmp <- max(tmp, na.rm = TRUE)
      # Change the values we will generate new names with to start with the highest already used and move up from there
      PickOut2 <- (tmp + 1):(tmp + length(PickOut))
    } else {
      PickOut2 <- PickOut
    }
    prior_names[PickOut] <- sprintf(expected_fmt, PickOut2)
  }

  # Check for uniqueness
  stopifnot(
    "Names must be unique." = length(prior_names) == length(unique(prior_names))
  )

  return(prior_names)
}
