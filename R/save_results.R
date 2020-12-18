save_results <- function(..., out_dir, append) {
  dots <- list(...)

  # Control for only a single file being saved
  if (length(dots) == 2 && class(dots[[1]]) != "list") {
    dots <- list(dots)
  }

  # Ensure names are correct
  for (i1 in 1:length(dots)) {
    n_subs <- length(dots[[i1]])
    # Check that we have the appropriate inputs
    if (n_subs != 2) {
      stop("save_results: Please provide a dataset and a name to save the file to.")
    }

    # Names of sublist provided
    old_names <- names(dots[[i1]])

    # If name information is missing or incomplete
    if (length(old_names) == 0 || any(old_names == "")) {
      # Initialize new name vector
      new_names <- rep(NA_character_, n_subs)

      # For each object in sublist determine the most likely name
      for (i2 in 1:n_subs) {
        new_names[i2] <- ifelse(all(class(dots[[i1]][[i2]]) == "character"), "name", "dt")
      }
      names(dots[[i1]]) <- new_names
      dots[[i1]]$dt <- as.data.frame(dots[[i1]]$dt)
    } # length(old_names) == 0 || any(old_names == "")
  }

  # Store results quietly
  invisible(
    lapply(dots, FUN = function(x, loc, app) {
      write.table(
        x = x$dt,
        file = paste0(loc, "/", x$name),
        sep = ",",
        append = app,
        col.names = !app,
        row.names = FALSE
      )
    }, loc = out_dir, app = append)
  )
}
