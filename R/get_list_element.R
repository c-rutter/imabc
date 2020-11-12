get_list_element <- function(x, name = NULL, unlist = FALSE, keep_length = TRUE) {
  # Pull element(s) of a list
  if (is.null(name)) { # No name is provided to search for
    vec <- x

  } else { # Name(s) is(are) provided to search for
    vec <- lapply(x, FUN = function(y) {
      if (keep_length && any(sapply(y[name], is.null))) {
        new_out <- y[name]
        not_in <- which(sapply(y[name], is.null))
        new_out[not_in] <- NA
        names(new_out)[not_in] <- name[not_in]
        new_out
      } else {
        y[name]
      }
    })

  }

  # Convert list to vector
  if (unlist) {
    nas <- names(vec)
    vec <- unlist(vec)
    names(vec) <- nas
  }

  return(vec)
}
