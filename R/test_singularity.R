test_singularity <- function(x, tol) {
  # Initialize return value to FALSE
  singular <- FALSE

  # Get eigen values
  e_value <- eigen(x, tol)$values
  if (any(e_value < tol)) { singular <- TRUE }

  return(singular)
}
