test_singularity = function(x, tol) {
  singular <- FALSE
  e_value <- eigen(x, tol)$values
  if (any(e_value < tol)) { singular <- TRUE }

  return(singular)
}
