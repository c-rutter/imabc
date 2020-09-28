# test matrix singularity

test.singularity = function(x,tol){
  singular <- FALSE
  e.value <- eigen(x,tol)$values
  if(any(e.value<tol)) singular <- TRUE
  singular
}
