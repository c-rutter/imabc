.validate_prior_function <- function(x) {
  # Check that function provided exists
  fn_name <- eval(x)
  e <- sprintf("The function %s cannot be found. Check the spelling or that it is loaded into your environment", eval(fn_name))
  if (!exists(fn_name, mode = 'function')) { stop(e) }

  return(fn_name)
}
