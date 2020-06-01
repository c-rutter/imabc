initialize.integer.NA <- function(dt,var){
  if(var %in% names(dt)){ dt[,(var) := NULL]}
  dt[,(var) := as.integer(NA)]
}