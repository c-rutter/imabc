
# Create string representation of parameters to pass to queues
parms.dt.to.string <- function(dt){
  paste0(apply(dt,1,paste0,collapse = ","), collapse = ";")
}
