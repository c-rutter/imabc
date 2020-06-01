get.alpha <- function(sim.p,sim.p.names,target.data,specs=NULL){
  # TO DO: revise this so that returns missing values for any rows
  #        that have missing sim.p. currently causes error/fail 
  #        correction needs to occur in calc.alpha
  p.value=calc.alpha(sim.p,sim.p.names,target.data,specs)
  
  p.min = apply(p.value, 1, min)
  p.min[p.min<=1e-28 & !is.na(p.min)] = 0
  
  return(p.min)
}