get.calib.target.names <- function(sim.parm.names,target.specs,calib.targets){

  calib.target.names=NULL
  for(i in 1:(nrow(target.specs))){
    target.name=target.specs$data[i]
    n.targets = nrow(calib.targets[[target.name]])
    calib.target.names = c(calib.target.names,
                           sim.parm.names[grepl(target.name,sim.parm.names)][1:n.targets])
  }
  return(calib.target.names)
  
}
