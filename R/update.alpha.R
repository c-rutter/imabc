update.alpha <- function(sim.p,sim.p.names,target.data,specs=NULL){

  update.names=specs$data[specs$alpha<specs$alpha.target]
  
  bias.info=grepl(".bias",update.names)
  target.names=update.names[!bias.info]
  bias.names=update.names[bias.info]
  if(length(bias.names)>0){
    bias.names = gsub(".bias", "", bias.names)
  }
  
  p.value=calc.alpha(sim.p,sim.p.names,target.data,specs)
 
  p.min=rep(NA,length(update.names))
  names(p.min)=update.names
  if(length(target.names)>0){
    for(i in 1:length(target.names)){
      use.targets = grepl(target.names[i],sim.p.names)
      use.targets.names = sim.p.names[use.targets]
  
      p.min[[target.names[i]]] = min(p.value[,use.targets.names,with=FALSE])
    }
  }
  
  if(length(bias.names)>0){
    for(i in 1:length(bias.names)){
      p.min[[paste0(bias.names[i],".bias")]] = p.min[[bias.names[i]]]
    }
  }
  
  p.min[p.min<=1e-28 & !is.na(p.min)] = 0
  
  return(p.min)
}  