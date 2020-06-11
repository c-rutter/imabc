update.in.range <- function(target.dist,sim.parm,target.specs,calib.targets){
# rewrite to check all at once, using the approach in DistTarget

  bias.info=grepl(".bias",target.specs$data)
  bias.names=target.specs$data[bias.info]
  if(length(bias.names)>0){
    bias.names = gsub(".bias", "", bias.names)
  }
  
  use.targets=target.specs$data[!bias.info]
  sim.p.names=names(sim.parm)
  
  for(i in 1:nrow(target.specs[!bias.info,])){

    target.name = use.targets[i]
    target.ind = grepl(target.name,sim.p.names)
    use.targets.names = sim.p.names[target.ind][1:nrow(calib.targets[[target.name]])]
    sim.res = sim.parm[,use.targets.names,with=FALSE]

    if(any(grepl("SEER",target.name))){
      
      SEER.names= paste0(target.name,c(".colon.fem",".rectal.fem",".colon.male",".rectal.male"))

      #  4 distances for SEER: this order is fixed and aligns with SimTarget, 
      #   code could be improved so that it doesn't ref column nums            
      for(i in 1:4){
        c.range=(5*(i-1)+1):(5*i)
        sub.sim.res=sim.res[,use.targets.names[c.range],with=FALSE]
        
        in.range= apply(t(t(sub.sim.res) >= calib.targets[[target.name]]$lo.lim[c.range] &
                          t(sub.sim.res) <= calib.targets[[target.name]]$up.lim[c.range] ),
                          1, 
                          all)
        
          if(any(!in.range)){
            target.dist[!in.range,][[SEER.names[i]]] = -1*abs(target.dist[!in.range,][[SEER.names[i]]])
          }      

      }
    }else{
   
      if(target.name %in% bias.names){ 
        
         in.range= apply(t(t(sim.res) >= calib.targets[[target.name]]$lo.lim &
                           t(sim.res) <= calib.targets[[paste0(target.name,".bias")]]$up.lim ),
                          1, 
                          all)
        
          

      }else{

        in.range= apply(t(t(sim.res) >= calib.targets[[target.name]]$lo.lim &
                          t(sim.res) <= calib.targets[[target.name]]$up.lim ),
                        1, 
                        all)
        
      }
  
      target.dist[[target.name]] = abs(target.dist[[target.name]])
      if(any(!in.range)){
        target.dist[!in.range,][[target.name]] = -1*abs(target.dist[!in.range,][[target.name]])
      }      
      
        
      }
    }
      
  return(target.dist)

}
