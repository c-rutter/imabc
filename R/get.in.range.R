get.in.range <- function(parms,priors,p.names){
  in.range = rep(TRUE,length=nrow(parms))
  for(i in 1:length(p.names)){
    in.range = in.range & 
      parms[[p.names[i]]]>=priors$c[priors$parm==p.names[i]] &
      parms[[p.names[i]]]<=priors$d[priors$parm==p.names[i]]
  }
  in.range=as.numeric(in.range)
  in.range[is.na(in.range)]=0
  
  return(in.range)
  
}