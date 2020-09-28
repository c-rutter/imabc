get.log.prior.d <- function(parms,p.names,priors){

  n.parms = length(p.names)
  prev.parm.names=names(parms)[!names(parms) %in% c("seed","iter","step","draw","scaled.dist","sample.wt")]

  # calculate log(prior density) for normally distributed paramters
  # line below for backward compatibility, can later remove
  if("prior.dist" %in% names(priors)) priors$norm.dist = priors$prior.dist
  parms$log.prior.d = 0
  for(i in 1:n.parms){
    prior.i = priors[priors$parm==p.names[i],]
    if((prior.i$norm.dist==1) & (p.names[i] %in% prev.parm.names)){
      parms[, log.prior.d := log.prior.d +  log(dtruncnorm( x=parms[[p.names[i]]],
                                                            a=prior.i$c,
                                                            b=prior.i$d,
                                                            mean = prior.i$a,
                                                            sd=prior.i$b))]
    }
  }
  
  return(parms$log.prior.d)
  
}