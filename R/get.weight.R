get.weight <- function(parms,p.names,priors,mixture.file,N0){

  log.prior.d = get.log.prior.d(parms,p.names,priors)
  sampling.d  =  get.sampling.d(parms,p.names,priors,mixture.file)
  mix.wt = 1/log1p(sampling.d/exp(log.prior.d)/N0)
 
  return(mix.wt/sum(mix.wt))

}