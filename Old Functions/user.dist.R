user.dist <- function(u,parm.name,parm.draws,parm.priors,calib.targets){
  # sample growth.beta2 from a uniform, conditional on growth.beta1
  # relies on beta2.min and beta2.max, which are also used by DistTarget()
  # the ordering of the paramter vector needs to account for any user dependencies,
  # sampling independent parameters before parms that depend on them.

    beta1 = gsub("2", "1", parm.name)
    
    range.p10mmIn10yrs=unlist(calib.targets[['Range.p10mmIn10yrs']])
    
    unif.lim=parm.priors[parm.priors$parm==parm.name,c('a','b')]

    ll = beta2.min(parm.draws[[beta1]],range.p10mmIn10yrs,unif.lim)
    
    ul = beta2.max(parm.draws[[beta1]],range.p10mmIn10yrs,unif.lim)
    
    return(u*(ul-ll) + ll)
    
  
}
