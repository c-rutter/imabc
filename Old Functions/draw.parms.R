draw.parms <- function(n.add,mu,sigma,parm.priors,parm.names,calib.targets){
  # independently draw parameters values, with the exception of beta2, sampled conditional on beta2
  # parm.priors columns are: parm,norm.dist,a,b,c,d
  # parm: parameter name, 
  # a,b: hyper-parameters corresponding to N(a,b) or U(a,b)
  # c,d: the prior range, [c,d] for U(a,b) we expect a=c and b=d
  
  if(is.vector(mu)){
    mu=matrix(mu,byrow=TRUE,nrow=1)
  }
  n.centers = nrow(mu)
  n.parms = ncol(mu)

  draws = data.table(data = matrix(0,nrow=n.add*n.centers,ncol=n.parms))
  u.draws=matrix(runif(2*n.add*n.centers),ncol=2,nrow=n.add*n.centers)
  setnames(draws,parm.names)
  Range.p10mmIn10yrs=unlist(calib.targets[["Range.p10mmIn10yrs"]])
  
  for(j.center in 1:n.centers){
    row.range = ((j.center-1)*n.add + 1):(j.center*n.add)
    for(i.parm in 1:n.parms){  
      if(parm.priors[i.parm,'prior.dist']==3){
        beta1 = gsub("2", "1", parm.names[i.parm])
        unif.lim=parm.priors[parm.priors$parm==parm.priors[i.parm,'parm'],c('a','b')]
        
        lower.bound=beta2.min(draws[[beta1]][row.range],
                              Range.p10mmIn10yrs,
                              unif.lim)
        upper.bound=beta2.max(draws[[beta1]][row.range],
                              Range.p10mmIn10yrs,
                              unif.lim)

       }else{
          lower.bound=parm.priors$c[i.parm]
          upper.bound=parm.priors$d[i.parm]
       }
      
      draws[row.range,parm.names[i.parm] := rtruncnorm(n.add,
                                                      a=lower.bound,
                                                      b=upper.bound,
                                                      mean=mu[j.center,i.parm],
                                                      sd=sigma[j.center,i.parm])]

    } # for(i.parm in 1:n.parms)

  } # over centers
  return(draws)
}




