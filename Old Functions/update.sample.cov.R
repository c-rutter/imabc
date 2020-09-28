update.sample.cov= function(x.cov,x.mean){
  cv=(x.mean/sqrt(diag(x.cov)))
  drop.order=order(-abs(cv))
  
  n.parm=length(x.mean)
  all.v=1:n.parm
  
  for(drop.i in 1:(n.parm-2)){
    keep.v = all.v[all.v %!in% drop.order[1:drop.i]]
    if(!test.singularity(x.cov[keep.v,keep.v],1e-8)){break}
  }
  if((drop.i==(n.parm-2)) & (test.singularity(x.cov[keep.v,keep.v],1e-8))){
    new.cov=diag(x=diag(x.cov), nrow=n.parm, ncol=n.parm)
  }else{
    # for dropped terms, set all off-diagonals to zero
    new.cov=x.cov
    new.cov[drop.order[1:drop.i],all.v]=0
    new.cov[all.v,drop.order[1:drop.i]]=0
    diag(new.cov)[drop.order[1:drop.i]]=
      diag(x.cov)[drop.order[1:drop.i]]
    
  }
  
  return(new.cov)
}
