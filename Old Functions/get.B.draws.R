get.B.draws <- function(B,inflate,center,cov,priors,p.names){
  x=as.data.table(mvrnorm(n = trunc(inflate*B), mu=center, Sigma=cov))
  x$in.range=get.in.range(parms=x,priors=priors,p.names=p.names)
  if(sum(x$in.range)>0){
    setorder(x,-in.range)
    x$in.range=NULL
    return(x[1:B,])
  }else{
    print(paste("error: no parameter draws are in range with inflate=",inflate))
    return(NULL)
  }
}
