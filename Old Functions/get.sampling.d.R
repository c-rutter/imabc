get.sampling.d <- function(parms,p.names,priors,mixture.file){

  mean.cov = get.mix.dist(p.names,mixture.file)
  last.iter=max(mean.cov$iter)
  n.mix = nrow(mean.cov[parm==0,])
  B.draws=unique(mean.cov[,c("iter","step","B.in"),with=FALSE])
 
  parm.mat = as.matrix(sapply(parms[,p.names,with=FALSE],as.numeric))
  if(nrow(parms)==1) parm.mat = t(parm.mat)  # set to a n.parm x 1 matrix when n.good==1
  
  sum.H=rep(0,nrow(parms))
  
  for(i in 1:n.mix){
    iter.i = B.draws$iter[i]
    step.i = B.draws$step[i]
    B.i = B.draws[iter==iter.i & step==step.i,]$B.in

    center.i = as.vector(as.numeric(
      mean.cov[iter==iter.i & step==step.i & parm==0,p.names,with=FALSE]
    ))
    is.calib = (1:length(center.i))[!is.na(center.i)]

    Sigma.i =  as.matrix(sapply(
      mean.cov[iter==iter.i & step==step.i & parm>0,p.names,with=FALSE],
      as.numeric))
  
    Sigma.i = Sigma.i[,is.calib]
    center.i = center.i[is.calib]
    if(nrow(Sigma.i)!=ncol(Sigma.i)) browser()
    sum.H = sum.H +  B.i*dMvn(X=parm.mat[,is.calib],mu=center.i,Sigma=Sigma.i)
  }
  
  return(sum.H)
  
}
