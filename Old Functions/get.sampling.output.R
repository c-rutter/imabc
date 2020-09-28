get.sampling.output = function(iter,mu,sd,center,B.in,parm.names){
  # put mean and SD into format expeced in outfile.sampling

  save.names = NULL
  if(is.vector(mu)){
    save.names=names(mu)
    mu=matrix(mu,byrow=TRUE,nrow=1)
  }
  n.parm=length(parm.names)
  n.centers=nrow(mu)
  mu=data.frame(mu)
  if(!is.null(save.names)) names(mu)=save.names
  mu.use = mu[ , which(names(mu) %in% parm.names)]
  
  sd=data.frame(sd)
  names(sd)=names(mu)
  sd.use = sd[, which(names(mu) %in% parm.names)]
  
  numeric.NA=as.numeric(NA)
  x.out <- setnames(data.table(matrix(numeric.NA,
                                      nrow=n.centers*(n.parm+1),
                                      ncol=n.parm)),
                    parm.names)

  for(i in 1:n.centers){
    x.out[((i-1)*(n.parm+1) + 1),] <- mu.use[i,]
    x.out[((i-1)*(n.parm+1) + 2):(i*(n.parm+1)),] <- setnames(data.frame(diag(sd.use[i,])),parm.names)
  }
  
  x.out$iter = iter
  x.out$step = rep(1:n.centers,each=(1+n.parm))
  x.out$center = rep(center,each=(1+n.parm))
  x.out$parm = rep(0:(n.parm),n.centers)

  setkey(x.out,iter,step)
  setkey(B.in,iter,step)
 
  x.withB=x.out[B.in]
  setcolorder(x.withB,c("iter","step","center","B.in","parm",parm.names))
  
  x.withB

}