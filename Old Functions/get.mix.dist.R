get.mix.dist <- function(p.names,mixture.file){

  n.parms.now = length(p.names)
  for(i in 1:length(mixture.file)){
    
    x = data.table(read.table(file=mixture.file[i],
                              header=TRUE,
                              sep=",",
                              na.strings="NA"))
    
    n.parms.x = ncol(x) - 5
   
    x.parm.order=names(x)[!names(x) %in% c("iter","step","center","B.in","parm")]
    if(!(all(x.parm.order %in% p.names))){  # test that this works
      print(paste0("error, parameters varied in previous calibration are not included: ",
            paste(x.parm.order[!(x.parm.order %in% p.names)],collapse=", ") ))
      break()
    }
    
    # check to see if there are any new  parameters
    if(any(!(p.names %in% x.parm.order))){ 
      p.names.in = p.names[p.names %in% x.parm.order]
      p.names.out = p.names[!(p.names %in% x.parm.order)]
      
      # set parms not included to missing: allows stacking of mean.cov
      x[,(p.names.out):=NA]
      
    }

    new.parmnum = rep(0,n.parms.x)
    for(j in 1:n.parms.x){
      new.parmnum[j]=which(p.names==x.parm.order[j])
    }

    setcolorder(x,c("iter","step","center","B.in","parm",p.names))
    n.mix=length(unique(10*max(x$step)*x$iter + x$step))
    x$parm.num = rep(c(0,new.parmnum),n.mix)
    setkey(x,iter,step,center,parm.num)
    x$parm=x$parm.num
    x$parm.num=NULL

    if(i==1){
      mean.cov=x
    }else{
      first.iter=x$iter[1]          
      n.keep = (n.parms.now+1)*(first.iter-1) # keep info up to first iter in next file
      mean.cov=mean.cov[iter<=n.keep,]
      mean.cov=rbind(mean.cov,x)
    }
    
  } # for(i in 1:length(mixture.file))
 
  return(mean.cov)
}