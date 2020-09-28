read.multi.runs <- function(in.dir,f.name,n.in=NULL,ch.seed=FALSE,int.cols,num.cols,
                            get.B=FALSE){
  big.B=NULL
  big.iter=NULL
  big.x=NULL
  for(i in 1:length(in.dir)){
    B=NULL
    last.iter=NULL
    x=data.table(read.table(file=paste(in.dir[i],f.name,sep="/"),
                            header=TRUE,
                            sep=",",
                            na.strings="NA"))
    if(ch.seed) x$seed = as.character(x$seed)
    x[, (int.cols) := lapply(.SD, as.integer), .SDcols=int.cols]
    x[, (num.cols) := lapply(.SD, as.numeric), .SDcols=num.cols]
    if( !(is.null(n.in[i]))){
      x=x[iter<=n.in[i],]
    }
    if(any(x$iter>0)){
      last.iter=max(x$iter)
      B=nrow(x[iter==last.iter & step==1,])
    }
    big.x=rbind(big.x,x)
    big.B=c(big.B,B)
    big.iter=c(big.iter,last.iter)
  }
  if(get.B){
    return(list(big.B,big.iter,big.x))
  }else{
    return(big.x)
  }
}