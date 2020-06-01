# Indices function
get_i.out <- function(target.specs){
  i.out=data.frame(matrix(0,nrow=2,ncol=nrow(target.specs)))
  names(i.out)=target.specs$data
  i.out[1,1]=1 # all other indices are keyed off of this one
  if(nrow(target.specs)==1){
    loop.range=NULL
  }else{
    loop.range=1:(nrow(target.specs)-1)
  }
  for(i in loop.range){
    i.out[2,i]=i.out[1,i]+target.specs$n.ret[i]-1
    i.out[1,i+1]=i.out[2,i]+1
  }
  i=nrow(target.specs)
  i.out[2,i] = i.out[1,i] + target.specs$n.ret[i]-1
  return(i.out)
}
