get.update.targets <- function(x, y){
  z=NULL
  if(length(y)>0)
  for(i in 1:length(y)){ 
    z=c(z,x[x %like% y[i]]) 
  }
  return(z)
}