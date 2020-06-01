
get.dist = function(p.draws,p.names,mu,sd){
  # calculate scaled distance from each center point
  
  sd[sd==0]=1
  x=p.draws[,(p.names),with=FALSE]
  
  x <- sweep(x,2,mu,"-")
  x <- x^2
  x <- sweep(x,2,sd,"/")
  x.sum = apply(x, 1, sum)
  
  return(x.sum)
}
