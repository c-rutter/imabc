
# Create an empty individual log.like array
get.empty.indiv.dist <- function(){
  n.dist=length(get.dist.names())
  target.dist <- as.numeric(rep(NA,n.dist))
  names(target.dist)=get.dist.names()
  return(target.dist)
}

