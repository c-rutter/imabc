get.alpha.draw <- function(x,n.keep=50){
  return(x[order(-alpha,tot.dist,na.last=TRUE)][n.keep]$draw)
}
