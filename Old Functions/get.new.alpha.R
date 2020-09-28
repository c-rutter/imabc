get.new.alpha <- function(alpha,n.keep=50){
  return(alpha[order(-alpha,na.last=TRUE)][n.keep])
}