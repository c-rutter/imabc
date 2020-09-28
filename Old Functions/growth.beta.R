#calculate growth for beta. Used by sample.beta2

growth.beta <- function(p=0.03,alpha){
  10*(-log(p))^(1/alpha)
}