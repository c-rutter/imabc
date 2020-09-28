# lower limit
lo.lim <- function(p,n,alpha){p+qnorm(alpha/2)*sqrt(p*(1-p)/n)}