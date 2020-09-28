#get upper and lower bounds

get.bounds <- function(x,alpha.level=1e-9){
    x$lo.lim = NA
    x$up.lim = NA
    x$lo.lim[x$binomial] = lo.lim(p=x$p[x$binomial],n=x$n[x$binomial],alpha=alpha.level)
    x$up.lim[x$binomial] = up.lim(p=x$p[x$binomial],n=x$n[x$binomial],alpha=alpha.level) 
    x$lo.lim[x$poisson] = x$p[x$poisson] + qnorm(alpha.level/2)*sqrt(x$p[x$poisson]/x$n[x$poisson])
    x$up.lim[x$poisson] = x$p[x$poisson] - qnorm(alpha.level/2)*sqrt(x$p[x$poisson]/x$n[x$poisson])
  return(x) 
}

