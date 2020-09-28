p.value <- function(p.obs,p.sim,n){
  z.score = -1*abs((p.sim - p.obs) /sqrt(p.obs*(1-p.obs)/n))
  p.val = 2*pnorm(z.score)
  return(p.val)
}
