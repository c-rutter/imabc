# sample growth.beta2 conditional on growth.beta1
# relies on growth.beta

beta2.max <- function(beta1,Range.p10mmIn10yrs,unif.lim){
  pmin(growth.beta(p=Range.p10mmIn10yrs[1],alpha=beta1),as.numeric(unif.lim[2]))
}