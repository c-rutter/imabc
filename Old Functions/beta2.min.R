# sample growth.beta2 conditional on growth.beta1
# relies on growth.beta

beta2.min <- function(beta1,Range.p10mmIn10yrs,unif.lim){
  pmax(growth.beta(p=Range.p10mmIn10yrs[2],alpha=beta1),as.numeric(unif.lim[1]))
}