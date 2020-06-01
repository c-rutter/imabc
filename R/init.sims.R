init.sims <- function(N,x.names){
  x <- data.table(iter=as.integer(rep(NA,N)),
                  draw=as.integer(rep(NA,N)),
                  step=as.integer(rep(NA,N)))
  x[ , (x.names) := as.numeric(NA)]
  
  x
  
}