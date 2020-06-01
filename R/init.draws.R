# initialize draws data structure

init.draws <- function(N,x.names){
   x <- data.table(iter=as.integer(rep(NA,N)),
                  draw=as.integer(rep(NA,N)),
                  step=as.integer(rep(NA,N)),
                  seed=as.character(rep("",N)))
   x[ , (x.names) := as.numeric(NA)]

   x
   
}