# written for TargetSims function
get.biggest <- function(adenoma.in=adenoma){
  # identify the largest adenoma within person
  # returns a data frame
  adenoma.dt = data.table(adenoma.in)
  setkey(adenoma.dt,p.id)
  largest.idx = adenoma.dt[, .(n=.N,wm=which.max(size)), by=p.id]
  largest.idx$nc = c(0,cumsum(largest.idx$n[-nrow(largest.idx)]))
  adenoma.largest = adenoma.dt[largest.idx$nc+largest.idx$wm,]
  data.frame(adenoma.largest)
}
