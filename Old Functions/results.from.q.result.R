# Parse string representation of results from queues
results.from.q.result <- function(q.res,names = c("ll","sp")){
  temp0 <- unlist(strsplit(q.res,"#"))
  temp1 <- strsplit(temp0,";")
  # first
  first_rows <- lapply(temp1,function(x) as.numeric(unlist(strsplit(x[1],split = ","))))
  first_dt <- data.table(do.call(rbind,first_rows))
  # second
  second_rows <- lapply(temp1,function(x) as.numeric(unlist(strsplit(x[2],split = ","))))
  second_dt <- data.table(do.call(rbind,second_rows))
  res <- list(first_dt, second_dt)
  names(res) <- names
  return(res)
}