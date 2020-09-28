# random seed strings
get.random.seed.strings <- function(s,n.seeds){
  random.seed.strings <- rep("",n.seeds)
  for (i in 1:n.seeds){
    s <- nextRNGStream(s)
    random.seed.strings[i] <- paste0(s,collapse = "_")
  }
  random.seed.strings
}