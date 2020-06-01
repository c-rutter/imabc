split.into.param.lines <- function(x){
  
  res1 <- unlist(strsplit(x,split = ";"))
  res_seed_info <- lapply(strsplit(res1,split = ","), function(x) as.integer(unlist(strsplit(x[1],"_"))))
  res_parms <- lapply(strsplit(res1,split = ","), function(x) as.numeric(x[2:length(x)]))
  list(parms = res_parms, seed_info = res_seed_info)
}