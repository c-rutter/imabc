# Transform the data type expected from LikelihoodsTarget
# to result expected in q.res
make.into.q.res <- function(resFull){
  resLString <- paste0(
    lapply(resFull,function(x) paste0(
      lapply(x,function(y) paste0(y,collapse = ",")),
      collapse = ";")),
    collapse = "#")
  return(resLString)
}
