# create the function get.dist.names
make.get.dist.names <- function(x) {
  dist.names=x[!(x %like% "SEER")]
  if(any(x %like% "SEER")){
    dist.names = c(dist.names,
               paste0(x[(x %like% "SEER")],
                      c(".colon.fem",".rectal.fem",".colon.male",".rectal.male")))
  }
  get.dist.names = function(){
    dist.names
  }
  return(get.dist.names)
}
