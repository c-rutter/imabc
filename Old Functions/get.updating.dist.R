get.updating.dist <- function(x,target.names=NULL){

  x$updating.dist = 0
  
  # if target.names is null, calculate distance across all targets
  if(length(target.names)==0){
    drop.names = c("iter","draw","step","tot.dist","alpha","n.good")
    names.x=names(x)
    target.names=names.x[!(names.x %in% drop.names)]
    SEER.add=rep(FALSE,length(target.names))
  }else{
    
    bias.info=grepl(".bias",target.names)
    target.names=target.names[!bias.info]

    SEER.add=grepl("SEER",target.names)
  }
  
  for(i in 1:length(target.names)){
    if(!SEER.add[i]){
      x[["updating.dist"]] = x[["updating.dist"]] + x[[target.names[i]]]^2
    }else{
      x[["updating.dist"]] = x[["updating.dist"]] +
          x[[paste0(target.names[i],".colon.fem")]]^2 +
          x[[paste0(target.names[i],".rectal.fem")]]^2 +
          x[[paste0(target.names[i],".colon.male")]]^2 +
          x[[paste0(target.names[i],".rectal.male")]]^2 
      }
  }

  return(sqrt(x[["updating.dist"]]))
}