sigfigs <- function(x){
  orig_scipen <- getOption("scipen")
  options(scipen = 999)
  on.exit(options(scipen = orig_scipen))
  
  x <- as.character(x)
  x <- sub("\\.", "", x)
  x <- gsub("(^0+|0+$)", "", x)
  nchar(x)
}