sourceFolder <- function(folder, recursive = FALSE, local=TRUE, ...){

  wd <- getwd()
  setwd(folder)
  files <- list.files(pattern = "[.][rR]$", full.names = TRUE, recursive = recursive)
   if (!length(files)){
    setwd(wd)
    stop(simpleError(sprintf('No R files in folder "%s"', folder)))
   }
  src <- invisible(sapply(files, source, local, ...))

  message(sprintf('%s files sourced from folder "%s"', ncol(src), folder))
  setwd(wd)

}

