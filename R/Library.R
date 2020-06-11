##### Locates files #####
# Defines directory with files
library.loc <- "R/"
# Creates list of files at directory
l <- list.files(path = library.loc)
# Removes non-R files
l <- l[grep("\\.R", l)]
# Checks for tilde's
tilde <- grep("\\.R~", l)
# If tilde's exist, files are removed from list
if (length(tilde) > 0) {
  l <- l[-tilde]
}
# search for ip
inProg <- grep("_ip", l)
if (length(inProg) > 0) {
  l <- l[-inProg]
}
# Removes this script from list
l <- l[l != "Library.R"]

##### Source #####
# For every script in list, loads the file to the global environment
s <- sapply(l, FUN = function(x, library.loc) {
  sys.source(file.path(library.loc, x), .GlobalEnv)
}, library.loc)
# Cleans memory
rm(s, l, tilde)