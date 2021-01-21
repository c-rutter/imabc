combine_results <- function(iterator, verbose = TRUE) {
  if (verbose) {
    # Create a progress bar to help track the runs
    pb <- txtProgressBar(min = 1, max = iterator - 1, style = 3)
    count <- 0
    function(...) {
      count <<- count + length(list(...)) - 1
      setTxtProgressBar(pb, count)
      flush.console()
      as.data.frame(rbind(...))
    }
  } else {
    # Just row bind the items
    function(...) {
      as.data.frame(rbind(...))
    }
  }
}
