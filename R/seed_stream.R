seed_stream <- function(seed_stream_start, length.out) { # same as get.random.seed.strings.R
  # Initialize vector
  stream <- rep(NA_character_, length.out)

  # Loop over vector to get
  for (i1 in 1:length.out) {
    seed_stream_start <- parallel::nextRNGStream(seed_stream_start)
    stream[i1] <- paste0(seed_stream_start, collapse = "_")
  }

  return(stream)
}
