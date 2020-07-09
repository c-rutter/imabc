get_in_range <- function(parms, priors, parm_names) {

  # All parms must be in range based on their prior information
  in_range <- Reduce(`&`, lapply(parm_names, FUN = function(y, parms, priors) {
    parms[[y]] >= attr(priors, which = "mins")[y] &
      parms[[y]] <= attr(priors, which = "maxs")[y]
  }, parms = parms, priors = priors))
  in_range <- as.numeric(in_range)
  in_range[is.na(in_range)] <- 0

  return(in_range)
}
