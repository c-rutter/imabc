#' Title
#'
#' @param parm_df
#' @param name_parms
#' @param prior_list
#' @param sampling
#'
#' @return
parms_from_priors <- function(parm_df, name_parms, prior_list, sampling) {

  parm_df[1:nrow(sampling), (name_parms) := lapply(name_parms, FUN = function(x, prior_funcs, df) {
    if (attributes(prior_funcs)$fixed[x]) {
      prior_funcs[[x]](nrow(df))
    } else {
      prior_funcs[[x]](df[, which(name_parms %in% x)])
    }
  }, prior_funcs = prior_list, df = sampling)]

  return(parm_df)
}
