#' Title
#'
#' @param parm_df
#' @param name_parms
#' @param prior_list
#' @param sampling
#'
#' @return
#' @export
parms_from_priors <- function(parm_df, name_parms, prior_list, sampling) {
  if (is.null(sampling)) {
    parm_df[1:nrow(parm_df), (name_parms) := lapply(name_parms, FUN = function(x, prior_funcs, n) {
      prior_funcs[[x]]$quantile_function(n)

    }, prior_funcs = prior_list, n = nrow(parm_df))]
  } else {
    parm_df[1:nrow(sampling), (name_parms) := lapply(name_parms, FUN = function(x, prior_funcs, df) {
      prior_funcs[[x]]$quantile_function(df[, which(name_parms %in% x)])

    }, prior_funcs = prior_list, df = sampling)]
  }

  return(parm_df)
}
