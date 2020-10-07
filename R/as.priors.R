#' @export
as.priors <- function(df, name_var = NULL, dist_var = NULL, density_var = NULL, quantile_var = NULL) {
  # Make sure at least one of dist_var, density_var, or quantile_var are found
  cols <- colnames(df)
  provided_names <- c(dist_var, density_var, quantile_var)
  if (!any(c(!is.null(provided_names)))) {
    stop(paste(
      "Must specify the name of a column that represents one of the following:", "distribution base name (dist_var),",
      "the density function (density_var),", "or", "the quantile function (quantile_var)."
    ))
  }
  if (!any(provided_names %in% cols)) {
    ins <- c("dist_var", "density_var", "quantile_var")[which(!c(is.null(dist_var), is.null(density_var), is.null(quantile_var)))]
    "%s = %s. Column %s not found in the df"
    stop(sprintf(
      "%s = %s. Column %s not found in the data.frame; ", ins, provided_names, provided_names
    ))
  }

  # Make sure the names we can control match the expected name for add_prior()
  if (!is.null(name_var)) {
    cols[cols == name_var] <- "parameter_name"
  }
  if (!is.null(dist_var)) {
    cols[cols == dist_var] <- "dist_base_name"
  }
  if (!is.null(density_var)) {
    cols[cols == density_var] <- "density_fn"
  }
  if (!is.null(quantile_var)) {
    cols[cols == quantile_var] <- "quantile_fn"
  }
  colnames(df) <- cols

  # For each parameter, create a prior object
  prio_list <- lapply(1:nrow(df), FUN = function(i1, dta) {
    tmp <- dta[i1, , drop = TRUE]
    tmp <- tmp[!is.na(tmp)]

    do.call(add_prior, tmp)
  }, dta = df)

  # Take all prior objects and add them into a final imabc priors object
  final_prio_list <- do.call(define_priors, prio_list)

  return(final_prio_list)
}
