#'
#' @param df data.frame. Each parameter should be a row and each column is an input into add_prior. If a given column doesn't
#' relate to a parameter, set its value to NA.
#'
#' @export
#'
#' @examples
as.priors <- function(df, dist_base_name = NULL, density_fn = NULL, quantile_fn = NULL, parameter_name = NULL) {
  # Make sure at least one of dist_base_name, density_fn, or quantile_fn are found
  cols <- colnames(df)
  provided_names <- c(dist_base_name, density_fn, quantile_fn)
  if (!any(c(!is.null(provided_names)))) {
    stop(paste(
      "Must specify the name of a column that represents one of the following:", "distribution base name (dist_base_name),",
      "the density function (density_fn),", "or", "the quantile function (quantile_fn)."
    ))
  }
  if (!any(provided_names %in% cols)) {
    ins <- c("dist_base_name", "density_fn", "quantile_fn")[which(!c(is.null(dist_base_name), is.null(density_fn), is.null(quantile_fn)))]
    "%s = %s. Column %s not found in the df"
    stop(sprintf(
      "%s = %s. Column %s not found in the data.frame; ", ins, provided_names, provided_names
    ))
  }

  # Make sure the names we can control match the expected name for add_prior()
  if (!is.null(parameter_name)) {
    cols[cols == parameter_name] <- "parameter_name"
  }
  if (!is.null(dist_base_name)) {
    cols[cols == dist_base_name] <- "dist_base_name"
  }
  if (!is.null(density_fn)) {
    cols[cols == density_fn] <- "density_fn"
  }
  if (!is.null(quantile_fn)) {
    cols[cols == quantile_fn] <- "quantile_fn"
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
