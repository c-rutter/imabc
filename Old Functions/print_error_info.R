#' @export
print_error_info <- function(e, parms){
  print(paste0(e,paste0(parms, collapse = ",")), collapse = ": ")
}
