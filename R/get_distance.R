get_distance <- function(dt, target_list) {

  distance <- Reduce(`+`, lapply(attr(target_list, which = "target_ids"), FUN = function(x, dt, target_list) {
    sim <- dt[, x, with = FALSE]
    obs <- target_list$targets[gsub(paste0(target_list["target_group"], "_"), "", x)]

    return(((obs - sim)^2)/(obs^2))
  }, dt = dt, target_list = target_list))


  return(distance)
}


