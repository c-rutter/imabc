get_distance <- function(dt, target_list) {

  distance <- Reduce(`+`, lapply(attr(target_list, which = "target_ids"), FUN = function(x, dt, target_list) {
    sim <- dt[, x, with = FALSE]
    obs <- target_list$targets[gsub(paste0(target_list["target_group"], "_"), "", x)]

    if (obs == 0) {
      new_scale <- target_list$upper_bounds_stop[gsub(paste0(target_list["target_group"], "_"), "", x)] -
        target_list$upper_bounds_start[gsub(paste0(target_list["target_group"], "_"), "", x)]
      new_scale <- new_scale*0.5

      return(((sim)^2)/(new_scale^2))
    } else {

      return(((obs - sim)^2)/(obs^2))
    }
  }, dt = dt, target_list = target_list))


  return(distance)
}
