get_distance <- function(dt, target_list, dist = getOption("imabc.target_eval_distance")) {
  # CM NOTE: This is in case someone isn't use library or devtools::load_all() to load functions properly
  if (is.null(dist)) { dist <- "chisquare" }

  distance <- Reduce(`+`, lapply(attr(target_list, which = "target_ids"), FUN = function(x, dt, target_list, dist_opt) {
    sim <- dt[, x, with = FALSE]
    obs <- target_list$targets[gsub(paste0(target_list["target_group"], "_"), "", x)]

    if (obs == 0 | dist_opt == "stopping_range") {
      new_scale <- target_list$upper_bounds_stop[gsub(paste0(target_list["target_group"], "_"), "", x)] -
        target_list$lower_bounds_stop[gsub(paste0(target_list["target_group"], "_"), "", x)]
      new_scale <- new_scale*0.5

      return(((obs - sim)^2)/(new_scale^2))
    } else {

      return(((obs - sim)^2)/(obs^2))
    }
  }, dt = dt, target_list = target_list, dist_opt = dist))


  return(distance)
}
