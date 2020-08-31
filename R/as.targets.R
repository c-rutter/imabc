as.targets <- function(df,
                       group = "group", name = "name", target = "target",
                       lower_bounds_start = "lower_bounds_start", upper_bounds_start = "upper_bounds_start",
                       lower_bounds_stop = "lower_bounds_stop", upper_bounds_stop = "upper_bounds_stop") {


  targ <- do.call(define_targets,
                  # For each target group
                  lapply(split(df, df[group]), FUN = function(dta1) {
                    # group the subtargets
                    do.call(group_targets,
                            c(list(group_name = unique(dta1[[group]])), # group name (will only be one value for a given group)
                              # For each subtarget
                              lapply(split(dta1, dta1[name]), FUN = function(dta2) {
                                # add target information
                                add_target(
                                  target_name = dta2[[name]],
                                  target = dta2[[target]],
                                  starting_range = c(dta2[[lower_bounds_start]], dta2[[upper_bounds_start]]),
                                  stopping_range = c(dta2[[lower_bounds_stop]], dta2[[upper_bounds_stop]])
                                )
                              })
                            ))
                  })
  )

  return(targ)
}
