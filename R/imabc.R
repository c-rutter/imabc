#########################################################################################################################
# Function Start ########################################################################################################
imabc <- function(
  priors,
  targets,
  target_fun,
  N_start = 1,
  seed = 1234,
  latinHypercube = TRUE,
  N_centers = 1,
  Center_n = 50,
  N_post = 100,
  max_iter = 1000,
  N_cov_points = 0,
  sample_inflate = 1,
  recalc_centers = TRUE,
  continue_runs = FALSE,
  verbose = TRUE
) {
  # Initial Print information
  run_timestamp <- base::date()
  if (verbose) { cat(run_timestamp, "\n") }
  if (verbose & !continue_runs) {
    cat(sep = "\n",
        sprintf("New ABC with specifications:"),
        sprintf("N_start = %s", N_start),
        sprintf("max_iter = %s", max_iter),
        sprintf("N_centers = %s", N_centers),
        sprintf("Center_n = %s", Center_n),
        sprintf("N_post = %s", N_post)
    )
  }
  # CM NOTE: Not worked on yet
  # if (verbose & continue_runs) {
  #     cat(sep = "\n",
  #         sprintf("Continuing ABC:"),
  #         sprintf("Previously stored at %s", location),
  #         sprintf("Last run on %s", last_run),
  #         sprintf("N_start = %s", N_start),
  #         sprintf("max_iter = %s", max_iter),
  #         sprintf("N_post = %s", N_post)
  #     )
  # }

  # Checks --------------------------------------------------------------------------------------------------------------
  # CM NOTE: Need to think about good checks (also good put some in add_* and define_* functions)

  # Environment setup ---------------------------------------------------------------------------------------------------
  # Randomization method (must be L'Ecuyer-CMRG for parallelization)
  rngKind <- "L'Ecuyer-CMRG"
  RNGkind(kind = rngKind, normal.kind = NULL)
  set.seed(seed)
  seed_stream_start <- .Random.seed

  # Miscellaneous Initializations
  # CM NOTE: Better names
  f_append <- FALSE
  ESS <- 0 # effective sample size
  n_store <- N_post + N_centers*(Center_n + 1)
  # initializations needed for new runs
  start_iter <- 1
  end_iter <- max_iter
  total_draws <- 0
  prevruns.dir <- NULL
  n_draw <- N_start # first iteration only, then set to n.center*Center_n
  n_rows_init <- max(n_draw, N_centers*Center_n) + recalc_centers*N_centers
  n_in <- 0
  n_use <- 0

  # Parameter Handling --------------------------------------------------------------------------------------------------
  # Determine number of paramters to calibrate
  all_parm_names <- names(priors)
  n_parms <- length(priors)
  calibrate_parms <- !attr(priors, "fixed")
  calibrate_parms <- names(calibrate_parms)[calibrate_parms]
  n_calib_parms <- length(calibrate_parms)

  # Priors Handling -----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  parm_draws <- init_run_dt(n = n_rows_init, parms = all_parm_names, type = "draw", out_final = FALSE)
  good_parm_draws <- init_run_dt(n_store, parms = all_parm_names, type = "draw", out_final = TRUE)
  parm_draws$seed <- seed_stream(seed_stream_start, n_rows_init)

  if (!continue_runs) {
    # Generate random inputs for prior distribution calculation
    if (latinHypercube) {
      u_draws <- lhs::randomLHS(N_start, n_parms)
    } else { # ! latinHypercube
      u_draws <- matrix(runif(N_start*n_parms), nrow = N_start, ncol = n_parms)
    } # latinHypercube
    colnames(u_draws) <- all_parm_names

    # Generate parameter space from prior distribution functions
    parm_draws <- parms_from_priors(
      parm_df = parm_draws,
      name_parms = all_parm_names,
      prior_list = priors,
      sampling = u_draws
    )
  } # !continue_runs

  # Targets Handling ----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  target_names <- names(targets)
  n_targets <- length(target_names)
  target_dist <- init_run_dt(n_rows_init, target_names, type = "distance", out_final = FALSE)
  good_target_dist <- init_run_dt(n_store, target_names, type = "distance", out_final = TRUE)
  # These are sub-targets within each of the main targets
  sim_parm_names <- attributes(targets)$sub_targets
  sim_parm <- init_run_dt(n_rows_init, sim_parm_names, type = "sim", out_final = FALSE)
  good_sim_parm <- init_run_dt(n_store, sim_parm_names, type = "sim", out_final = TRUE)

  # Miscellaneous Handling ----------------------------------------------------------------------------------------------
  if (N_cov_points == 0) { N_cov_points <- 25*n_parms }
  # Print information
  if (verbose) {
    bound_info <- lapply(targets[attr(targets, "update")], FUN = function(x) {
      sprintf("%s - %s", x$lower_bounds_start, x$upper_bounds_start)
    })
    bound_info <- paste(names(bound_info), unlist(bound_info), sep = ": ")
    cat("Current bound info:", bound_info, sep = "\n")
  }

  # Main Loop -----------------------------------------------------------------------------------------------------------
  for (i1 in start_iter:end_iter) {
    # if (i1 %% 20 == 0)
      print(paste("iter", i1))
    n_in_i <- 0 # CM NOTE: Better names
    # What targets are left to update based on stopping bounds
    update_targets <- attr(targets, "update")
    update_targets <- names(update_targets)[update_targets]

    # If not the first iteration on a continuing run
    if (!(continue_runs == TRUE & i1 == start_iter)) {
      # Print information
      if (verbose) {
        iter_info <- sprintf("Starting iteration %s at: %s", i1, Sys.time())
        n_info <- sprintf("Current n_in = %s", n_in)
        if (length(update_targets) > 0) {
          bound_info <- lapply(targets[attr(targets, "update")], FUN = function(x) {
            sprintf("%s - %s", x$lower_bounds_start, x$upper_bounds_start)
          })
          bound_info <- paste(names(bound_info), unlist(bound_info), sep = ": ")
          cat(iter_info, n_info, "Current bound info:", bound_info, sep = "\n")
        } else {
          cat(iter_info, "All tolerance intervals at target bounds")
        }
      }

      # Parms to check
      parms_to_run <- parm_draws[1:n_draw, c("seed", all_parm_names), with = FALSE]
      # Setup parallel handling
      registerDoParallel(cores = detectCores() - 1) # cluster auto-closed with foreach
      # User defined Distance Function applied on all simulated parms
      # CM NOTE: If expecting a list of objects (ll, sp) then need to change combine_results (see combine_results.R)
      res <- foreach(i1 = 1:nrow(parms_to_run), .combine = combine_results) %dopar% {
        inp <- as.numeric(parms_to_run[i1, all_parm_names, with = FALSE])
        sim_target <- target_fun(inp)
        # CM NOTE: res is supposed to be a measure of how all the subtargets did to match for a given main target
        #   e.g. Pickhardt has 4 sub targets that make up sim_target
        # list(ll = res, sp = sim_target) # CM NOTE: Better names

        return(sim_target) # CM NOTE: Better names
      }

      # Store results
      total_draws <- total_draws + n_draw
      sim_parm[1:n_draw, (sim_parm_names) := res]
      # Evaluate Targets
      target_dist[, (target_names) := eval_targets(sim_targets = sim_parm, target_list = targets, criteria = "start")]
      # CM NOTE: Should this be target_names or update_targets?
      target_dist$tot_dist <- total_distance(dt = target_dist, target_names = target_names, scale = FALSE)

      # Count the good points: points associated with positive distances
      target_dist$n_good[1:n_draw] <- rowSums(target_dist[1:n_draw, (target_names), with = FALSE] >= 0, na.rm = TRUE)
      n_in_i <- sum(target_dist$n_good[target_dist$step <= N_centers] == n_targets, na.rm = TRUE)

      # CM NOTE: Not worked on yet
      # write.table(
      #   parm_draws[1:n_draw, c("iter", "draw", "step", "seed", all_parm_names), with = FALSE],
      #   file = paste0(output.directory, "/", outfile.modelparms),
      #   sep = ",", append = f_append, col.names = !f_append, row.names = FALSE
      # )
      # write.table(
      #   target_dist[1:n_draw, c("iter", "draw", "step", target_names), with = FALSE],
      #   file=paste0(output.directory, "/", outfile.dist),
      #   sep = ",", append = f_append, col.names = !f_append, row.names = FALSE
      # )
      # write.table(
      #   sim_parm[1:n_draw, c("iter", "draw", "step", sim_parm_names), with = FALSE],
      #   file = paste0(output.directory, "/", outfile.simparms),
      #   sep = ",", append = f_append, col.names = !f_append, row.names = FALSE
      # )

      # Stop if there are no close points (and so cannot continue)
      # CM NOTE: Should this be a warning, a stop, or just a print?
      if (n_in + n_in_i == 0) {
        # Print information
        if (verbose) { print("No valid parameters to work from.") }
        break
      }

      # replace re-simulated targets for center draws in good.* matrices
      # and recalculate p-value and distance
      if (recalc_centers & i1 > 1) {
        # What were the centers from last time
        center_draw <- parm_draws$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)]

        # sort to ensure alignment across data tables
        # CM NOTE: This really shouldn't matter - I think this has to do with how values are being replace/updated
        #   a better way in data.table (and might speed up other calculations) is to use keys see:
        #   https://stackoverflow.com/questions/32371188/merge-and-replace-values-in-two-data-tables
        center_draw <- sort(center_draw, na.last = TRUE)
        setorder(good_parm_draws, iter, draw, step, na.last = TRUE)
        setorder(parm_draws, iter, draw, step, na.last = TRUE)
        setorder(good_sim_parm, iter, draw, step, na.last = TRUE)
        setorder(sim_parm, iter, draw, step, na.last = TRUE)
        setorder(good_target_dist, iter, draw, step, na.last = TRUE)
        setorder(target_dist, iter, draw, step, na.last = TRUE)

        # CM NOTE: A better way might be to merge/replace using data.table and keys:
        #   https://stackoverflow.com/questions/32371188/merge-and-replace-values-in-two-data-tables
        good_sim_parm[draw %in% center_draw, c("draw", sim_parm_names)] <-
          sim_parm[draw %in% center_draw, c("draw", sim_parm_names), with = FALSE]
        good_target_dist[draw %in% center_draw, c("draw", target_names)] <-
          target_dist[draw %in% center_draw, c("draw", target_names), with = FALSE]
        good_target_dist[draw %in% center_draw, ]$n_good <-
          rowSums(good_target_dist[draw %in% center_draw, (target_names), with = FALSE] >= 0, na.rm = TRUE)
        good_target_dist$n_good[is.na(good_target_dist$n_good)] <- 0L

        # Determine which center draws to keep and which to remove
        remove_draws <- good_target_dist[((draw %in% center_draw) & (n_good < n_targets)), ]$draw
        keep_draws <- good_target_dist[((draw %in% center_draw) & (n_good == n_targets)), ]$draw

        # Update good draw distance and stopping criteria
        if (length(keep_draws) > 0) {
          # if there are any centers that are kept, recalculate
          if (length(update_targets) == 0) {
            good_target_dist[draw %in% keep_draws, ]$tot_dist <- total_distance(
              dt = good_target_dist[draw %in% keep_draws, ],
              target_names = target_names,
              scale = FALSE
            )
          } else { # length(update_targets) == 0
            good_target_dist[draw %in% keep_draws, ]$tot_dist <- total_distance(
              dt = good_target_dist[draw %in% keep_draws, ],
              target_names = update_targets,
              scale = FALSE
            )
          } # ! length(update_targets) == 0
        } # length(keep_draws) > 0

        # Remove bad draws
        if (length(remove_draws) > 0) {
          # Print information
          if (verbose) {
            cat("Removing centers as good draws:", paste(remove_draws, collapse = ", "))
          }

          # Remove bad draws
          good_parm_draws[draw %in% remove_draws, names(good_parm_draws) := NA]
          good_sim_parm[draw %in% remove_draws, names(good_sim_parm) := NA]
          good_target_dist[draw %in% remove_draws, names(good_target_dist) := NA]

          # place removed draws at the bottom
          setorder(good_parm_draws, draw, iter, step, na.last = TRUE)
          setorder(good_sim_parm, draw, iter, step, na.last = TRUE)
          setorder(good_target_dist, draw, iter, step, na.last = TRUE)
        } # length(remove_draws) > 0

        # after replacing recalculated centers in good.* data tables,
        # remove them from the iteration-specific data tables
        parm_draws[step == (N_centers + 1), (all_parm_names) := NA_real_]
        sim_parm[step == (N_centers + 1), (sim_parm_names) := NA_real_]
        target_dist[step == (N_centers + 1), c(target_names, "tot_dist") := NA_real_]
        target_dist[step == (N_centers + 1), n_good := 0L]

        # Total draws being kept
        n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)
      } # if (recalc.centers & i1 > start_iter)

      # save good draws, distances, and simulated parms
      # CM NOTE: these seem like a round about way to add rows from parm_draws, etc. to the good.* versions
      #   could probably write more efficiently and clearly with which/subset and rbind while also ignoring or doing
      #   away with the draw variable
      # CM NOTE: What do we do if there are no n_in_i?
      if (n_in_i > 0) {
        if ((i1 == 1) & (n_in_i > n_store)) {
          # keep the n_store best draws (largest alpha level & smallest distance)
          # CM NOTE: Adding a sort on n_good to make sure the ones that are actually in bounds are considered first
          setorder(target_dist, -n_good, tot_dist, na.last = TRUE)
          add_draws <- target_dist$draw[1:n_store]
          setorder(target_dist, draw, na.last = TRUE)  # likely an unnecessary sort
          good_target_dist <- target_dist[draw %in% add_draws, ]
          good_parm_draws <- parm_draws[draw %in% add_draws, ]
          good_sim_parm <- sim_parm[draw %in% add_draws, ]
          add_row_range <- 1:n_store

        } else { # ! (i1 == 1) & (n_in_i > n_store)
          if ((n_in + n_in_i) > n_store) {
            # keep the best (n_store - n_in_i) draws (largest alpha level & smallest distance)
            # and add the current n_in_i runs to the bottom
            n_keep <- n_store - n_in_i
            # CM NOTE: Adding a sort on n_good to make sure the ones that are actually in bounds are considered first
            setorder(good_target_dist, -n_good, tot_dist, na.last = TRUE)
            keep_draws <- good_target_dist$draw[1:n_keep]
            setorder(good_target_dist, draw, na.last = TRUE)
            # CM NOTE: In the original code, good_target_dist does not have a thing here...
            #   this leads to discrepancies and errors when any draw %in% keep_draws were in rows greater than n_keep
            good_target_dist[1:n_keep, ] <- good_target_dist[draw %in% keep_draws, ]
            good_parm_draws[1:n_keep, ] <- good_parm_draws[draw %in% keep_draws, ]
            good_sim_parm [1:n_keep, ] <- good_sim_parm[draw %in% keep_draws, ]
            add_row_range <- (n_keep + 1):(n_store)

          } else { # ! (n_in + n_in_i) > n_store
            add_row_range <- (n_in + 1):(n_in + n_in_i)

          } # (n_in + n_in_i) > n_store
          # Get draw ID for rows to keep (good)
          add_draws <- target_dist$draw[target_dist$n_good == n_targets & target_dist$step < (N_centers + 1)]

          # Transfer good rows to results data.tables
          good_parm_draws[add_row_range, ] <- parm_draws[draw %in% add_draws, ]
          good_target_dist[add_row_range, ] <- target_dist[draw %in% add_draws, ]
          good_sim_parm[add_row_range, ] <- sim_parm[draw %in% add_draws, ]

        } # (i1 == 1) & (n_in_i > n_store)

        # CM NOTE: I think this taken care of by the fact that we do this above as well. May not need anymore
        if (length(update_targets) == 0) {
          good_target_dist[add_row_range, ]$tot_dist <- total_distance(
            dt = good_target_dist[add_row_range, ],
            target_names = target_names,
            scale = FALSE
          )
        } else {
          good_target_dist[add_row_range, ]$tot_dist <- total_distance(
            dt = good_target_dist[add_row_range, ],
            target_names = update_targets,
            scale = FALSE
          )
        }
      } # n_in_i > 0

      n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)
    } # !(continue_runs == TRUE & iter == start_iter)
    # Completed good rows
    # CM NOTE: Old method wouldn't work if good_target_dist was out of order which I think happens when there n_in_i == 0
    #   new method is marginally slower but will always find the right rows. We can switch back if we want but then we
    #   need to deal with n_in_i == 0 scenario
    # good_row_range <- 1:n_in
    good_row_range <- which(good_target_dist$n_good == n_targets)

    # If we have enough points, try to contrict bounds that we use
    if ((n_in >= 2*N_cov_points) & length(update_targets) > 0) {
      ##########################################################################
      # until target alpha-levels are reached, update alpha-levels every
      # 2*N.cov.points, reducing good points by half
      ###########################################################################
      # Sort targets based on overall distance of major targets still being updated
      # CM NOTE: This may be done already. May be able to remove
      good_target_dist$tot_dist <- total_distance(dt = good_target_dist, target_names = update_targets, scale = FALSE)

      # Ensure that we end up with at least N_cov_points
      keep_points <- c(trunc(seq(1.0, 2.0, 0.1)*N_cov_points), n_in)

      # Find least amount of points that move us towards the stopping bounds while letting us have enough for the more
      #   complex resampling method
      for (n_get in keep_points) {
        # Calculate a new alpha value
        if (n_get < n_in) {
          # Get the n_get best draws to determine an empirical set of new bounds
          get_draws <- good_target_dist[good_row_range, ][order(tot_dist, na.last = TRUE)][1:n_get]$draw

          # Get new bounds using n_get best draws
          targets[update_targets] <- get_new_bounds(
            targets_list = targets[update_targets],
            sims = good_sim_parm[draw %in% get_draws, ]
          )

        } else { # n_get < n_in
          # warning("No improvement")
          # return target bounds to original values
          targets[update_targets] <- update_target_bounds(targets[update_targets], from = "new", to = "start")

        } # ! n_get < n_in

        # Find which targets have moved closer to the stopping bounds
        improve <- unlist(lapply(targets[update_targets], FUN = function(x) {
          any(x$lower_bounds_new != x$lower_bounds_start | x$upper_bounds_new != x$upper_bounds_start)
        }))
        if (any(improve) | n_get == n_in) {
          # Update distances (values < 0 indicate out of tolerance bounds)
          good_target_dist[good_row_range, (update_targets) := eval_targets(
            sim_targets = good_sim_parm[good_row_range, ],
            target_list = targets[update_targets],
            criteria = "update"
          )]

          # update n_good
          good_target_dist$n_good <- 0L
          good_target_dist[good_row_range]$n_good <-
            rowSums(good_target_dist[good_row_range, (update_targets), with = FALSE] >= 0, na.rm = TRUE)
          n_in_new <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)

          # If we have enough points stop looking for more
          if (n_in_new >= N_cov_points) { break }
        } # any(improve) | n_get == n_in
      } # n_get in keep_points

      # If we tightened our bounds
      if (n_in_new >= N_cov_points & n_in_new < n_in) {
        # Update starting target bounds
        targets[update_targets] <- update_target_bounds(targets[update_targets], from = "start", to = "new")

        # Update n_in
        keep_draws <- good_target_dist[n_good == n_targets, ]$draw
        n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)

        # Determine which targets are at the most restrictive we want them to be
        attr(targets, "update") <- get_update_targets(targets)

        # CM NOTE: Commented out in ABCloopFunction.R as well. Should remove if not going to use.
        # if(N.in>N.cov.points &
        #    (sum(target.specs$alpha<target.specs$alpha.target)>=1)){
        #   # if more than N.cov.points that meet the updated alpha-criteria &
        #   # we are still updating tolerance intervals for some targets, then
        #   # keep only the closest N.cov.points among them, based on nearness
        #   # to targets still being updated. needed b/c SEER data starts @ alpha=0
        #
        #   good.target.dist[,tot.dist := NA]
        #   good.target.dist[draw %in% keep.draws,
        #                    tot.dist :=
        #                      get.updating.dist(x=good.target.dist[draw %in%
        #                                                           keep.draws,],
        #                                        update.names)]
        #
        #   setorder(good.target.dist,-n.good,-alpha,tot.dist,na.last=TRUE)
        #   keep.draws = good.target.dist[1:N.cov.points,]$draw
        #   setorder(good.target.dist,draw)
        #   N.in=N.cov.points
        #
        # }

        # CM NOTE: I am really confident we can find a better way of doing this
        # Move the best draws to top of good_target_dist and remove the rest
        good_row_range <- 1:n_in
        good_target_dist[good_row_range, ] <- good_target_dist[draw %in% keep_draws, ]
        good_parm_draws[good_row_range] <- good_parm_draws[draw %in% keep_draws]
        good_sim_parm[good_row_range] <- good_sim_parm[draw %in% keep_draws]
        if (n_in < n_store) {
          # clear unused rows in good.* data tables
          blank_rows <- (n_in + 1):n_store
          good_parm_draws[blank_rows, c("draw", "step", "iter") := NA_integer_]
          good_parm_draws[blank_rows, c(all_parm_names, "scaled_dist", "sample_wt") := NA_real_]
          good_parm_draws$seed[blank_rows] <- ""
          good_sim_parm[blank_rows, c("draw", "step", "iter") := NA_integer_]
          good_sim_parm[blank_rows, (sim_parm_names) := NA_real_]
          good_target_dist[blank_rows, c("draw", "step", "iter") := NA_integer_]
          good_target_dist[blank_rows, c(target_names, "tot_dist") := NA_real_]
          good_target_dist$n_good[blank_rows] <- NA_integer_
        } # n_in < n_store

        # Print information
        if (verbose) {
          n_info <- sprintf("New n_in = %s", n_in)
          bound_info <- lapply(targets[attr(targets, "update")], FUN = function(x) {
            sprintf("%s - %s", x$lower_bounds_start, x$upper_bounds_start)
          })
          bound_info <- paste(names(bound_info), unlist(bound_info), sep = ": ")
          cat("Updated bounds:", n_info, bound_info, sep = "\n")
        }
      } else { # n_in_new >= N_cov_points & n_in_new < n_in
        # Print information
        if (verbose) {
          n_info <- sprintf("New n_in = %s", n_in)
          bound_info <- lapply(targets[attr(targets, "update")], FUN = function(x) {
            sprintf("%s - %s", x$lower_bounds_start, x$upper_bounds_start)
          })
          bound_info <- paste(names(bound_info), unlist(bound_info), sep = ": ")
          cat("Unable to update bounds:", n_info, bound_info, sep = "\n")
        }
      } # ! n_in_new >= N_cov_points & n_in_new < n_in
    } # (n_in >= 2*N_cov_points) & length(update_targets) > 0

    # CM NOTE: Adding a condition that this can't be the first iteration. This is because mean_cov is not defined until
    #   the first iteration is done. Need to fix code to have it initialized or read in and used
    if ((n_in >= N_post | length(update_targets) == 0 | (i1 >= end_iter & n_in > 0)) & i1 > 1) {
      # CM NOTE: Original if statement for comparison
      # (N.in>=N.post | !any(target.specs$update.alpha)) | (iter>=end.iter & N.in>0)
      good_parm_draws$sample_wt <- 0
      in_draws <- good_target_dist[!is.na(draw), ]$draw
      # CM NOTE: Not worked on yet
      # CM NOTE: New method of handling the mixture file. Need to incorporate when old data is being used
      # if (continue_runs == TRUE & i1 == start_iter & start_iter > 1) {
      #   stop("continue_runs == TRUE & i1 == start_iter & start_iter > 1")
      #   # if continuing runs, at the first iter use only previous mixture distns
      #   m.file=paste0(prevruns.dir,"/",outfile.sampling)
      # } else if (continue_runs == TRUE & start_iter == 1) {
      #   stop("continue_runs == TRUE & start_iter == 1")
      #   m.file=paste0(output.directory,"/",outfile.sampling)
      # } else {
      #   m.file=paste0(c(prevruns.dir,output.directory),"/",outfile.sampling)
      # }
      if (!exists("mean_cov")) { stop("Missing mean_cov object") }
      good_parm_draws[draw %in% in_draws, ]$sample_wt <-
        get_weight(
          parms = good_parm_draws[draw %in% in_draws, ],
          parm_names = all_parm_names, # CM NOTE: calib.parm.names
          priors = priors,
          mixture_file = mean_cov,
          n = N_start
        )

      # calculate effective sample size using Kish formula. Here sum(sample.wt)=1
      ESS <- 1/sum(good_parm_draws[draw %in% in_draws, ]$sample_wt^2)

      # Print information
      if (verbose) {
        cat(sprintf("Effective Sample Size is %s", round(ESS, 2)))
      }
    } # n_in >= N_post | length(update_targets) == 0 | (i1 >= end_iter & n_in > 0)

    # Determine if there are enough points to quit
    if (ESS >= N_post & length(update_targets) == 0) {
      # Print information
      if (verbose) {
        n_info <- sprintf("Generated final in-range points, n_in = %s", n_in)
        ess_info <- sprintf("Effective Sample Size was %s", round(ESS, 2))
        target_info <- sprintf("(Target was %s)", N_post)
        cat(n_info, ess_info, target_info, sep = "\n")
      }
      break
    }

    # Simulate new draws
    if (i1 < end_iter) {
      ############################################################################
      # Find the n.center in range draws with model predictions closest to targets
      ############################################################################

      # check for high-weight points, defined as $\theta_i$ with
      # $w_i> 10/\sum_{i=1}^{N_{(t+1)}}$, meaning that the maximum weight is 10 times
      # greater than expected for a simple random sample from in-range points
      n_hiwt <- 0
      center_draw_hiwt <- NULL
      if (length(update_targets) == 0) {
        max_wt <- max(good_parm_draws[draw %in% in_draws, ]$sample_wt)
        if (max_wt >= 10/n_in) {
          draw_order <- setorder(good_parm_draws[good_row_range, ], -sample_wt, na.last = TRUE)$draw
          n_hiwt <- min(N_centers, length(draw_order))
          center_draw_hiwt <- draw_order[1:n_hiwt]

          # Print information
          if (verbose) {
            cat("Adding samples around high weight points", paste(center_draw_hiwt, collapse = ", "))
          }
        } # max.wt >= 10/N.in
      } # length(update_targets) == 0

      n_best_draw <- 0
      center_draw_best <- NULL
      if (n_hiwt < N_centers) {
        draw_order <- setorder(good_target_dist[good_row_range, ], tot_dist, na.last = TRUE)$draw
        setorder(good_target_dist, draw, na.last = TRUE)
        n_best_draw <- min(n_in, (N_centers - n_hiwt))
        center_draw_best <- draw_order[1:n_best_draw]
      } # n_hiwt < N_centers
      num_centers <- n_best_draw + n_hiwt
      center_draw <- c(center_draw_best, center_draw_hiwt)
      center_next <- as.matrix(good_parm_draws[draw %in% center_draw, all_parm_names, with = FALSE])

      ###########################################################################
      # Sample Center_n draws around these centers. Some draws may be out of range
      ###########################################################################
      # reset parm_draws, sim.parms, & target dist before simulating
      # draws for next iteration
      #--------------------------------------------------------------------------
      # Add additional rows
      n_draw <- num_centers*Center_n + recalc_centers*num_centers
      new_steps <- rep(1:num_centers, each = Center_n)
      if (recalc_centers) { new_steps <- c(new_steps, rep.int((N_centers + 1), num_centers)) }
      new_draws <- (total_draws + 1):(total_draws + n_draw)

      # Reset calculation information
      parm_draws$iter <- target_dist$iter <- sim_parm$iter <- i1 + 1
      parm_draws$draw <- target_dist$draw <- sim_parm$draw <- NA_integer_
      parm_draws$step <- target_dist$step <- sim_parm$step <- NA_integer_
      parm_draws$draw[1:n_draw] <- target_dist$draw[1:n_draw] <- sim_parm$draw[1:n_draw] <- new_draws
      parm_draws$step[1:n_draw] <- target_dist$step[1:n_draw] <- sim_parm$step[1:n_draw] <- as.integer(new_steps)
      parm_draws[, c(all_parm_names, "scaled_dist", "sample_wt") := NA_real_]
      parm_draws$seed <- ""
      seed_stream_start <- .Random.seed
      parm_draws$seed[1:n_draw] <- seed_stream(seed_stream_start, n_draw)
      # parm_draws[1:n.draw, seed := seed_stream(seed_stream_start, n_draw)]
      sim_parm[, (sim_parm_names) := NA_real_]
      target_dist[, c(target_names, "tot_dist") := NA_real_]
      target_dist$n_good <- 0L

      # simulate new draws
      #--------------------------------------------------------------------------

      # set any fixed parameter values (specified in parm.priors)
      # CM NOTE: Not worked on yet
      # if (length(fixed.parm.names) > 0) {
      #   names(fixed.parm.values)=fixed.parm.names
      #   set(parm.draws,j=fixed.parm.names,value=as.data.table(t(fixed.parm.values)))
      # } # length(fixed.parm.names) > 0

      # New draw sampling
      if (n_in < N_cov_points) {
        # if there are too few good points
        # Pull standard deviations of parameters provided with the prior information
        prior_sds <- attr(priors, "sds")
        sd_next <- matrix(0.5*prior_sds, ncol = n_parms, nrow = num_centers, byrow = TRUE)
        colnames(sd_next) <- names(priors)
        # Perform random draws
        parm_draws[1:(num_centers*Center_n), (all_parm_names) := draw_parms(
          n_add = Center_n,
          mu = center_next[, all_parm_names], # if not dealing with fixed parms else parm_names needs to be just fixed_parm_names
          sigma = sd_next,
          priors_list = priors,
          targets_list = targets
        )]

        # If recalculating centers
        if (recalc_centers) {
          # CM NOTE: order of center_draw and center_next are potentially not the same (search CENTER_ORDER_NOTE for extra
          #   info) the issue occurs because center_draw is sorted by tot_dist where center_next is sorted on draw. I
          #   am not sure it matters and I don't know if it affects anywhere else but I am fixing it here just in case
          center_draw <- sort(center_draw)
          parm_draws[parm_draws$step == (N_centers + 1), (all_parm_names)] <- as.data.table(center_next)
          parm_draws$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
          sim_parm$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
          target_dist$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
        }

        # Store results (for mean_cov)
        x <- parm_draws[1:n_draw, c("iter", "step"), with = FALSE]
        x$in_range <- get_in_range(
          parms = parm_draws[1:n_draw, all_parm_names, with = FALSE],
          priors = priors,
          parm_names = all_parm_names
        ) # CM NOTE: calib.parm.names
        setkey(x, iter, step)
        B_in <- x[step <= N_centers, list(B.in = sum(in_range, na.rm = TRUE)), by = .(iter, step)]
        if (exists("mean_cov")) {
          mean_cov <- rbind(
            mean_cov,
            get_mean_cov(
              iter = i1 + 1,
              mu = center_next[, all_parm_names],
              sd = sd_next,
              center = center_draw,
              B = B_in,
              parm_names = all_parm_names
            ) # CM NOTE: calib.parm.names
          )
        } else {
          mean_cov <- get_mean_cov(
            iter = i1 + 1,
            mu = center_next[, all_parm_names],
            sd = sd_next,
            center = center_draw,
            B = B_in,
            parm_names = all_parm_names
          ) # CM NOTE: calib.parm.names
        }
        # CM NOTE: Not worked on yet
        # write.table(sampling.output[,c("iter","step","center","B.in",
        #                                "parm",calib.parm.names),
        #                             with=FALSE],
        #             file=paste0(output.directory,"/",outfile.sampling),sep=",",
        #             append=f.append,
        #             col.names=!f.append,
        #             row.names=FALSE)
        # f.append=TRUE

      } else { # n_in < N_cov_points
        # sample MVN points around centers if there are enough points to
        # estimate the cov matrix
        #--------------------------------------------------------------------------
        n_use <- min(n_in, N_cov_points)
        sample_mean <- as.data.frame(center_next)

        # Given place in code this is really n_in == N_cov_points
        if (n_in <= N_cov_points) {
          var_data <- good_parm_draws[1:n_use, all_parm_names, with = FALSE] # CM NOTE: calib.parm.names
          sample_cov <- parm_covariance(var_data)
          # CM NOTE: Use to just be sample_cov == -1. sample_cov is usually a matrix though so unless this condition is
          #   met that will give a warning. adding all() will work when sample_cov is -1 and won't give a warning when it
          #   is a matrix
          # CM NOTE: Should this be an error?
          if (all(sample_cov == -1)) { return() }
          if (any(diag(sample_cov) == 0)) {
            # this occurs when adding a new parameter: it is set to default for all prior draws
            is_zero <- diag(sample_cov) == 0
            sd_next <- 0.5*attr(priors, "sds")
            sd_next[!is_zero] <- 0
            diag(sample_cov) <- diag(sample_cov) + sd_next^2
          }

        } # n_in <= N_cov_points

        for (center_i1 in 1:num_centers) {
          sample_mean_i1 <- unlist(sample_mean[center_i1, all_parm_names]) # if fixed vs calibrated parms are defined this needs to be calibrated parms
          draw_rows <- ((center_i1 - 1)*Center_n + 1):(center_i1*Center_n)

          if (n_in >= N_cov_points) {
            # use different var-cov matrices for each center
            # Find the n.use closest draws to each center point,
            #------------------------------------------------------------------------
            # Find scaled distance
            prior_sds <- attr(priors, "sds")
            good_parm_draws$scaled_dist <- Inf
            good_parm_draws[1:n_in, ]$scaled_dist <- total_distance(
              dt = good_parm_draws[1:n_in, ],
              target_names = all_parm_names,
              mu = sample_mean_i1,
              sd = prior_sds,
              scale = TRUE
            )
            # Find best points based on scaled distance
            setorder(good_parm_draws, scaled_dist, na.last = TRUE)
            var_data <- good_parm_draws[1:n_use, all_parm_names, with = FALSE] # calib.parm.names
            # Find sample covariance
            sample_cov <- parm_covariance(var_data)
            # CM NOTE: Should this be an error, what should we print out?
            # CM NOTE: This could also be all(sample_cov == -1)
            if (length(sample_cov) == 1 && sample_cov == -1) { return() }
            if (any(diag(sample_cov) == 0)) {
              # this occurs when adding a new parameter: it is set to default for all prior draws
              is_zero <- (diag(sample_cov) == 0)
              sd_next <- 0.5*prior_sds
              sd_next[!is_zero] <- 0
              diag(sample_cov) <- diag(sample_cov) + (sd_next^2)
            }
          } # n_in >= N_cov_points

          # Draw Center_n new parm values usign an MVN draw...............................
          # assign fixed parameters
          # parm.draws[draw.rows,(fixed.parm.names) := as.list(fixed.parm.values)]

          # simulate Center_n random draws of calibrated parameters
          if (abs(sum(sample_cov) - sum(diag(sample_cov))) < 1e-10) {
            warning("abs(sum(sample_cov) - sum(diag(sample_cov))) < 1e-10: This part of code should be tested")
            # CM NOTE: Not worked on yet
            # parm_draws[draw_rows, (all_parm_names) := draw.parms(n_add=Center_n,
            #                                                       mu=as.matrix(t(sample_mean_i1)),
            #                                                       sigma=as.matrix(t(diag(sample_cov))),
            #                                                       parm.priors=parm.priors.df,
            #                                                       parm.names=calib.parm.names,
            #                                                       calib.targets)] # CM NOTE: calib.parm.names
            # Perform random draws
            parm_draws[1:(num_centers*Center_n), (all_parm_names) := draw_parms(
              n_add = Center_n,
              mu = as.matrix(t(sample_mean_i1)),
              sigma = as.matrix(t(diag(sample_cov))),
              priors_list = priors,
              targets_list = targets
            )] # CM NOTE: calib.parm.names

          } else { # abs(sum(sample_cov) - sum(diag(sample_cov))) < 1e-10
            x <- get_B_draws(
              B = Center_n,
              inflate = sample_inflate,
              center = sample_mean_i1,
              cov_matrix = sample_cov,
              priors = priors,
              parm_names = all_parm_names
            )
            if (is.null(x)) {
              # CM NOTE: Should this be an error?
              print(paste("iteration=", i1, "center=", center_i1))
              return()
            }
            parm_draws[draw_rows, (all_parm_names) := x] # calib.parm.names

          } # ! abs(sum(sample_cov) - sum(diag(sample_cov))) < 1e-10

          # Store Mean Covariance results
          sample_mean_i1 <- setnames(as.data.frame(t(sample_mean_i1)), all_parm_names) # CM NOTE: calib.parm.names
          sample_cov <- setnames(as.data.frame(sample_cov), all_parm_names) # CM NOTE: calib.parm.names
          mean_cov_center_i1 <- data.table(rbind(sample_mean_i1, sample_cov))
          mean_cov_center_i1$iter <- i1 + 1
          mean_cov_center_i1$step <- center_i1
          mean_cov_center_i1$center <- center_draw[center_i1]
          mean_cov_center_i1$parm <- 0:(length(all_parm_names)) # CM NOTE: calib.parm.names
          B_in <- sum(get_in_range(
            parms = parm_draws[draw_rows, ],
            priors = priors,
            parm_names = all_parm_names
          )) # CM NOTE: calib.parm.names
          mean_cov_center_i1$B.in <- B_in
          setcolorder(mean_cov_center_i1, c("iter", "step", "center", "B.in" , "parm", all_parm_names)) # CM NOTE: calib.parm.names

          if (exists("mean_cov")) {
            mean_cov <- rbind(mean_cov, mean_cov_center_i1)
          } else {
            mean_cov <- mean_cov_center_i1
          }
          # CM NOTE: Not worked on yet
          # if(B_in==0) print(paste("*** warning: B.in=",B_in,
          #                         "for iter=",iter+1,"and center=",center_i1))
          # write.table(sampling.output[,c("iter","step","center","B.in",
          #                                "parm",calib.parm.names),
          #                             with=FALSE],
          #             file=paste0(output.directory,"/",outfile.sampling),sep=",",
          #             append=f.append,
          #             col.names=!f.append,
          #             row.names=FALSE)
          # f.append=TRUE

        } # center_i1 in 1:num_centers

        if (recalc_centers) {
          parm_draws[step == (N_centers + 1), (all_parm_names)] <- as.data.table(center_next)
          parm_draws[step == (N_centers + 1), ]$draw <- center_draw
          sim_parm[step == (N_centers + 1), ]$draw <- center_draw
          target_dist[step == (N_centers + 1), ]$draw <- center_draw
        }

        # put good.* data tables in draw order
        # (though all references are by draw %in% avoid potential problems)
        setorder(good_parm_draws, draw, na.last = TRUE)
        setorder(good_sim_parm, draw, na.last = TRUE)
        setorder(good_target_dist, draw, na.last = TRUE)

      } # ! n_in < N_cov_points
      # if (continue_runs == TRUE & i1 == start_iter) { f.append <- FALSE }
    } # if (i1 < end_iter)
  } # i1 in start_iter:end_iter

  # Print information
  run_timestamp <- base::date()
  if (verbose) {
    time_info <- sprintf("Finishing ABC at %s", run_timestamp)
    seed_info <- sprintf("seed value is: %s", paste0(.Random.seed, collapse = ", "))
    n_info <- sprintf("number of in range draws was &s", n_in)
    bound_info <- lapply(targets[attr(targets, "update")], FUN = function(x) {
      sprintf("%s - %s", x$lower_bounds_start, x$upper_bounds_start)
    })
    bound_info <- paste(names(bound_info), unlist(bound_info), sep = ": ")
    cat(time_info, seed_info, n_info, "Final bounds info:", bound_info, sep = "\n")
  }

  # Store results
  good_parm_draws <- good_parm_draws[!is.na(draw), ]
  good_parm_draws[, scaled_dist := NULL]  # scaled.dist is used for cov calcs only
  # setorder(good.parm.draws,draw,na.last=TRUE)
  # write.table(good.parm.draws,
  #             file=paste0(output.directory,"/good.",outfile.modelparms),
  #             sep=",",append=FALSE,col.names=TRUE,row.names=FALSE)

  good_sim_parm <- good_sim_parm[!is.na(draw), ]
  # setorder(good.sim.parm,draw,na.last=TRUE)
  # write.table(good.sim.parm,
  #             file=paste0(output.directory,"/good.",outfile.simparms),
  #             sep=",",append=FALSE,col.names=TRUE,row.names=FALSE)

  good_target_dist <- good_target_dist[!is.na(draw), ]
  good_target_dist[, (target_names) := lapply(.SD ,"abs"), .SDcols = target_names]
  # setorder(good.target.dist,draw,na.last=TRUE)
  # write.table(good.target.dist[,c("iter","draw","step",dist.names),with=FALSE],
  #             file=paste0(output.directory,"/good.",outfile.dist),
  #             sep=",",append=FALSE,col.names=TRUE,row.names=FALSE)

  return(list(
    parm_draws = good_parm_draws,
    sim_parm = good_sim_parm,
    target_dist = good_target_dist
  ))
}

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
