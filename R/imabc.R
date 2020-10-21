#' imabc
#'
#' @param priors list. A list with all the information required by define_priors and add_priors
#' @param targets list. A list of the main targets and their subtargets
#' @param target_fun function. A function that takes in parameters and returns the predicted subtarget values
#' @param previous_results list. optional.
#' @param N_start
#' @param seed
#' @param latinHypercube Only needed if previous_results not included
#' @param N_centers
#' @param Center_n
#' @param N_post
#' @param max_iter
#' @param N_cov_points
#' @param sample_inflate
#' @param recalc_centers
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
imabc <- function(
  priors, # Always needed
  targets, # Always needed
  target_fun, # Always needed
  previous_results = NULL,
  N_start = 1,
  seed = 12345,
  latinHypercube = TRUE,
  N_centers = 1,
  Center_n = 50,
  N_post = 100,
  max_iter = 1000,
  N_cov_points = 0,
  sample_inflate = 1,
  recalc_centers = TRUE, # Remove as option
  backend_fun = NULL,
  output_directory = NULL,
  output_tag = "timestamp",
  verbose = TRUE
) {
  # Checks -------------------------------------------------------------------------------------------------------------
  # CM NOTE: Need to think about good checks (also good put some in add_* and define_* functions)
  testing <- F
  stopifnot(!testing)

  # Continuing run
  continue_runs <- !is.null(previous_results)

  # Initial Print information
  run_timestamp <- Sys.time()
  print_time_fmt <- "%a %b %d %X %Y"
  write_time_fmt <- "%Y%m%d_%H%M%Z"
  if (verbose) { cat(format(run_timestamp, print_time_fmt), "\n") }
  if (verbose & !continue_runs) {
    cat(sep = "\n",
        sprintf("New IMABC with specifications:"),
        sprintf("  N_start = %s", N_start),
        sprintf(" max_iter = %s", max_iter),
        sprintf("N_centers = %s", N_centers),
        sprintf(" Center_n = %s", Center_n),
        sprintf("   N_post = %s", N_post)
    )
  }
  if (verbose & continue_runs) {
      cat(sep = "\n",
          sprintf("Continuing IMABC with specifications:"),
          sprintf(" N_start = %s", N_centers*Center_n),
          sprintf("max_iter = %s", max_iter),
          sprintf("  N_post = %s", N_post)
      )
  }

  # Environment setup ---------------------------------------------------------------------------------------------------
  # Create output directory if it doesn't already exist
  dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

  # Randomization method (must be L'Ecuyer-CMRG for parallelization)
  rngKind <- "L'Ecuyer-CMRG"
  RNGkind(kind = rngKind, normal.kind = NULL)
  set.seed(seed)
  seed_stream_start <- .Random.seed

  # Miscellaneous Initializations
  # CM NOTE: Better names
  output_tag <- ifelse(output_tag == "timestamp", format(run_timestamp, write_time_fmt), output_tag)
  parm_df_outfile <- sprintf("Good_SimulatedParameters_%s.csv", output_tag)
  sims_df_outfile <- sprintf("Good_SimulatedTargets_%s.csv", output_tag)
  targ_df_outfile <- sprintf("Good_SimulatedDistances_%s.csv", output_tag)
  meancov_outfile <- sprintf("MeanCovariance_%s.csv", output_tag)
  run_meta_df_outfile <- sprintf("RunMetadata_%s.csv", output_tag)

  # CM NOTE: In original function there were very specific ways of handling certain cases (like N_centers = 0) when a
  #   previous set of results was supplied. I'm holding off on that for now since that information can just as easily be
  #   supplied by the user. I'm only handling start_iter, total_draws and n_draw differently because it will make accounting
  #   easier (in case of start_iter and total_draws) or save memory (in case of n_draw)
  # CM NOTE: f_append - Since we save new results to a new timestamp this should always start FALSE but do we want the ability to
  #   append to an old file instead as well?
  f_append <- FALSE
  ESS <- 0 # Effective Sample Size
  n_store <- N_post + N_centers*(Center_n + 1)
  # Initializations needed for new runs
  start_iter <- ifelse(
    continue_runs,
    as.numeric(previous_results$prev_run_meta$VALUE[previous_results$prev_run_meta$INFO == "current_iteration"]) + 1, 1
  )
  end_iter <- (start_iter - 1) + max_iter
  total_draws <- ifelse(
    continue_runs,
    as.numeric(previous_results$prev_run_meta$VALUE[previous_results$prev_run_meta$INFO == "last_draw"]) + 1, 0
  )
  n_draw <- ifelse(continue_runs, N_centers*Center_n, N_start)
  n_rows_init <- max(n_draw, N_centers*Center_n) + recalc_centers*N_centers
  n_in <- 0
  n_use <- 0
  # CM NOTE: Name? I think group_names, n_groups, distance_dt, good_distance_dt
  target_names <- unique(attr(targets, "target_groups"))
  n_targets <- length(target_names)
  have_N_cov_points <- FALSE # For testing/checking everything runs ok

  # Parameter Handling --------------------------------------------------------------------------------------------------
  # Determine number of paramters to calibrate
  all_parm_names <- names(priors)
  n_parms <- length(priors)
  # calibrate_parms <- !attr(priors, "fixed") # CM NOTE: Now we tract distribution names
  # calibrate_parms <- names(calibrate_parms)[calibrate_parms]
  # n_calib_parms <- length(calibrate_parms)

  # Priors Handling -----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  parm_draws <- init_run_dt(n = n_rows_init, parms = all_parm_names, type = "draw", out_final = FALSE)
  # CM NOTE: I think a better idea might be split init_run_dt into two functions - one for good_* and one for *.
  #   This is really just splitting based on out_final = FALSE vs out_final = TRUE.
  #   init_run_dt would initialize intermediate tables
  #   new function would intialize good_* tables. It would also handle previous_results (and thus the continue runs)
  #   this is somewhat dependent on how we plan to handle the more complex versions of a restart
  #   for now just use an if () else
  good_parm_draws <- if (continue_runs) {
    # Number of good results from the last set of runs
    n_in <- sum(previous_results$good_target_dist$n_good == n_targets)

    # Need to add rows to make sure we have enough for storing
    if (n_in < n_store) {
      # add blank rows to the bottom of the good.* data tables for additional stored points
      add_blank_rows <- n_store - n_in
      blank_rows <- init_run_dt(n = add_blank_rows, parms = all_parm_names, type = "draw", out_final = TRUE)
      data.table(rbind(previous_results$good_parm_draws, blank_rows))
    } else {
      data.table(previous_results$good_parm_draws)
    }
  } else {
    init_run_dt(n_store, parms = all_parm_names, type = "draw", out_final = TRUE)
  }
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
    # Get empirical standard deviation of parameters
    priors <- update_parm_sds(priors, dt = parm_draws, parms = all_parm_names)
  } # !continue_runs

  # Targets Handling ----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  target_dist <- init_run_dt(n = n_rows_init, parms = target_names, type = "distance", out_final = FALSE)
  # CM NOTE: See continue_runs for good_parm_draws
  good_target_dist <- if (continue_runs) {
    # Need to add rows to make sure we have enough for storing
    # n_in calculated at initialization of good_target_dist
    if (n_in < n_store) {
      # add blank rows to the bottom of the good.* data tables for additional stored points
      add_blank_rows <- n_store - n_in
      blank_rows <- init_run_dt(n = add_blank_rows, parms = target_names, type = "distance", out_final = TRUE)
      data.table(rbind(previous_results$good_target_dist, blank_rows))
    } else {
      data.table(previous_results$good_target_dist)
    }
  } else {
    init_run_dt(n_store, target_names, type = "distance", out_final = TRUE)
  }
  # These are sub-targets within each of the main targets
  sim_parm_names <- attributes(targets)$target_names
  sim_parm <- init_run_dt(n_rows_init, sim_parm_names, type = "sim", out_final = FALSE)
  # CM NOTE: See continue_runs for good_parm_draws
  good_sim_parm <- if (continue_runs) {
    # Need to add rows to make sure we have enough for storing
    # n_in calculated at initialization of good_target_dist
    if (n_in < n_store) {
      # add blank rows to the bottom of the good.* data tables for additional stored points
      add_blank_rows <- n_store - n_in
      blank_rows <- init_run_dt(n = add_blank_rows, parms = sim_parm_names, type = "sim", out_final = TRUE)
      data.table(rbind(previous_results$good_sim_parm, blank_rows))
    } else {
      data.table(previous_results$good_sim_parm)
    }
  } else {
    init_run_dt(n_store, sim_parm_names, type = "sim", out_final = TRUE)
  }

  # Miscellaneous Handling ----------------------------------------------------------------------------------------------
  if (N_cov_points == 0) { N_cov_points <- 25*n_parms }
  # Print information
  if (verbose) {
    bound_info <- lapply(attr(targets, "target_names"), FUN = function(x, targ) {
      tmp <- targ[x]
      paste(sprintf("%s: %s - %s", x, tmp$current_lower_bounds, tmp$current_upper_bounds), collapse = "\n")
    }, targ = targets)
    bound_info <- lapply(unique(attr(targets, "target_groups")), FUN = function(x, groups, bound_info) {
      paste(unlist(bound_info[x == groups]), collapse = "\n")
    }, groups = attr(targets, "target_groups"), bound_info = bound_info)
    bound_info <- paste(sprintf("---- %s ----", unique(attr(targets, "target_groups"))), bound_info, sep = "\n", collapse = "\n")
    cat("Current bound info:", bound_info, sep = "\n")
  }

  # Main Loop -----------------------------------------------------------------------------------------------------------
  for (main_i1 in start_iter:end_iter) {
    if (testing) {
      if (!exists("main_i1")) {
        main_i1 <- start_iter - 1
      }
      main_i1 <- main_i1 + 1
      warning(sprintf("Testing. main_i1 artificially set to %s", main_i1))
    }
    n_in_i <- 0 # CM NOTE: Better names
    # What targets are left to update based on stopping bounds
    update_targets <- unique(attr(targets, "update"))

    # Print information
    if (verbose) {
      header <- sprintf("\n---------- Start Iter %s ----------", main_i1)
      iter_info <- sprintf("Starting at: %s", Sys.time())
      n_info <- sprintf("Current n_in = %s", n_in)
      if (length(update_targets) > 0) {
        # CM NOTE: Need to try tapply
        bounds_to_print <- names(attr(targets, "update"))
        bound_info <- lapply(bounds_to_print, FUN = function(x, targ) {
          tmp <- targ[x]
          paste(sprintf("%s: %s - %s", x, tmp$current_lower_bounds, tmp$current_upper_bounds), collapse = "\n")
        }, targ = targets)
        bound_info <- lapply(unique(attr(targets, "target_groups")), FUN = function(x, groups, bound_info) {
          paste(unlist(bound_info[x == groups]), collapse = "\n")
        }, groups = attr(targets, "target_groups"), bound_info = bound_info)
        bound_info <- paste(sprintf("---- %s ----", unique(attr(targets, "target_groups"))), bound_info, sep = "\n", collapse = "\n")
        cat(header, iter_info, n_info, "Current bound info:", bound_info, sep = "\n")
      } else {
        cat(header, iter_info, "All tolerance intervals at target bounds", sep = "\n")
      }
    }

    # If not the first iteration on a continuing run
    if (!(continue_runs == TRUE & main_i1 == start_iter)) {
      # Parms to check
      parms_to_run <- parm_draws[1:n_draw, c("seed", all_parm_names), with = FALSE]
      # User defined Distance Function applied on all simulated parms
      res <- run_handler(
        parms_to_run = parms_to_run, target_fun = target_fun, all_parm_names = all_parm_names,
        targets = targets, priors = priors, custom_function = backend_fun
      )
      # Ensure order of columns matches order of sim_parm if names exist
      if (all(names(res) %in% colnames(sim_parm))) {
        res <- res[, match(sim_parm_names, colnames(res))]
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

      # Stop if there are no close points (and so cannot continue)
      stopifnot("No valid parameters to work from." = n_in + n_in_i > 0)

      # replace re-simulated targets for center draws in good.* matrices
      # and recalculate p-value and distance
      if (recalc_centers & main_i1 > 1) {
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
            cat("Removing centers as good draws:", paste(remove_draws, collapse = ", "), "\n")
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
      } # if (recalc.centers & main_i1 > start_iter)

      # Save good draws, distances, and simulated parms
      if (n_in_i > 0) {
        if ((main_i1 == 1) & (n_in_i > n_store)) {
          # keep the n_store best draws (smallest distance and all targets number of targets in range)
          setorder(target_dist, -n_good, tot_dist, na.last = TRUE)
          add_draws <- target_dist$draw[1:n_store]
          setorder(target_dist, draw, na.last = TRUE)
          good_target_dist <- target_dist[draw %in% add_draws, ]
          good_parm_draws <- parm_draws[draw %in% add_draws, ]
          good_sim_parm <- sim_parm[draw %in% add_draws, ]
          add_row_range <- 1:n_store

        } else { # ! (main_i1 == 1) & (n_in_i > n_store)
          if ((n_in + n_in_i) > n_store) {
            # keep the best (n_store - n_in_i) draws (largest alpha level & smallest distance)
            # and add the current n_in_i runs to the bottom
            n_keep <- n_store - n_in_i
            setorder(good_target_dist, -n_good, tot_dist, na.last = TRUE)
            keep_draws <- good_target_dist$draw[1:n_keep]
            setorder(good_target_dist, draw, na.last = TRUE)
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

        } # (main_i1 == 1) & (n_in_i > n_store)

        # Recalculate distances for only the target groups being updated
        # CM NOTE: I believe "if length(update_targets) == 0" is redundent as total_distance should be for everything
        #   already. Alternatively we can move this outside of !(continue_runs == TRUE & iter == start_iter) as suggested
        #   in the next total_distance calc
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
    good_row_range <- which(good_target_dist$n_good == n_targets)

    # If we have enough points, try to contrict bounds that we use
    if ((n_in >= 2*N_cov_points) & length(update_targets) > 0) {
      ##########################################################################
      # until target alpha-levels are reached, update alpha-levels every
      # 2*N.cov.points, reducing good points by half
      ###########################################################################
      # Sort targets based on overall distance of major targets still being updated
      # CM NOTE: This is redundent if !(continue_runs == TRUE & iter == start_iter). Can move to an else statement or
      #   move last total_distance to outside of that if statement
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
          targets <- get_new_bounds(
            to_update = update_targets,
            targets_list = targets,
            sims = good_sim_parm[draw %in% get_draws, ]
          )

        } else { # n_get < n_in
          # return target bounds to original values
          targets <- update_target_bounds(targets, from = "new", to = "current")

        } # ! n_get < n_in

        # Find which targets have moved closer to the stopping bounds
        improve <- any(
          targets$new_lower_bounds != targets$current_lower_bounds |
            targets$new_upper_bounds != targets$current_upper_bounds
          )
        if (improve | n_get == n_in) {
          # Update distances (values < 0 indicate out of tolerance bounds)
          good_target_dist[good_row_range, (update_targets) := eval_targets(
            sim_targets = good_sim_parm[good_row_range, ],
            target_list = targets[update_targets, groups_given = TRUE],
            criteria = "update"
          )]

          # update n_good
          good_target_dist$n_good <- 0L
          good_target_dist[good_row_range]$n_good <-
            rowSums(good_target_dist[good_row_range, (target_names), with = FALSE] >= 0, na.rm = TRUE)
          n_in_new <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)

          # If we have enough points stop looking for more
          if (n_in_new >= N_cov_points) { break }
        } # any(improve) | n_get == n_in
      } # n_get in keep_points

      # If we tightened our bounds
      if (n_in_new >= N_cov_points & n_in_new < n_in) {
        # Update starting target bounds
        targets <- update_target_bounds(targets, from = "current", to = "new")

        # Update n_in
        keep_draws <- good_target_dist[n_good == n_targets, ]$draw
        n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)

        # Determine which targets are at the most restrictive we want them to be
        attr(targets, "update") <- get_update_targets(targets)

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

          bounds_to_print <- names(attr(targets, "update"))
          bound_info <- lapply(bounds_to_print, FUN = function(x, targ) {
            tmp <- targ[x]
            paste(sprintf("%s: %s - %s", x, tmp$current_lower_bounds, tmp$current_upper_bounds), collapse = "\n")
          }, targ = targets)
          bound_info <- lapply(unique(attr(targets, "target_groups")), FUN = function(x, groups, bound_info) {
            paste(unlist(bound_info[x == groups]), collapse = "\n")
          }, groups = attr(targets, "target_groups"), bound_info = bound_info)
          bound_info <- paste(sprintf("---- %s ----", unique(attr(targets, "target_groups"))), bound_info, sep = "\n", collapse = "\n")
          bound_info <- ifelse(bound_info == "", "All tolerance intervals at target bounds", bound_info)
          cat("Updated bounds:", n_info, bound_info, sep = "\n")
        }
      } else { # n_in_new >= N_cov_points & n_in_new < n_in
        # Print information
        if (verbose) {
          n_info <- sprintf("New n_in = %s", n_in)
          bounds_to_print <- names(attr(targets, "update"))
          bound_info <- lapply(bounds_to_print, FUN = function(x, targ) {
            tmp <- targ[x]
            paste(sprintf("%s: %s - %s", x, tmp$current_lower_bounds, tmp$current_upper_bounds), collapse = "\n")
          }, targ = targets)
          bound_info <- lapply(unique(attr(targets, "target_groups")), FUN = function(x, groups, bound_info) {
            paste(unlist(bound_info[x == groups]), collapse = "\n")
          }, groups = attr(targets, "target_groups"), bound_info = bound_info)
          bound_info <- paste(sprintf("---- %s ----", unique(attr(targets, "target_groups"))), bound_info, sep = "\n", collapse = "\n")
          cat("Unable to update bounds:", n_info, bound_info, sep = "\n")
        }
      } # ! n_in_new >= N_cov_points & n_in_new < n_in
    } # (n_in >= 2*N_cov_points) & length(update_targets) > 0

    # Calculate Effective Sample Size if we have enough sample, met the target stopping ranges or on the last iteration
    if ((n_in >= N_post | length(update_targets) == 0 | (main_i1 >= end_iter & n_in > 0)) & main_i1 > 1) {
      good_parm_draws$sample_wt <- 0
      in_draws <- good_target_dist[!is.na(draw), ]$draw
      if (continue_runs == TRUE & main_i1 == start_iter) {
        mean_cov <- data.table(previous_results$mean_cov)
      }
      stopifnot(
        "Missing mean_cov object" =
          exists("mean_cov")
      )
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
        cat(sprintf("Effective Sample Size is %s", round(ESS, 2)), "\n")
      }
    } # n_in >= N_post | length(update_targets) == 0 | (main_i1 >= end_iter & n_in > 0)

    # Determine if there are enough points to quit
    if (ESS >= N_post & length(update_targets) == 0) {
      # Print information
      if (verbose) {
        n_info <- sprintf("Generated final in-range points, n_in = %s", n_in)
        ess_info <- sprintf("Effective Sample Size was %s", round(ESS, 2))
        target_info <- sprintf("(Target was %s)", N_post)
        cat(n_info, ess_info, target_info, sep = "\n")
      }

      # Have met the appropriate sample size. Stop looking and return final results
      break
    }

    # Simulate new draws
    if (main_i1 < end_iter) {
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
            cat("Adding samples around high weight points", paste(center_draw_hiwt, collapse = ", "), "\n")
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

      # new_draws <- (total_draws + 1):(total_draws + n_draw)


      # Reset calculation information
      parm_draws$iter <- target_dist$iter <- sim_parm$iter <- main_i1 + 1
      parm_draws$draw <- target_dist$draw <- sim_parm$draw <- NA_integer_
      parm_draws$step <- target_dist$step <- sim_parm$step <- NA_integer_

      # parm_draws$draw[1:n_draw] <- target_dist$draw[1:n_draw] <- sim_parm$draw[1:n_draw] <- new_draws

      parm_draws$step[1:n_draw] <- target_dist$step[1:n_draw] <- sim_parm$step[1:n_draw] <- as.integer(new_steps)
      parm_draws[, c(all_parm_names, "scaled_dist", "sample_wt") := NA_real_]
      parm_draws$seed <- ""

      # parm_draws$seed[1:n_draw] <- seed_stream(seed_stream_start, n_draw)

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
          compare_list = priors,
          check_dt = parm_draws[1:n_draw, all_parm_names, with = FALSE],
          out = "logical"
        ) # CM NOTE: calib.parm.names
        setkey(x, iter, step)
        B_in <- x[step <= N_centers, list(B.in = sum(in_range, na.rm = TRUE)), by = .(iter, step)]
        if (exists("mean_cov")) {
          new_mean_cov <- get_mean_cov(
            iter = main_i1 + 1,
            mu = center_next[, all_parm_names],
            sd = sd_next,
            center = center_draw,
            B = B_in,
            parm_names = all_parm_names
          ) # CM NOTE: calib.parm.names
          new_rows <- (nrow(mean_cov) + 1):(nrow(mean_cov) + nrow(new_mean_cov))
          mean_cov <- rbind(mean_cov, new_mean_cov)
        } else {
          mean_cov <- get_mean_cov(
            iter = main_i1 + 1,
            mu = center_next[, all_parm_names],
            sd = sd_next,
            center = center_draw,
            B = B_in,
            parm_names = all_parm_names
          ) # CM NOTE: calib.parm.names
          new_rows <- 1:nrow(mean_cov)
        }
        # Store results
        save_results(
          mean_cov[new_rows, c("iter", "step", "center", "B.in", "parm", all_parm_names), with = FALSE], meancov_outfile,
          out_dir = output_directory, append = f_append
        ) # CM NOTE: calib.parm.names
        f_append <- TRUE
      } else { # n_in < N_cov_points
        # sample MVN points around centers if there are enough points to
        # estimate the cov matrix
        #--------------------------------------------------------------------------
        n_use <- min(n_in, N_cov_points) # maybe here to have N_cov_points + N_centers
        sample_mean <- as.data.frame(center_next)

        # CM NOTE: I am pretty sure this doesn't matter because we recalculate this in the for loop below
        # Given place in code this is really n_in == N_cov_points
        # if (n_in <= N_cov_points) {
        #   var_data <- good_parm_draws[1:n_use, all_parm_names, with = FALSE] # CM NOTE: calib.parm.names
        #   sample_cov <- parm_covariance(var_data)
        #   if (any(diag(sample_cov) == 0)) {
        #     # this occurs when adding a new parameter: it is set to default for all prior draws
        #     is_zero <- diag(sample_cov) == 0
        #     sd_next <- 0.5*attr(priors, "sds")
        #     sd_next[!is_zero] <- 0
        #     diag(sample_cov) <- diag(sample_cov) + sd_next^2
        #   }
        # } # n_in <= N_cov_points

        # For tracking which rows need to be written out
        new_rows <- c()
        for (center_i1 in 1:num_centers) {
          sample_mean_i1 <- unlist(sample_mean[center_i1, all_parm_names]) # CM NOTE: calib.parm.names
          # Determine rows to be filled for a given center
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
            # CM NOTE: Done above
            # # Determine rows to be filled for a given center
            # draw_rows <- ((center_i1 - 1)*Center_n + 1):(center_i1*Center_n)

            # Perform random draws
            parm_draws[draw_rows, (all_parm_names) := draw_parms(
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
              priors = priors#,
              # CM NOTE: If we added in fixed parameter capabilities, this will be needed again
              # parm_names = all_parm_names
            )
            if (is.null(x)) {
              warning(sprintf("No valid parameters were simulated for center = %s during iteration %s", center_i1, main_i1))
              # Move to the next center
              next
            }
            # CM NOTE: Done above
            # # Get rows to update for a given center
            # draw_rows <- ((center_i1 - 1)*Center_n + 1):(center_i1*Center_n)
            parm_draws[draw_rows, (all_parm_names) := x] # calib.parm.names

          } # ! abs(sum(sample_cov) - sum(diag(sample_cov))) < 1e-10

          # Store Mean Covariance results
          sample_mean_i1 <- setnames(as.data.frame(t(sample_mean_i1)), all_parm_names) # CM NOTE: calib.parm.names
          sample_cov <- setnames(as.data.frame(sample_cov), all_parm_names) # CM NOTE: calib.parm.names
          mean_cov_center_i1 <- data.table(rbind(sample_mean_i1, sample_cov))
          mean_cov_center_i1$iter <- main_i1 + 1
          mean_cov_center_i1$step <- center_i1
          mean_cov_center_i1$center <- center_draw[center_i1]
          mean_cov_center_i1$parm <- 0:(length(all_parm_names)) # CM NOTE: calib.parm.names
          B_in <- sum(get_in_range(
            compare_list = priors,
            check_dt = parm_draws[draw_rows, ],
            out = "logical"
            # CM NOTE: If we added in fixed parameter capabilities, this will be needed again
            # parm_names = all_parm_names
          )) # CM NOTE: calib.parm.names
          mean_cov_center_i1$B.in <- B_in
          setcolorder(mean_cov_center_i1, c("iter", "step", "center", "B.in" , "parm", all_parm_names)) # CM NOTE: calib.parm.names

          if (exists("mean_cov")) {
            new_rows <- c(new_rows, (nrow(mean_cov) + 1):(nrow(mean_cov) + nrow(mean_cov_center_i1)))
            mean_cov <- rbind(mean_cov, mean_cov_center_i1)
          } else {
            new_rows <- 1:nrow(mean_cov_center_i1)
            mean_cov <- mean_cov_center_i1
          }
        } # center_i1 in 1:num_centers

        # See how many new valid parameters have been generated
        keep_rows <- get_in_range(
          compare_list = priors,
          check_dt = parm_draws,
          out = "logical"
        )
        if (sum(keep_rows) == 0) {
          warning(sprintf("Unable to generate new parameters for any center at iteration %s. Ending run.", main_i1))
          break
        }

        # Remove any invalid rows from the draw, keeping center rows which haven't been filled yet
        if (recalc_centers) { keep_rows <- keep_rows | parm_draws$step %in% (N_centers + 1) }
        # Reset step column to NA for non-valid rows
        parm_draws$step[!keep_rows] <- target_dist$step[!keep_rows] <- sim_parm$step[!keep_rows] <- NA
        # Set new draw numbers
        n_draw <- sum(keep_rows)
        new_draws <- (total_draws + 1):(total_draws + n_draw)
        parm_draws$draw[keep_rows] <- target_dist$draw[keep_rows] <- sim_parm$draw[keep_rows] <- new_draws
        # Set draw seed
        seed_stream_start <- .Random.seed
        parm_draws$seed[keep_rows] <- seed_stream(seed_stream_start, n_draw)
        # reorder data.tables so NAs are on bottom
        setorder(parm_draws, iter, step, na.last = TRUE)
        setorder(target_dist, iter, step, na.last = TRUE)
        setorder(sim_parm, iter, step, na.last = TRUE)

        # Store results
        save_results(
          mean_cov[new_rows, c("iter", "step", "center", "B.in", "parm", all_parm_names), with = FALSE], meancov_outfile,
          out_dir = output_directory, append = f_append
        ) # CM NOTE: calib.parm.names
        f_append <- TRUE

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
      # CM NOTE: Not sure this makes sense, why would we set the append to false after we do an iteration?
      if (continue_runs == TRUE & main_i1 == start_iter) { f_append <- FALSE }
    } # if (main_i1 < end_iter)

    # Save iteration results
    save_results(
      list(.run_info_to_df(targets = targets, priors = priors, iter = main_i1, draw = total_draws), run_meta_df_outfile),
      list(good_parm_draws, parm_df_outfile),
      list(good_sim_parm, sims_df_outfile),
      list(good_target_dist, targ_df_outfile),
      out_dir = output_directory, append = FALSE
    )
    if (verbose) { cat(sprintf("----------- End Iter %s -----------\n", main_i1)) }
    # CM NOTE: Should store prior list and target list
  } # main_i1 in start_iter:end_iter

  # Store results
  # Paramters
  good_parm_draws <- good_parm_draws[!is.na(draw), ]
  # scaled_dist is used for cov calcs only but no reason to remove it. Makes continue a little easier as well
  setorder(good_parm_draws, draw, na.last = TRUE)
  # Subtargets
  good_sim_parm <- good_sim_parm[!is.na(draw), ]
  setorder(good_sim_parm, draw, na.last = TRUE)
  # Major targets
  good_target_dist <- good_target_dist[!is.na(draw), ]
  good_target_dist[, (target_names) := lapply(.SD ,"abs"), .SDcols = target_names]
  setorder(good_target_dist, draw, na.last = TRUE)
  # Save
  save_results(
    list(.run_info_to_df(targets = targets, priors = priors, iter = main_i1, draw = total_draws), run_meta_df_outfile),
    list(good_parm_draws, parm_df_outfile),
    list(good_sim_parm, sims_df_outfile),
    list(good_target_dist, targ_df_outfile),
    out_dir = output_directory, append = FALSE
  )

  # Print information
  done_timestamp <- Sys.time()
  calc_time <- done_timestamp - run_timestamp
  if (verbose) {
    header <- "\n---------- Run Completed ----------"
    time_info <- sprintf("Finishing ABC at %s", format(done_timestamp, print_time_fmt))
    duration_info <- sprintf("Calculation took %s %s", round(calc_time, 2), attr(calc_time, "units"))
    seed_info <- sprintf("seed value is: %s", paste0(.Random.seed, collapse = ", "))
    n_info <- sprintf("number of in range draws was %s", n_in)

    bound_info <- lapply(attr(targets, "target_names"), FUN = function(x, targ) {
      tmp <- targ[x]
      paste(sprintf("%s: %s - %s", x, tmp$current_lower_bounds, tmp$current_upper_bounds), collapse = "\n")
    }, targ = targets)
    bound_info <- lapply(unique(attr(targets, "target_groups")), FUN = function(x, groups, bound_info) {
      paste(unlist(bound_info[x == groups]), collapse = "\n")
    }, groups = attr(targets, "target_groups"), bound_info = bound_info)
    bound_info <- paste(sprintf("---- %s ----", unique(attr(targets, "target_groups"))), bound_info, sep = "\n", collapse = "\n")
    cat(header, time_info, duration_info, seed_info, n_info, "Final bounds info:", bound_info, sep = "\n")
  }

  return(list(
    # CM NOTE: Need to determine what information we absolutely think should be returned
    # e.g. in old code, we only used parm_draws in a new run to determine N_start and N_centers
    #   we could just return those values to keep memory and storage down. we could give an option to return intermediate
    #   results though as well
    # CM NOTE: Only need iterations and draws
    # parm_draws = parm_draws,
    # sim_parm = sim_parm,
    # target_dist = target_dist,
    good_parm_draws = good_parm_draws,
    good_sim_parm = good_sim_parm,
    good_target_dist = good_target_dist,
    mean_cov = mean_cov,
    priors = priors,
    targets = targets
  ))
}

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
