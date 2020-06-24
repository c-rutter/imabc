#########################################################################################################################
# Function Start ########################################################################################################
imabc <- function(priors, parm_names, targets, dist_func, N_start = 1, seed = 1234, latinHypercube = TRUE) {
  # CM NOTE: Not taken care of throughout: Continue.runs
  # Checks --------------------------------------------------------------------------------------------------------------
  # Check all params found in models have priors
  n_parms <- length(parm_names)
  tryCatch(
    stopifnot(parm_names %in% names(priors)),
    error = function(e) stop("All Parameters must be given Prior Distribution Information")
  )

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
  n_store <- N_post + N_centers*(B + 1)
  # initializations needed for new runs
  start_iter <- 1
  end_iter <- max_iter
  total_draws <- 0
  prevruns.dir <- NULL
  n_draw <- N_start # first iteration only, then set to n.center*B
  n_rows_init <- max(n_draw, N_centers*B) + recalc_centers*N_centers
  n_in <- 0
  n_use <- 0

  # Parameter Handling --------------------------------------------------------------------------------------------------
  # Determine number of paramters to calibrate
  n_parms <- length(parm_names)
  # CM NOTE: In toy model we don't worry about a fixed parm but if we change then this code would need to be implemented
  #   and code with parm_names and/or n_parms would need to be evaulated for appropriateness
  # calibrate_parms <- unlist(lapply(priors, FUN = function(x) !attributes(x)$fixed))
  # calibrate_parm_names <- names(calibrate_parms)[calibrate_parms]
  # n_calib_parms <- sum(calibrate_parms)

  # Priors Handling -----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  parm_draws <- init_run_dt(n = n_rows_init, parms = parm_names, type = "draw")
  good_parm_draws <- init_run_dt(n_store, parms = parm_names, type = "draw")
  parm_draws$seed <- seed_stream(seed_stream_start, n_rows_init)
  sim_parm <- init_run_dt(n_rows_init, parm_names, type = "sim")
  good_sim_parm <- init_run_dt(n_store, parm_names, type = "sim")

  if (!continue.runs) {
    # Generate random inputs for prior distribution calculation
    if (latinHypercube) {
      u_draws <- lhs::randomLHS(N_start, n_parms)
    } else {
      u_draws <- matrix(runif(N_start*n_parms), nrow = N_start, ncol = n_parms)
    }
    colnames(u_draws) <- parm_names

    # Generate parameter space from prior distribution functions
    parm_draws <- parms_from_priors(parm_df = parm_draws, name_parms = parm_names, prior_list = priors, sampling = u_draws)
  }

  # Targets Handling ----------------------------------------------------------------------------------------------------
  # Initialize inputs/results data frames
  target_names <- names(targets)
  n_targets <- length(target_names)
  target_dist <- init_run_dt(n_rows_init, target_names, type = "distance")
  good_target_dist <- init_run_dt(n_store, target_names, type = "distance")

  # Miscellaneous Handling ----------------------------------------------------------------------------------------------
  if (N_cov_points == 0) { N_cov_points <- 25*n_parms }

  # Main Loop -----------------------------------------------------------------------------------------------------------
  for (i1 in start_iter:end_iter) {
    # CM NOTE: used for testing iterations 1 at a time
    # if (TRUE) {
    #   warning("In Testing iter is set to 1")
    #   i1 <- 2
    # }
    n_in_i <- 0 # CM NOTE: Better names

    # If not the first iteration on a continuing run
    if (!(continue.runs == TRUE & i1 == start_iter)) {
      # Parms to check
      parms_to_run <- parm_draws[1:n_draw, c("seed", parm_names), with = FALSE]
      # Setup parallel handling
      registerDoParallel(cores = detectCores() - 1) # cluster auto-closed with foreach
      # User defined Distance Function applied on all simulated parms
      res <- foreach(i1 = 1:nrow(parms_to_run), .combine = combine_results) %dopar% {
        inp <- as.numeric(parms_to_run[i1, parm_names, with = FALSE])
        res <- dist_func(inp, targets)
        list(ll = res, sp = inp) # CM NOTE: Better names
      }

      # Store results
      target_dist[1:n_draw, (target_names) := res$ll]
      sim_parm[1:n_draw, (parm_names) := res$sp]
      total_draws <- total_draws + n_draw

      # Evaluate Targets
      target_dist <- eval_targets(dist_target = target_dist, target_names = target_names, target_list = targets)

      # Count the good points: points associated with positive distances
      target_dist$n_good[1:n_draw] <- rowSums(target_dist[1:n_draw, (target_names), with = FALSE] >= 0, na.rm = TRUE)
      n_in_i <- sum(target_dist$n_good[target_dist$step <= N_centers] == n_targets, na.rm = TRUE)

      # replace re-simulated targets for center draws in good.* matrices
      # and recalculate p-value and distance
      if (recalc_centers & i1 > 1) {
        # What were the centers from last time
        center_draw <- parm_draws$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)]

        # sort to ensure alignment across data tables
        # CM NOTE: This really shouldn't matter - I think this has to do help how values are being replace/updated
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
        good_sim_parm[draw %in% center_draw, c("draw", parm_names)] <-
          sim_parm[draw %in% center_draw, c("draw", parm_names), with = FALSE]
        good_target_dist[draw %in% center_draw, c("draw", target_names)] <-
          target_dist[draw %in% center_draw, c("draw", target_names), with = FALSE]
        good_target_dist[draw %in% center_draw,]$n_good <-
          rowSums(good_target_dist[draw %in% center_draw, (target_names), with = FALSE] >= 0, na.rm = TRUE)
        good_target_dist$n_good[is.na(good_target_dist$n_good)] <- 0L

        # Determine which draws to keep and which to remove
        remove_draws <- good_target_dist[((draw %in% center_draw) & (n_good < n_targets)), ]$draw
        keep_draws <- good_target_dist[((draw %in% center_draw) & (n_good == n_targets)), ]$draw

        # CM NOTE: Not worked on yet
        # Update good draw distance and stopping criteria
        # if (length(keep_draws) > 0) {
        #   # if there are any centers that are kept, recalculate
        #   # total distance and alpha-levels
        #   good_target_dist[draw %in% keep.draws,]$tot.dist =
        #     get.updating.dist(x=good_target_dist[draw %in% keep.draws,],
        #                       update.names)
        #
        #   if(any(target.specs$update.alpha)){
        #     good_target_dist[draw %in% keep.draws,]$alpha =
        #       get.alpha(sim.p=good_sim_parm[draw %in% keep.draws,],
        #                 sim.p.names=update.target.names,
        #                 target.data=calib.targets,
        #                 specs=target.specs[target.specs$update.alpha,])
        #   }else{
        #     good_target_dist$alpha = get.alpha(sim.p=good_sim_parm,
        #                                        sim.p.names=calib.target.names,
        #                                        target.data=calib.targets,
        #                                        specs=target.specs)
        #   }
        #
        # } # length(keep_draws) > 0

        # Remove bad draws
        if (length(remove_draws) > 0) {
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
        parm_draws[step == (N_centers + 1), (parm_names) := NA_real_]
        sim_parm[step == (N_centers + 1), (parm_names) := NA_real_]
        target_dist[step == (N_centers + 1), c(target_names, "tot_dist") := NA_real_]
        target_dist[step == (N_centers + 1), n_good := 0L]

        # Total draws being kept
        n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)

      } # if (recalc.centers & iter > start_iter)

      # save good draws, distances, and simulated parms
      # CM NOTE: these seem like a round about way to add rows from parm_draws, etc. to the good.* versions
      #   could probably write more efficiently and clearly with which/subset and rbind while also ignoring or doing
      #   away with the draw variable
      # CM NOTE: What do we do if there are no n_in_i?
      if (n_in_i > 0) {
        if ((i1 == 1) & (n_in_i > n_store)) {
          # CM NOTE: Not worked on yet
          # keep the N.store best draws (largest alpha level & smallest distance)
          # setorder(target_dist, -alpha, tot_dist, na.last = TRUE)
          # add.draws = target.dist$draw[1:N.store]
          # setorder(target.dist,draw,na.last=TRUE)  # likely an unnecessary sort
          # good_target_dist <- target.dist[draw %in% add.draws,]
          # good_parm_draws  <- parm.draws[draw %in% add.draws,]
          # good_sim_parm    <- sim.parm[draw %in% add.draws,]
          # add.row.range = 1:N.store

        } else { # ! (i1 == 1) & (n_in_i > n_store)
          if ((n_in + n_in_i) > n_store) {
            # CM NOTE: Not worked on yet
            # keep the best (N.store - n_in_i) draws (largest alpha level & smallest distance)
            # and add the current n_in_i runs to the bottom
            # N.keep=N.store-n_in_i
            # setorder(good_target_dist,-alpha,tot.dist,na.last=TRUE)
            # keep.draws = good_target_dist$draw[1:N.keep]
            # setorder(good_target_dist,draw,na.last=TRUE)  # likely an unnecessary sort
            # good_parm_draws[1:N.keep,]  <- good_parm_draws[draw %in% keep.draws,]
            # good_sim_parm [1:N.keep,]   <- good_sim_parm[draw %in% keep.draws,]
            # add.row.range = (N.keep+1):(N.store)

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

        # CM NOTE: Not worked on yet
        # Can we update the target alpha level
        # if (any(target.specs$update.alpha)) {
        #   good.target.dist[add.row.range,]$alpha =
        #     get.alpha(sim.p=good.sim.parm[add.row.range,],
        #               sim.p.names=update.target.names,
        #               target.data=calib.targets,
        #               specs=target.specs[target.specs$update.alpha,])
        # } else { # ! any(target.specs$update.alpha)
        #   good.target.dist[add.row.range,]$alpha =
        #     get.alpha(sim.p=good.sim.parm[add.row.range,],
        #               sim.p.names=calib.target.names,
        #               target.data=calib.targets,
        #               specs=target.specs)
        # } # any(target.specs$update.alpha)
        # good_target_dist[add_row_range, ]$tot_dist <- get.updating.dist(
        #   x = good.target.dist[add_row_range, ],
        #   target.names = update.names
        # )
      } # n_in_i > 0

      n_in <- sum(good_target_dist$n_good == n_targets, na.rm = TRUE)
    } # !(continue.runs == TRUE & iter == start_iter)
    # Completed good rows
    good_row_range <- 1:n_in

    #####################################################################################################################
    # CM NOTE: Not worked on yet ########################################################################################
    # if((N.in>=2*N.cov.points) & any(target.specs$update.alpha)){
    #
    #   ##########################################################################
    #   # until target alpha-levels are reached, update alpha-levels every
    #   # 2*N.cov.points, reducing good points by half
    #   ###########################################################################
    #
    #   # 1. Identify the point that yields best n.keep draws,
    #   #    based on \rho_{i\cdot} and  distance from targets still being updated.
    #
    #   good.target.dist$tot.dist = get.updating.dist(x=good.target.dist,
    #                                                 target.names=update.names)
    #
    #   # ensure that we end up with at least N.cov.points.
    #   # This is needed if alpha.target differs across targets & there is tension in targets
    #
    #   keep.points = c(trunc( seq(1.0,2.0,0.1)*N.cov.points),N.in)
    #
    #   for(N.get in keep.points){
    #
    #     if(N.get<N.in){
    #
    #       alpha.draw = good.target.dist[good.row.range,][
    #         order(-alpha,tot.dist,na.last=TRUE)][N.get]$draw
    #
    #       # get new.alpha for all targets currently being updated,
    #       # based on simulated values at alpha.draw
    #       target.specs[target.specs$update.alpha,]$new.alpha =
    #         update.alpha(sim.p=good.sim.parm[draw==alpha.draw,],
    #                      sim.p.names=update.target.names,
    #                      target.data=calib.targets,
    #                      specs=target.specs[target.specs$update.alpha,])
    #     }else{
    #       # return target.specs to original values
    #       target.specs[target.specs$update.alpha,]$new.alpha =
    #         target.specs[target.specs$update.alpha,]$alpha
    #
    #     }
    #
    #     if(any(target.specs[target.specs$update.alpha,]$new.alpha>
    #            target.specs[target.specs$update.alpha,]$alpha) | N.get==N.in){
    #       # if any new.alpha>alpha
    #
    #       # don't go above specified target
    #       target.specs[target.specs$update.alpha,]$new.alpha =
    #         apply(target.specs[target.specs$update.alpha,
    #                            c("new.alpha","alpha.target")], 1, FUN=min)
    #
    #       for(i in 1:length(update.names)){
    #
    #         # 2. update tolerance bounds
    #         calib.targets[[update.names[i]]] =
    #           get.bounds(x=calib.targets[[update.names[i]]],
    #                      alpha.level=target.specs[
    #                        target.specs$data==update.names[i],]$new.alpha)
    #       }
    #       # 3. update distances (values<0 indicate out of tolerance bounds)
    #       good.target.dist[good.row.range,] =
    #         update.in.range(target.dist=good.target.dist[good.row.range,],
    #                         sim.parm=good.sim.parm[good.row.range,],
    #                         target.specs=target.specs[target.specs$update.alpha,],
    #                         calib.targets=calib.targets)
    #
    #       # update n.good
    #       good.target.dist$n.good = as.integer(0)
    #       good.target.dist[good.row.range]$n.good =
    #         rowSums(good.target.dist[good.row.range,
    #                                  (dist.names),with=FALSE]>=0,na.rm=TRUE)
    #
    #       N.in.new   = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)
    #       if(N.in.new>=N.cov.points) break
    #     } # if any new.alpha>alpha
    #   } # for(i.get in keep.points)
    #
    #   if(N.in.new>=N.cov.points & N.in.new<N.in){
    #     # some points are dropped
    #
    #     target.specs[target.specs$update.alpha,]$alpha =
    #       target.specs[target.specs$update.alpha,]$new.alpha
    #
    #     # update N.in
    #     keep.draws = good.target.dist[n.good==n.targets,]$draw
    #     N.in   = sum(good.target.dist$n.good==n.targets,na.rm=TRUE)
    #
    #
    #     # update the names of targets with alpha-levels less than target alpha
    #     update.names = target.specs$data[target.specs$alpha<
    #                                        target.specs$alpha.target]
    #
    #     # if(N.in>N.cov.points &
    #     #    (sum(target.specs$alpha<target.specs$alpha.target)>=1)){
    #     #   # if more than N.cov.points that meet the updated alpha-criteria &
    #     #   # we are still updating tolerance intervals for some targets, then
    #     #   # keep only the closest N.cov.points among them, based on nearness
    #     #   # to targets still being updated. needed b/c SEER data starts @ alpha=0
    #     #
    #     #   good.target.dist[,tot.dist := NA]
    #     #   good.target.dist[draw %in% keep.draws,
    #     #                    tot.dist :=
    #     #                      get.updating.dist(x=good.target.dist[draw %in%
    #     #                                                           keep.draws,],
    #     #                                        update.names)]
    #     #
    #     #   setorder(good.target.dist,-n.good,-alpha,tot.dist,na.last=TRUE)
    #     #   keep.draws = good.target.dist[1:N.cov.points,]$draw
    #     #   setorder(good.target.dist,draw)
    #     #   N.in=N.cov.points
    #     #
    #     # }
    #
    #     good.row.range = 1:N.in
    #     good.target.dist[good.row.range,]<-good.target.dist[draw %in%
    #                                                           keep.draws,]
    #     good.parm.draws[good.row.range]  <-good.parm.draws[draw %in% keep.draws]
    #     good.sim.parm[good.row.range]    <-good.sim.parm[draw %in% keep.draws]
    #
    #     if(N.in<N.store){
    #       # clear unused rows in good.* data tables
    #       blank.rows = (N.in+1):N.store
    #
    #       good.parm.draws[blank.rows,c("draw","step","iter") := as.integer(NA)]
    #       good.parm.draws[blank.rows,c(parm.names,"scaled.dist","sample.wt") :=
    #                         as.numeric(NA)]
    #       good.parm.draws$seed[blank.rows]=""
    #
    #       good.sim.parm[blank.rows,c("draw","step","iter") := as.integer(NA)]
    #       good.sim.parm[blank.rows,(sim.parm.names) := as.numeric(NA)]
    #
    #       good.target.dist[blank.rows,c("draw","step","iter") := as.integer(NA)]
    #       good.target.dist[blank.rows,c(dist.names,"tot.dist","alpha") :=
    #                          as.numeric(NA)]
    #       good.target.dist$n.good[blank.rows]=as.integer(0)
    #
    #     } #  if(N.in<N.store)
    #
    #     if(print.status){
    #       print(paste0("Updated alpha, new N.in=",N.in,
    #                    " new alpha-levels: ",
    #                    paste(target.specs[target.specs$update.alpha & !bias.info,]$data," ",
    #                          format(
    #                            target.specs[
    #                              target.specs$update.alpha & !bias.info,]$alpha,
    #                            digits=2),
    #                          collapse="; ")
    #       ))
    #     }
    #
    #     # update.alpha: indicates if tolerance intervals are still being adjusted
    #     target.specs$update.alpha = target.specs$alpha<target.specs$alpha.target
    #     update.target.names = get.update.targets(calib.target.names,
    #                                              target.specs$data[
    #                                                target.specs$update.alpha])
    #
    #     # if N.in.new<N.in - alpha levels are updated
    #   }else{
    #     if(print.status){
    #       print(paste0("Unable to update alpha-levels, N.in=",N.in,
    #                    " alpha-levels below targets: ",
    #                    paste(target.specs[target.specs$update.alpha & !bias.info,]$data," ",
    #                          format(
    #                            target.specs[
    #                              target.specs$update.alpha & !bias.info,]$alpha,
    #                            digits=2),
    #                          collapse="; ")
    #       ))
    #     }
    #
    #   }
    #
    # }

    # if((N.in>=N.post | !any(target.specs$update.alpha)) |
    #    (iter>=end.iter & N.in>0)){
    #
    #   good.parm.draws$sample.wt = 0
    #   in.draws = good.target.dist[!is.na(draw),]$draw
    #   if(continue.runs==TRUE & iter==start.iter & start.iter>1){
    #     # if continuing runs, at the first iter use only previous mixture distns
    #     m.file=paste0(prevruns.dir,"/",outfile.sampling)
    #   }
    #   else if(continue.runs==TRUE & start.iter==1){
    #     m.file=paste0(output.directory,"/",outfile.sampling)
    #   }
    #   else {
    #     m.file=paste0(c(prevruns.dir,output.directory),"/",outfile.sampling)
    #   }
    #
    #   good.parm.draws[draw %in% in.draws,]$sample.wt =
    #     get.weight(parms=good.parm.draws[draw %in% in.draws,],
    #                p.names=calib.parm.names,
    #                priors=parm.priors.df,
    #                mixture.file=m.file,
    #                N0)
    #
    #   # calculate effective sample size using Kish formula. Here sum(sample.wt)=1
    #   ESS = 1/sum(good.parm.draws[draw %in% in.draws,]$sample.wt^2)
    #   if(print.status){
    #     print(paste("    Effective Sample Size is",round(ESS,2)))
    #   }
    # }
    #
    # # Determine if there are enough points to quit
    # # ---------------------------------------------------------------------------
    # if(ESS>=N.post & !any(target.specs$update.alpha)){ # stop if ESS > N.post
    #   break
    # }
    #####################################################################################################################

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
      # CM NOTE: Not worked on yet
      # if(!any(target.specs$update.alpha)){ # & N.in>500){
      #   max.wt = max(good.parm.draws[draw %in% in.draws,]$sample.wt)
      #   if(max.wt >= 10/N.in){
      #     draw.order = setorder(good.parm.draws[good.row.range,],
      #                           -sample.wt,na.last=TRUE)$draw
      #     N.hiwt = min(N.centers,length(draw.order))
      #     center_draw_hiwt= draw.order[1:N.hiwt]
      #
      #     if(print.status){
      #       print(paste0("adding samples around high weight points: ",
      #                    paste0(center_draw_hiwt,collapse=", ")))
      #     } # print.status
      #
      #   } # max.wt >= 10/N.in
      # } # !any(target.specs$update.alpha)

      n_best_draw <- 0
      center_draw_best <- NULL
      if (n_hiwt < N_centers) {
        # CM NOTE: is it really faster to sort good_target_dist to get draw order and then resort it or would it be
        #   better to just do something like draw_order = df$draw[order(df$tot_dist[good_row_range], na.lat = T)] which
        #   wouldn't require us to sort good_target_dist twice
        draw_order <- setorder(good_target_dist[good_row_range, ], tot_dist, na.last = TRUE)$draw
        setorder(good_target_dist, draw, na.last = TRUE)
        n_best_draw <- min(n_in, (N_centers - n_hiwt))
        center_draw_best <- draw_order[1:n_best_draw]
      } # n_hiwt < N_centers
      num_centers <- n_best_draw + n_hiwt
      # CM NOTE: order of center_draw and center_next are potentially not the same (search CENTER_ORDER_NOTE for extra info)
      center_draw <- c(center_draw_best, center_draw_hiwt)
      center_next <- as.matrix(good_parm_draws[draw %in% center_draw, parm_names, with = FALSE])

      ###########################################################################
      # Sample B draws around these centers. Some draws may be out of range
      ###########################################################################
      # re-initialize parm_draws, sim.parms, & target dist before simulating
      # draws for next iteration
      # CM NOTE: It seems like this leaves a lot of extra rows that aren't needed, we could just re-inialize these
      #   data objects using our init_* function. which would get rid of the extra information (just need to pass info
      #   on the iteration, draw numbers and step numbers)
      #--------------------------------------------------------------------------
      # Add additional rows
      n_draw <- num_centers*B + recalc_centers*num_centers
      new_steps <- rep(1:num_centers, each = B)
      if (recalc_centers) { new_steps <- c(new_steps, rep.int((N_centers + 1), num_centers)) }
      new_draws <- (total_draws + 1):(total_draws + n_draw)

      # Reset calculation information
      parm_draws$draw <- target_dist$draw <- sim_parm$draw <- NA_integer_
      parm_draws$step <- target_dist$step <- sim_parm$step <- NA_integer_
      parm_draws$iter <- target_dist$iter <- sim_parm$iter <- i1 + 1
      parm_draws$draw[1:n_draw] <- target_dist$draw[1:n_draw] <- sim_parm$draw[1:n_draw] <- new_draws
      parm_draws$step[1:n_draw] <- target_dist$step[1:n_draw] <- sim_parm$step[1:n_draw] <- as.integer(new_steps)
      parm_draws[, c(parm_names, "scaled_dist", "sample_wt") := NA_real_]
      parm_draws$seed <- ""
      seed_stream_start <- .Random.seed
      parm_draws$seed[1:n_draw] <- seed_stream(seed_stream_start, n_draw)
      # parm_draws[1:n.draw, seed := seed_stream(seed_stream_start, n_draw)]
      sim_parm[, (parm_names) := NA_real_]
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
        prior_sds <- unlist(lapply(priors, FUN = function(x) { attr(x, "sd") }))
        sd_next <- matrix(0.5*prior_sds, ncol = n_parms, nrow = num_centers, byrow = TRUE)
        colnames(sd_next) <- names(priors)
        # Perform random draws
        parm_draws[1:(num_centers*B), (parm_names) := draw_parms(
          n_add = B,
          mu = center_next[, parm_names], # if not dealing with fixed parms else parm_names needs to be just fixed_parm_names
          sigma = sd_next,
          priors_list = priors,
          targets_list <- targets
        )]

        # If recalculating centers
        if (recalc_centers) {
          # CM NOTE: order of center_draw and center_next are potentially not the same (search CENTER_ORDER_NOTE for extra
          #   info) the issue occurs because center_draw is sorted by tot_dist where center_next is sorted on draw. I
          #   am not sure it matters and I don't know if it affects anywhere else but I am fixing it here just in case
          center_draw <- sort(center_draw)
          parm_draws[parm_draws$step == (N_centers + 1), (parm_names)] <- as.data.table(center_next)
          parm_draws$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
          sim_parm$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
          target_dist$draw[parm_draws$step == (N_centers + 1) & !is.na(parm_draws$step)] <- center_draw
        }

        # CM NOTE: Not worked on yet
        # x = parm.draws[1:n_draw, c("iter","step"), with = FALSE]
        # x$in.range = get.in.range(parm.draws[1:n.draw,calib.parm.names,with=FALSE],
        #                           parm.priors.df,
        #                           calib.parm.names)
        #
        # setkey(x,iter,step)
        # B.in=x[step<=N.centers,list(B.in = sum(in.range,na.rm=TRUE)),
        #        by = .(iter,step)]
        #
        # sampling.output = get.sampling.output(iter=iter+1,
        #                                       mu=center.next[,calib.parm.names],
        #                                       sd=sd.next,
        #                                       center=center.draw,
        #                                       B=B.in,
        #                                       parm.names=calib.parm.names)
        #
        # write.table(sampling.output[,c("iter","step","center","B.in",
        #                                "parm",calib.parm.names),
        #                             with=FALSE],
        #             file=paste0(output.directory,"/",outfile.sampling),sep=",",
        #             append=f.append,
        #             col.names=!f.append,
        #             row.names=FALSE)
        # f.append=TRUE
      } else { # ! n_in < N_cov_points
        #################################################################################################################
        # CM NOTE: Not worked on yet ####################################################################################
        # sample MVN points around centers if there are enough points to
        # estimate the cov matrix
        #--------------------------------------------------------------------------
        # n.use = min(N.in,N.cov.points)
        # sample.mean = as.data.frame(center.next)
        #
        # if(N.in<=N.cov.points){
        #   var.data=good.parm.draws[1:n.use,calib.parm.names,with=FALSE]
        #   sample.cov = get.parm.cov(var.data)
        #   if(sample.cov==-1) return()
        #   if(any(diag(sample.cov)==0)){
        #     # this occurs when adding a new parameter: it is set to default for all prior draws
        #     is.zero=(diag(sample.cov)==0)
        #     sd.next=0.5*parm.priors.df$sd
        #     sd.next[!is.zero] = 0
        #     diag(sample.cov) <- diag(sample.cov)+(sd.next^2)
        #   }
        # }
        #
        # for(i.center in 1:num.centers){
        #   sample.mean.i = as.vector(
        #     unlist(sample.mean[i.center,
        #                        which(names(sample.mean) %in%
        #                                calib.parm.names)]))
        #   draw.rows = ((i.center-1)*B + 1):(i.center*B)
        #
        #   if(N.in>=N.cov.points){ # use different var-cov matrices for each center
        #     # Find the n.use closest draws to each center point,
        #     #------------------------------------------------------------------------
        #     good.parm.draws$scaled.dist = Inf
        #     good.parm.draws$scaled.dist[1:N.in] =
        #       get.dist(p.draws=good.parm.draws[1:N.in,],
        #                p.names=parm.names,
        #                mu=as.vector(center.next[i.center,calib.parm.names]),
        #                sd=parm.priors.df$sd)
        #     setorder(good.parm.draws,scaled.dist,na.last=TRUE)
        #     var.data=good.parm.draws[1:n.use,calib.parm.names,with=FALSE]
        #     sample.cov = get.parm.cov(var.data)
        #     if(sample.cov==-1) return()
        #     if(any(diag(sample.cov)==0)){
        #       # this occurs when adding a new parameter: it is set to default for all prior draws
        #       is.zero=(diag(sample.cov)==0)
        #       sd.next=0.5*parm.priors.df$sd
        #       sd.next[!is.zero] = 0
        #       diag(sample.cov) <- diag(sample.cov)+(sd.next^2)
        #     }
        #   }
        #
        #   # Draw B new parm values usign an MVN draw...............................
        #
        #   # assign fixed parameters
        #   parm.draws[draw.rows,(fixed.parm.names) := as.list(fixed.parm.values)]
        #
        #   # simulate B random draws of calibrated parameters
        #   if(abs(sum(sample.cov) - sum(diag(sample.cov)))<1e-10){
        #     parm.draws[draw.rows,(calib.parm.names) := draw.parms(n.add=B,
        #                                                           mu=as.matrix(t(sample.mean.i)),
        #                                                           sigma=as.matrix(t(diag(sample.cov))),
        #                                                           parm.priors=parm.priors.df,
        #                                                           parm.names=calib.parm.names,
        #                                                           calib.targets)]
        #
        #   }else{
        #     x=get.B.draws(B,
        #                   inflate=sample.inflate,
        #                   center=sample.mean.i,
        #                   cov=sample.cov,
        #                   priors=parm.priors.df,
        #                   p.names=calib.parm.names)
        #     if(is.null(x)){
        #       print(paste("iteration=",iter,"center=",i.center))
        #       return()
        #     }
        #     parm.draws[draw.rows,(calib.parm.names) := x]
        #   }
        #   sample.mean.i =setnames(as.data.frame(t(sample.mean.i)),
        #                           calib.parm.names)
        #   sample.cov = setnames(as.data.frame(sample.cov),
        #                         calib.parm.names)
        #
        #   sampling.output= data.table(rbind(sample.mean.i,sample.cov))
        #   sampling.output$iter = iter+1
        #   sampling.output$step = i.center
        #   sampling.output$center=center.draw[i.center]
        #   sampling.output$parm = 0:(length(calib.parm.names))
        #   B.in = sum(get.in.range(parm.draws[draw.rows,],
        #                           parm.priors.df,calib.parm.names))
        #   sampling.output$B.in = B.in
        #   if(B.in==0) print(paste("*** warning: B.in=",B.in,
        #                           "for iter=",iter+1,"and center=",i.center))
        #
        #   setcolorder(sampling.output,
        #               c("iter","step","center","B.in","parm",calib.parm.names))
        #
        #   write.table(sampling.output[,c("iter","step","center","B.in",
        #                                  "parm",calib.parm.names),
        #                               with=FALSE],
        #               file=paste0(output.directory,"/",outfile.sampling),sep=",",
        #               append=f.append,
        #               col.names=!f.append,
        #               row.names=FALSE)
        #   f.append=TRUE
        #
        # } # loop over centers
        #
        # if(recalc.centers){
        #   parm.draws[step==(N.centers+1),(parm.names)] = as.data.table(center.next)
        #   parm.draws[step==(N.centers+1),]$draw = center.draw
        #   sim.parm[step==(N.centers+1),]$draw = center.draw
        #   target.dist[step==(N.centers+1),]$draw = center.draw
        # }
        #
        # # put good.* data tables in draw order
        # # (though all references are by draw %in% avoid potential problems)
        # setorder(good.parm.draws,draw,na.last = TRUE)
        # setorder(good.sim.parm,draw,na.last = TRUE)
        # setorder(good.target.dist,draw,na.last = TRUE)

      } # !(n_in < N_cov_points)
      # if (continue.runs == TRUE & i1 == start_iter) { f.append <- FALSE }
      ###################################################################################################################

    } # if (i1 < end_iter)

  } # End Main Loop

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
  good_target_dist[, (dist.names) := lapply(.SD ,"abs"), .SDcols = target_names]
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
