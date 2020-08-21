define_targets <- function(..., previous_run_targets = NULL) {
  # Newly added targets
  new_targets <- list(...)

  if (length(new_targets) > 0) {
    # Convert non-grouped targets to grouped targets for name control
    to_group <- unlist(lapply(new_targets, FUN = function(x) { !"names" %in% names(x) }))
    new_targets[to_group] <- lapply(new_targets[to_group], FUN = function(x) { group_targets(x) })

    # Get target group names
    new_names <- .unique_names(things_list = new_targets, thing = "target_group")

    # Add names to targets
    new_targets <- lapply(seq_along(new_names), FUN = function(x, targs, name) {
      targs[[x]]$target_group <- name[x]
      attr(targs[[x]], which = "target_ids") <- paste(name[x], targs[[x]]$names, sep = "_")

      targs[[x]]
    }, targs = new_targets, name = new_names)
    names(new_targets) <- new_names

    # Initialize update information
    update <- rep_len(TRUE, length.out = length(new_targets))
    names(update) <- new_names

    # Store appropriate attributes
    sub_targets <- unlist(lapply(new_targets, FUN = function(x) { x[["names"]] }))
    n_sub_targs <- unlist(lapply(new_targets, FUN = function(x) { length(x[["names"]]) }))
    target_ids <- paste(rep(new_names, times = n_sub_targs), sub_targets, sep = "_")
    attributes(new_targets)$update <- update
    attributes(new_targets)$target_groups <- new_names
    # attributes(new_targets)$sub_targets <- sub_targets
    attributes(new_targets)$target_ids <- target_ids
  } # length(new_targets) > 0

  # If reading from a previous set of results
  old_targets <- list()
  if (!is.null(previous_run_targets)) {
    # Pull Target Specific Information
    previous_run_targets <- previous_run_targets[previous_run_targets$IMABC == "Target", ]
    previous_run_targets$IMABC <- NULL

    # Get target group attributes
    groups <- previous_run_targets[previous_run_targets$INFO %in% c("group", "update"), ]
    update <- as.logical(groups$VALUE[groups$INFO == "update"])
    names(update) <- groups$ID[groups$INFO == "update"]
    target_groups <- names(update)

    # Target information
    sub_targets <- previous_run_targets[!previous_run_targets$INFO %in% c("group", "update"), ]

    # For each target group
    for (i1 in target_groups) {
      # Find appropriate subtargets
      idx <- grep(paste0("^", i1, "_"), sub_targets$ID)
      tmp <- sub_targets[idx, ]

      # Target IDs
      target_ids <- unique(tmp$ID)
      # Target names
      target_names <- tmp$VALUE[tmp$INFO == "name"]
      # Targets
      target_values <- as.numeric(tmp$VALUE[tmp$INFO == "target"])
      names(target_values) <- target_names
      # Start values
      lower_start <- as.numeric(tmp$VALUE[tmp$INFO == "lower_bound_current"])
      names(lower_start) <- target_names
      upper_start <- as.numeric(tmp$VALUE[tmp$INFO == "upper_bound_current"])
      names(upper_start) <- target_names
      # Stop values
      lower_stop <- as.numeric(tmp$VALUE[tmp$INFO == "lower_bound_stop"])
      names(lower_stop) <- target_names
      upper_stop <- as.numeric(tmp$VALUE[tmp$INFO == "upper_bound_stop"])
      names(upper_stop) <- target_names

      old_tmp <- list(
        target_group = i1,
        names = target_names,
        targets = target_values,
        lower_bounds_start = lower_start,
        upper_bounds_start = upper_start,
        lower_bounds_stop = lower_stop,
        upper_bounds_stop = upper_stop
      )
      attr(old_tmp, "target_ids") <- target_ids

      # Add target group to old targets list
      old_targets[[i1]] <- old_tmp
    }

    # Assign target group attributes
    attr(old_targets, "update") <- update
    attr(old_targets, "target_groups") <- target_groups
    attr(old_targets, "target_ids") <- unique(sub_targets$ID)
  } # !is.null(previous_run_targets)

  # Handle all cases
  if (length(new_targets) == 0 & length(old_targets) == 0) { # No targets added
    stop("Need to add at least one target.")
  } else if (length(new_targets) != 0 & length(old_targets) == 0) { # Only new targets added
    all_targets <- new_targets
  } else if (length(new_targets) == 0 & length(old_targets) != 0) { # Only old targets added
    all_targets <- old_targets
  } else {
    stop("NEED TO UPDATE")
    # All sub_targets
    allsub_old <- attr(old_targets, "sub_targets")
    allsub_new <- attr(new_targets, "sub_targets")

    # Split into newly added and updated
    newnew <- setdiff(allsub_new, allsub_old)
    newold <- intersect(allsub_new, allsub_old)

    # Initialize final target list using old targets as a starting spot
    all_targets <- old_targets

    # If person adds a new sub target already in old sub targets
    if (length(newold) > 0) {
      # For each new sub target in the old target list
      for (i1 in newold) {
        # Which major target is current sub target in
        new_major <- names(new_targets)[grep(i1, new_targets)]
        old_major <- names(all_targets)[grep(i1, new_targets)]

        # CM NOTE: Error for now. May be able to control later
        stopifnot(
          "define_targets: Subtarget defined in multiple main targets" =
            length(new_major) == 1 & length(old_major == 1),
          "define_targets: Attempting to add an old sub target to a different main target" =
            new_major == old_major
        )

        # Replace the old version of the sub target with the new version of the sub target
        all_targets[[new_major]] <- mapply(
          # Old target list       , New target list
          all_targets[[new_major]], new_targets[[new_major]],
          FUN = function(x, y, add) {
            # All elements of the main list are named except for one. This finds the index from the new list we are
            #   trying to add and ignores new sub targets that should not be added
            sub_to_use <- union(which(y == add), which(names(y) == add))
            sub_to_fix <- union(which(x == add), which(names(x) == add))
            x[sub_to_fix] <- y[sub_to_use]

            return(x)
          },
          MoreArgs = list(add = i1),
          SIMPLIFY = FALSE
        )

        # Update attributes
        attr(all_targets, "update")[new_major] <- TRUE
      } # i1 in newold
    }

    # If person adds a new sub target not found in old sub targets
    if (length(newnew) > 0) {
      # For each new sub target not in the old target list
      for (i1 in newnew) {
        # Which major target is current sub target in
        new_major <- names(new_targets)[grep(i1, new_targets)]

        # If new sub target is in old main target
        if (new_major %in% names(all_targets)) {
          # Add the current sub target to the appropriate main target list
          all_targets[[new_major]] <- mapply(
            # Old target list       , New target list
            all_targets[[new_major]], new_targets[[new_major]],
            FUN = function(x, y, add) {
              # All elements of the main list are named except for one. This finds the index from the new list we are
              #   trying to add and ignores new sub targets that should not be added
              sub_to_add <- union(which(y == add), which(names(y) == add))
              if (length(sub_to_add) > 0) {
                c(x, y[sub_to_add])
              } else {
                x
              }
            },
            MoreArgs = list(add = i1),
            SIMPLIFY = FALSE
          )

          # Update the attributes of the final target list
          attr(all_targets, "update")[new_major] <- TRUE
          attr(all_targets, "sub_targets") <- c(attr(all_targets, "sub_targets"), i1)
        } else { # new_major %in% names(all_targets)
          # If new sub target is in new main target
          tmp <- list(lapply(new_targets[[new_major]], FUN = function(x, sub) {
            sub_to_pull <- union(which(x == sub), which(names(x) == sub))
            x[sub_to_pull]
          }, sub = i1))
          names(tmp) <- new_major

          # Update attributes
          current_attr <- attributes(all_targets)
          current_attr$names <- c(current_attr$names, new_major)
          current_attr$update <- c(current_attr$update, TRUE)
          names(current_attr$update)[length(current_attr$update)] <- new_major
          current_attr$sub_targets <- c(current_attr$sub_targets, i1)

          # Update final list
          all_targets <- c(all_targets, tmp)
          attributes(all_targets) <- current_attr
        } # ! new_major %in% names(all_targets)
      } # i1 in newnew
    } # length(newnew) > 0
  } # length(new_targets) != 0 & length(old_targets) != 0

  # CM NOTE: Need to check what happens when multiple new targets are added to a new main target

  return(all_targets)
}
