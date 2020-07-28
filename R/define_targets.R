define_targets <- function(..., previous_run_targets = NULL) {
  # Newly added targets
  new_targets <- list(...)

  if (length(new_targets) > 0) {
    # Convert non-grouped targets to grouped targets for name control
    to_group <- unlist(lapply(new_targets, FUN = function(x) { !"names" %in% names(x) }))
    new_targets[to_group] <- lapply(new_targets[to_group], FUN = function(x) { group_targets(x) })

    # Get target group names
    new_names <- name_targets(new_targets, grouping = FALSE)

    # Add names to targets
    new_targets <- lapply(seq_along(new_names), FUN = function(x, targs, name) {
      attr(targs[[x]], which = "target_group") <- name[x]
      attr(targs[[x]], which = "target_ids") <- paste(name[x], targs[[x]]$names, sep = "")

      targs[[x]]
    }, targs = new_targets, name = new_names)
    names(new_targets) <- new_names

    # Initialize update information
    update <- rep_len(TRUE, length.out = length(new_targets))
    names(update) <- new_names

    # Store appropriate attributes
    sub_targets <- unlist(lapply(new_targets, FUN = function(x) { x[["names"]] }))
    target_ids <- paste(new_names, sub_targets, sep = "")
    attributes(new_targets)$update <- update
    attributes(new_targets)$target_groups <- new_names
    attributes(new_targets)$sub_targets <- sub_targets
    attributes(new_targets)$target_ids <- target_ids

    # Check all names are unique
    stopifnot(
      "Target names must be unique by group and target" = length(target_ids) == length(unique(target_ids))
    )
  } # length(new_targets) > 0

  # If reading from a previous set of results
  old_targets <- list()
  if (!is.null(previous_run_targets)) {
    stop("NEED TO UPDATE")
    # Main target information columns
    main_cols <- c("MainTarget", "update")
    # Subtarget information columns
    info_cols <- colnames(previous_run_targets)[!colnames(previous_run_targets) %in% main_cols]

    # Unique main targets from last run
    mains <- unique(previous_run_targets[ , main_cols])
    # sub targets from last run
    infos <- previous_run_targets[ , info_cols]

    # For each main target
    for (i1 in 1:nrow(mains)) {
      # Find appropriate subtargets
      idx <- which(previous_run_targets$MainTarget == mains$MainTarget[i1])
      # Convert info to list form
      old_targets[[i1]] <- as.list(infos[idx, ])

      # apply subtarget names to sub elements of target list
      sub_names <- infos$names[idx]
      old_targets[[i1]] <- lapply(old_targets[[i1]], FUN = function(x, sub_names) {
        names(x) <- sub_names
        x
      }, sub_names = sub_names)
      names(old_targets[[i1]]$names) <- NULL
    }

    # apply main target names to target list
    names(old_targets) <- mains$MainTarget

    # Pull update information
    update <- mains$update
    names(update) <- mains$MainTarget

    # Store appropriate attributes
    attributes(old_targets)$update <- update
    attributes(old_targets)$sub_targets <- infos$names

    attributes(new_targets)$target_groups <- target_groups
    attributes(new_targets)$sub_targets <- sub_targets
    attributes(new_targets)$target_ids <- target_ids

  } # !is.null(previous_run_targets)

  # Handle all cases
  if (length(new_targets) == 0 & length(old_targets) == 0) { # No targets added
    stop("Need to add at least some targets.")
  } else if (length(new_targets) != 0 & length(old_targets) == 0) { # Only new targets added
    all_targets <- new_targets
  } else if (length(new_targets) == 0 & length(old_targets) != 0) { # Only old targets added
    stop("NEED TO UPDATE")
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
