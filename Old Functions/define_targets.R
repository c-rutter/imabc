#' @export
define_targets <- function(..., previous_run_targets = NULL) {
  # Newly added targets
  new_targets <- list(...)
  if (length(new_targets) > 0) {
    # Find grouped targets
    is_grouped <- sapply(new_targets, function(x) inherits(x, "group"))

    # Create artificial groups for individual targets
    grouped_targets <- new_targets
    grouped_targets[!is_grouped] <- lapply(new_targets[!is_grouped], function(x) {
      x$group_name <- NA
      attr(x, "class") <- c("target", "group", "imabc")
      x
    })
    grouped_targets <- structure(grouped_targets, class = "groups")

    # Need to get the number of targets in a group
    n_targets <- rep.int(1, length(new_targets))
    if (sum(is_grouped) > 0) {
      n_targets[is_grouped] <- sapply(grouped_targets[is_grouped], function(x) length(x) - 1)
    }

    # pull the names of groups if they exist
    group_names <- get_list_element(new_targets, "group_name", unlist = TRUE)

    # generate unique group names
    group_names <- unique_names(grouped_targets, group_names)
    group_names[!is_grouped] <- NA
    target_groups <- rep(group_names, times = n_targets)
    is_grouped <- rep(is_grouped, times = n_targets)

    # unlist grouped targets so each element of the list is a target
    added_targets <- structure(list(), class = "targets")
    i_loc <- 1
    for (i1 in seq_along(new_targets)) {
      if (inherits(new_targets[[i1]], "group")) {
        to_add <- c(new_targets[[i1]])
        to_add$group_name <- NULL
        added_targets[i_loc:(i_loc + n_targets[i1] - 1)] <- to_add
      } else {
        t_name <- names(new_targets[i1])
        to_add <- new_targets[[i1]]
        to_add$target_name <- ifelse(
          (is.na(to_add$target_name) || is.null(to_add$target_name)) && t_name != "", t_name, ifelse(
            is.null(to_add$target_name), NA, to_add$target_name
          ))
        added_targets[[i_loc]] <- to_add
      }
      i_loc <- i_loc + n_targets[i1]
    }

    # Give targets their group attribute
    attr(added_targets, "groups") <- target_groups
    attr(added_targets, "grouped") <- is_grouped

    # pull target names if they exist
    name_vec <- get_list_element(added_targets, "target_name", unlist = TRUE)

    # generate unique target names
    target_names <- unique_names(added_targets, name_vec)

    # Update target groups for individual targets
    target_groups[!is_grouped] <- target_names[!is_grouped]

    # Pull all relevant information from targets list
    targets <- get_list_element(added_targets, "target", unlist = TRUE)
    current_lower_bounds <- get_list_element(added_targets, "current_lower_bound", unlist = TRUE)
    current_upper_bounds <- get_list_element(added_targets, "current_upper_bound", unlist = TRUE)
    stopping_lower_bounds <- get_list_element(added_targets, "stopping_lower_bound", unlist = TRUE)
    stopping_upper_bounds <- get_list_element(added_targets, "stopping_upper_bound", unlist = TRUE)
    FUNS <- get_list_element(added_targets, "FUN", unlist = TRUE)
    names(targets) <- target_names
    names(is_grouped) <- target_names
    names(target_groups) <- target_names
    names(current_lower_bounds) <- target_names
    names(current_upper_bounds) <- target_names
    names(stopping_lower_bounds) <- target_names
    names(stopping_upper_bounds) <- target_names
    if (all(is.na(FUNS))) {
      FUNS <- as.null()
    } else {
      PickOut <- which(!is.na(FUNS))
      FUNS <- FUNS[PickOut]
      names(FUNS) <- target_names[PickOut]
    }

    # Initialize update information
    update <- target_groups

    # Rebuild Final targets list
    added_targets <- structure(list(
      targets = targets,
      current_lower_bounds = current_lower_bounds,
      current_upper_bounds = current_upper_bounds,
      stopping_lower_bounds = stopping_lower_bounds,
      stopping_upper_bounds = stopping_upper_bounds,
      target_functions = FUNS
    ), class = c("grouped"[any(is_grouped)], "targets", "imabc"))

    attributes(added_targets)$update <- update
    attributes(added_targets)$grouped_targets <- is_grouped
    attributes(added_targets)$target_groups <- target_groups
    attributes(added_targets)$target_names <- target_names
  } # length(new_targets) > 0

  # If reading from a previous set of results
  old_targets <- list()
  if (!is.null(previous_run_targets)) {
    # Pull Target Specific Information
    previous_run_targets <- previous_run_targets[previous_run_targets$TYPE == "targets", ]
    previous_run_targets$TYPE <- NULL

    # Get target IDS
    target_ids <- unique(previous_run_targets$ID)
    n_targets <- length(target_ids)

    # Initialize target info
    targets <- structure(numeric(n_targets), names = target_ids)
    current_lower_bounds <- structure(numeric(n_targets), names = target_ids)
    current_upper_bounds <- structure(numeric(n_targets), names = target_ids)
    stopping_lower_bounds <- structure(numeric(n_targets), names = target_ids)
    stopping_upper_bounds <- structure(numeric(n_targets), names = target_ids)
    grouped_targets <- structure(logical(n_targets), names = target_ids)
    target_groups <- structure(character(n_targets), names = target_ids)
    update <- structure(character(n_targets), names = target_ids)

    for (i1 in target_ids) {
      targets[i1] <- as.numeric(previous_run_targets$VALUE[previous_run_targets$INFO == "targets" & previous_run_targets$ID == i1])
      current_lower_bounds[i1] <- as.numeric(previous_run_targets$VALUE[previous_run_targets$INFO == "current_lower_bounds" & previous_run_targets$ID == i1])
      current_upper_bounds[i1] <- as.numeric(previous_run_targets$VALUE[previous_run_targets$INFO == "current_upper_bounds" & previous_run_targets$ID == i1])
      stopping_lower_bounds[i1] <- as.numeric(previous_run_targets$VALUE[previous_run_targets$INFO == "stopping_lower_bounds" & previous_run_targets$ID == i1])
      stopping_upper_bounds[i1] <- as.numeric(previous_run_targets$VALUE[previous_run_targets$INFO == "stopping_upper_bounds" & previous_run_targets$ID == i1])
      # Target groups
      if (!is.na(previous_run_targets$VALUE[previous_run_targets$INFO == "target_groups" & previous_run_targets$ID == i1])) {
        target_groups[i1] <- previous_run_targets$VALUE[previous_run_targets$INFO == "target_groups" & previous_run_targets$ID == i1]
        grouped_targets[i1] <- TRUE
      } else {
        target_groups[i1] <- i1
        grouped_targets[i1] <- FALSE
      }
      # Update group
      if (as.logical(previous_run_targets$VALUE[previous_run_targets$INFO == "update" & previous_run_targets$ID == i1])) {
        update[i1] <- target_groups[i1]
      } else {
        update[i1] <- NULL
      }
    }

    # Rebuild Final targets list
    old_targets <- structure(list(
      targets = targets,
      current_lower_bounds = current_lower_bounds,
      current_upper_bounds = current_upper_bounds,
      stopping_lower_bounds = stopping_lower_bounds,
      stopping_upper_bounds = stopping_upper_bounds,
      target_functions = NULL
    ), class = c("grouped"[any(grouped_targets)], "targets", "imabc"))

    attributes(old_targets)$update <- update
    attributes(old_targets)$grouped_targets <- grouped_targets
    attributes(old_targets)$target_groups <- target_groups
    attributes(old_targets)$target_names <- target_ids
  } # !is.null(previous_run_targets)

  # Handle all cases
  if (length(new_targets) == 0 & length(old_targets) == 0) { # No targets added
    stop("Need to add at least one target.")
  } else if (length(new_targets) != 0 & length(old_targets) == 0) { # Only new targets added
    all_targets <- added_targets
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

#' @export
`[.targets` <- function(t, x, groups_given = FALSE) {
  old_class <- class(t)

  if (groups_given) {
    if (is.logical(x)) {
      keep <- rep(x, times = table(attributes(t)$target_groups))
    } else {
      keep <- attributes(t)$target_groups %in% x
    }
  } else {
    if (is.logical(x)) {
      keep <- x
    } else {
      keep <- attributes(t)$target_names %in% x
    }
  }

  # Add subset of target info to a new target lsit
  subset_targets <- structure(list(
    targets = t$targets[keep],
    current_lower_bounds = t$current_lower_bounds[keep],
    current_upper_bounds = t$current_upper_bounds[keep],
    stopping_lower_bounds = t$stopping_lower_bounds[keep],
    stopping_upper_bounds = t$stopping_upper_bounds[keep],
    target_functions = t$target_functions[keep]
  ), class = old_class)
  # If new lower bounds exist add them in
  if (any(c("new_lower_bounds", "new_upper_bounds") %in% names(t))) {
    subset_targets$new_lower_bounds <- t$new_lower_bounds[keep]
    subset_targets$new_upper_bounds <- t$new_upper_bounds[keep]
  }

  # Subset attributes
  grouped_targets <- attributes(t)$grouped_targets[keep]
  target_groups <- attributes(t)$target_groups[keep]
  target_names <- attributes(t)$target_names[keep]
  update <- intersect(attributes(t)$update, target_names)
  # Add them to new target list
  attributes(subset_targets)$update <- update
  attributes(subset_targets)$grouped_targets <- grouped_targets
  attributes(subset_targets)$target_groups <- target_groups
  attributes(subset_targets)$target_names <- target_names

  return(subset_targets)
}

#' @export
print.grouped <- function(t, digits = getOption("digits")) {
  random_string <- "ufkwnbiusheb"
  group_ids <- attr(t, "target_groups")
  group_ids[!attr(t, "grouped_target")] <- paste0(random_string, which(!attr(t, "grouped_target")))

  disp <- t(data.frame(
    t$current_lower_bounds,
    t$stopping_lower_bounds,
    t$targets,
    t$stopping_upper_bounds,
    t$current_upper_bounds
  ))
  disp <- data.frame(
    cols = c("Current Lower Bounds", "Lower Stopping Bounds", "Target", "Upper Stopping Bounds", "Current Upper Bounds"),
    disp
  )

  cols <- split(attr(t, "target_names"), group_ids)
  # Fix order
  cols <- cols[match(unique(group_ids), names(cols))]

  for (i1 in names(cols)) {
    if (!grepl(random_string, i1)) {
      cat(sprintf("Target Group: %s\n", i1))
    } else {
      cat("Single Target\n")
    }

    tmp <- disp[, c("cols", cols[[i1]])]
    colnames(tmp)[1] <- ""
    print(tmp, row.names = FALSE)
    cat("\n")
  }

  invisible(t)
}

#' @export
print.targets <- function(t, digits = getOption("digits")) {
  disp <- t(data.frame(
    t$current_lower_bounds,
    t$stopping_lower_bounds,
    t$targets,
    t$stopping_upper_bounds,
    t$current_upper_bounds
  ))
  disp <- data.frame(
    cols = c("Current Lower Bounds", "Lower Stopping Bounds", "Target", "Upper Stopping Bounds", "Current Upper Bounds"),
    disp
  )
  colnames(disp)[1] <- ""
  print(disp, row.names = FALSE)

  invisible(t)
}

