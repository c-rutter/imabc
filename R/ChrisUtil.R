add_prior <- function(..., FUN = NULL, use_length = TRUE) { # New
  dots <- list(...)

  # if FUN is NULL, assume a fixed parameter
  if (is.null(FUN)) {
    if (length(dots) != 1) {
      stop("For a Fixed Parameter only supply a constant to the prior function")
    }
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        res <- rep(as.numeric(dots), length(n))
      } else {
        res <- rep(as.numeric(dots), n)
      }

      return(res)
    }
    attributes(f)$fixed <- TRUE
  } else {
    f <- function(n, .use_length = use_length) {
      if (.use_length) {
        n <- length(n)
        res <- do.call(match.fun(FUN), c(n, dots))
      } else {
        res <- mapply(match.fun(FUN), n, MoreArgs = dots)
      }

      return(res)
    }

    attributes(f)$fixed <- FALSE
  }

  return(f)
}

init_run_df <- function(n, parms, type) { # same as init.draws
  if (type == "draw") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      seed = rep(NA_character_, n),
      matrix(NA_real_, n, length(parms)),
      scaled_dist = rep(NA_real_, n),
      sample_wt = rep(NA_real_, n)
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  } else if (type == "distance") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      matrix(NA_real_, n, length(parms)),
      tot_dist = rep(NA_real_, n),
      n_good = rep.int(0, n)
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  } else if (type == "sim") {
    df <- data.frame(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      matrix(NA_real_, n, length(parms))
    )
    pcol <- grep("X[0-9]+", colnames(df))
    colnames(df)[pcol] <- parms
  }

  return(df)
}

# CM NOTE: This is needed for R to recognize data.table for some reason. I think this is only when using
#   devtools::load_all() to load functions but once we install the package and import/depend on data.table it should
#   be fine to remove this. Unsure though.
.datatable.aware = TRUE
init_run_dt <- function(n, parms, type) { # same as init.draws
  if (type == "draw") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n),
      seed = rep(NA_character_, n)
    )
    df[, (parms) := NA_real_]
    df[, scaled_dist := NA_real_]
    df[, sample_wt := rep.int(1, n)]

  } else if (type == "distance") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n)
    )
    df[, (parms) := NA_real_]
    df[, tot_dist := NA_real_]
    df[, n_good := rep.int(0, n)]

  } else if (type == "sim") {
    df <- data.table(
      iter = rep.int(1, n),
      draw = 1:n,
      step = rep.int(0, n)
    )
    df[, (parms) := NA_real_]
  }

  return(df)
}

seed_stream <- function(seed_stream_start, length.out) { # same as get.random.seed.strings.R
  # Initialize vector
  stream <- rep(NA_character_, length.out)

  # Loop over vector to get
  for (i1 in 1:length.out) {
    seed_stream_start <- parallel::nextRNGStream(seed_stream_start)
    stream[i1] <- paste0(seed_stream_start, collapse = "_")
  }

  return(stream)
}

parms_from_priors <- function(parm_df, name_parms, prior_list, sampling) {
  parm_df[, (name_parms) := lapply(name_parms, FUN = function(x, prior_funcs, df) {
    if (attributes(prior_funcs[[x]])$fixed) {
      prior_funcs[[x]](nrow(df))
    } else {
      prior_funcs[[x]](df[, which(name_parms %in% x)])
    }
  }, prior_funcs = prior_list, df = sampling)]

  return(parm_df)
}

combine_results <- function(list1, list2) {
  # Returned sub-elements
  stored_items <- names(list1)

  # Join sub-elements together
  items <- list()
  for (i1 in stored_items) {
    items[[i1]] <- as.data.frame(rbind(list1[[i1]], list2[[i1]]))
  }

  return(items)
}

in_range <- function(target, low, high) {

  return(low < target & target < high)
}

eval_targets <- function(dist_target, target_names, target_list) {
  dist_target[, (target_names) := lapply(target_names, FUN = function(x, target_list, df) {
    y <- df[[x]]
    check <- in_range(y, target_list[[x]]$low_bound_start, target_list[[x]]$up_bound_start)
    y <- y*(check*2 - 1)
    y
  }, target_list = target_list, df = dist_target)]

  return(dist_target)
}
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
