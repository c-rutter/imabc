# An ensemble of standardised MLPs used as a surrogate for an expensive target
# function. The recipe (256 x 3 SiLU, Adam with reduce-on-plateau, early stopped
# on a holdout, members averaged) is the one validated against CRC-SPIN in
# neuralsbi-crcspin, so changes to it stay comparable to that benchmark.
#
# torch is a Suggests dependency, so every torch call here is qualified: nothing
# in the package attaches it, and the training workers only need it installed.

#' Emulator settings
#'
#' Defaults for emulator training and prediction. None of these is an imabc
#' argument yet; `options(imabc.neural_config = list(...))` is the development
#' override the calibration experiments use to test alternatives.
#'
#' @param ... Named settings that override the defaults.
#'
#' @return A list of settings.
#'
#' @noRd
neural_config <- function(...) {
  cfg <- list(
    hidden = 256L, depth = 3L, members = 4L,
    lr = 1e-3, batch = 1024L, max_epochs = 200L, patience = 20L, lr_patience = 5L,
    holdout = 0.1, holdout_max = 1e4, workers = 4L, threads = 1L,
    na = "mask", min_rows = 500L
  )
  cfg <- utils::modifyList(cfg, getOption("imabc.neural_config", list()))
  cfg <- utils::modifyList(cfg, list(...))
  cfg$na <- match.arg(cfg$na, c("mask", "zero", "drop"))
  cfg
}

#' Build an untrained multilayer perceptron
#'
#' @param n_in,n_out integer(1). Number of parameters in and targets out.
#' @param cfg list. Settings from `neural_config()`.
#'
#' @return A torch `nn_sequential` module.
#'
#' @noRd
neural_make_net <- function(n_in, n_out, cfg) {
  layers <- list()
  h <- n_in
  for (i in seq_len(cfg$depth)) {
    layers <- c(layers, list(torch::nn_linear(h, cfg$hidden), torch::nn_silu()))
    h <- cfg$hidden
  }
  do.call(torch::nn_sequential, c(layers, list(torch::nn_linear(h, n_out))))
}

#' Column means and standard deviations for standardising
#'
#' Fixed parameters and targets that never vary give a zero standard deviation,
#' which would make the standardised column infinite, so those are left on their
#' original scale.
#'
#' @param m numeric matrix. Missing values are ignored.
#'
#' @return A list with elements `mu` and `sd`, one value per column.
#'
#' @noRd
neural_col_stats <- function(m) {
  sd <- apply(m, 2, stats::sd, na.rm = TRUE)
  sd[!is.finite(sd) | sd < 1e-12] <- 1
  list(mu = colMeans(m, na.rm = TRUE), sd = sd)
}

#' Train one ensemble member
#'
#' A standardised regression from parameters to targets, early stopped on a
#' holdout split of its own. The member is returned as plain R arrays rather
#' than a live torch module so that it can cross a worker boundary and be saved.
#'
#' @param X,Y numeric matrices of parameters and targets, one row per draw.
#' @param cfg list. Settings from `neural_config()`.
#' @param seed integer(1). Drives the holdout split and the initial weights.
#'
#' @return A list with the state dictionary, the input and output scalers, the
#' best holdout mean squared error and the number of epochs run.
#'
#' @noRd
neural_fit_member <- function(X, Y, cfg, seed) {
  set.seed(seed)
  torch::torch_manual_seed(seed)
  n <- nrow(X)
  nv <- max(1L, min(as.integer(cfg$holdout_max), as.integer(cfg$holdout * n), n - 1L))
  perm <- sample.int(n)
  vi <- perm[seq_len(nv)]
  ti <- perm[-seq_len(nv)]

  x_s <- neural_col_stats(X[ti, , drop = FALSE])
  y_s <- neural_col_stats(Y[ti, , drop = FALSE])
  f32 <- function(m) torch::torch_tensor(m, dtype = torch::torch_float())
  std_x <- function(r) f32(scale(X[r, , drop = FALSE], x_s$mu, x_s$sd))
  # The mask zeroes an unobserved target's contribution to the loss, so a row
  # with only some targets reported still trains the rest. With no missing
  # values it reduces to the plain mean squared error.
  std_y <- function(r) {
    z <- scale(Y[r, , drop = FALSE], y_s$mu, y_s$sd)
    keep <- !is.na(z)
    z[!keep] <- 0
    list(y = f32(z), mask = f32(keep * 1))
  }
  tr_x <- std_x(ti)
  tr_y <- std_y(ti)
  va_x <- std_x(vi)
  va_y <- std_y(vi)
  masked_mse <- function(pred, y, mask) {
    torch::torch_sum((pred - y)^2 * mask) / torch::torch_sum(mask)
  }

  net <- neural_make_net(ncol(X), ncol(Y), cfg)
  opt <- torch::optim_adam(net$parameters, lr = cfg$lr)
  sched <- torch::lr_reduce_on_plateau(opt, factor = 0.5, patience = cfg$lr_patience)
  best <- Inf
  best_state <- NULL
  bad <- 0L
  n_tr <- length(ti)
  for (ep in seq_len(cfg$max_epochs)) {
    net$train()
    idx <- torch::torch_randperm(n_tr, dtype = torch::torch_long()) + 1L
    for (k in seq(1L, n_tr, by = cfg$batch)) {
      b <- idx[k:min(k + cfg$batch - 1L, n_tr)]
      opt$zero_grad()
      loss <- masked_mse(net(tr_x[b, ]), tr_y$y[b, ], tr_y$mask[b, ])
      loss$backward()
      opt$step()
    }
    net$eval()
    vl <- torch::with_no_grad(as.numeric(masked_mse(net(va_x), va_y$y, va_y$mask)))
    sched$step(vl)
    if (vl < best - 1e-6) {
      best <- vl
      bad <- 0L
      best_state <- lapply(net$state_dict(), function(z) z$clone())
    } else {
      bad <- bad + 1L
    }
    if (bad >= cfg$patience) break
  }
  net$load_state_dict(best_state)

  list(state = lapply(net$state_dict(), as.array), x = x_s, y = y_s, val_mse = best, epochs = ep)
}

#' Train every ensemble member
#'
#' Members differ only by seed, and a 256-wide net at batch 1024 is bound by
#' per-op overhead rather than flops, so one process per member with a single
#' torch thread each is far faster than one process with many threads.
#'
#' @param X,Y numeric matrices of parameters and targets, one row per draw.
#' @param cfg list. Settings from `neural_config()`.
#' @param seeds integer. One seed per member.
#'
#' @return A list of members, see `neural_fit_member()`.
#'
#' @noRd
neural_fit_members <- function(X, Y, cfg, seeds) {
  # The worker gets this environment and nothing else, so the functions are
  # rebound into it: their package namespace would not survive serialisation.
  env <- list2env(
    list(
      X = X, Y = Y, cfg = cfg, seeds = seeds,
      neural_make_net = neural_make_net, neural_col_stats = neural_col_stats,
      neural_fit_member = neural_fit_member
    ),
    new.env(parent = globalenv())
  )
  for (f in c("neural_make_net", "neural_col_stats", "neural_fit_member")) {
    environment(env[[f]]) <- env
  }
  fit_one <- function(m) neural_fit_member(X, Y, cfg, seeds[m])
  set_threads <- function(n) {
    torch::torch_set_num_threads(n)
    NULL
  }
  environment(fit_one) <- env
  environment(set_threads) <- globalenv()

  n_workers <- max(1L, min(as.integer(cfg$workers), cfg$members))
  if (n_workers == 1L) {
    return(lapply(seq_len(cfg$members), fit_one))
  }
  cl <- parallel::makePSOCKcluster(n_workers)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterCall(cl, set_threads, cfg$threads)
  parallel::parLapply(cl, seq_len(cfg$members), fit_one)
}

#' Fit an ensemble of neural emulators
#'
#' @param X numeric matrix of parameter values, one row per draw.
#' @param Y numeric matrix of target values, one row per draw. Missing values
#' are handled according to `cfg$na`: masked out of the loss, read as exact
#' zeroes, or their rows dropped.
#' @param cfg list. Settings from `neural_config()`.
#' @param seeds integer. One seed per member.
#'
#' @return An object of class `neural_emulator`.
#'
#' @noRd
neural_fit <- function(X, Y, cfg = neural_config(), seeds = seq_len(cfg$members)) {
  stopifnot(nrow(X) == nrow(Y), length(seeds) == cfg$members)
  blank <- colSums(!is.na(Y)) == 0
  if (any(blank)) {
    stop(
      "cannot train an emulator: no draw reported a value for ",
      paste(colnames(Y)[blank], collapse = ", "),
      call. = FALSE
    )
  }
  if (cfg$na == "zero") {
    Y[is.na(Y)] <- 0
  } else if (cfg$na == "drop") {
    keep <- stats::complete.cases(Y)
    X <- X[keep, , drop = FALSE]
    Y <- Y[keep, , drop = FALSE]
  }
  fits <- neural_fit_members(X, Y, cfg, seeds)

  structure(
    list(
      members = fits,
      parm_names = colnames(X),
      target_names = colnames(Y),
      val_mse = vapply(fits, function(f) f$val_mse, numeric(1)),
      epochs = vapply(fits, function(f) f$epochs, numeric(1)),
      n_train = nrow(X),
      cfg = cfg,
      cache = new.env(parent = emptyenv())
    ),
    class = "neural_emulator"
  )
}

#' Per-member prediction functions
#'
#' Rebuilding a net from its arrays costs more than a forward pass does, so the
#' predictors are built once and kept in the object's cache environment.
#'
#' @param object a `neural_emulator`.
#'
#' @return A list of functions, each mapping a parameter matrix to a matrix of
#' targets on their original scale.
#'
#' @noRd
neural_nets <- function(object) {
  if (is.null(object$cache$nets)) {
    object$cache$nets <- lapply(object$members, function(b) {
      net <- neural_make_net(length(b$x$mu), length(b$y$mu), object$cfg)
      net$load_state_dict(lapply(b$state, function(z) torch::torch_tensor(z, dtype = torch::torch_float())))
      net$eval()
      function(th) {
        z <- torch::with_no_grad(as.matrix(net(
          torch::torch_tensor(scale(th, b$x$mu, b$x$sd), dtype = torch::torch_float())
        )))
        sweep(sweep(z, 2, b$y$sd, "*"), 2, b$y$mu, "+")
      }
    })
  }
  object$cache$nets
}

#' Predict target values from a neural emulator
#'
#' @param object a `neural_emulator`, as returned in the `neural` element of an
#' `imabc()` run with `emulator != "none"`.
#' @param newdata matrix or data frame of parameter values, one row per draw.
#' Columns are matched by name when both sides are named.
#' @param type "mean" returns the ensemble mean, the predictor the calibration
#' itself uses. "members" returns each member's prediction, which shows how far
#' the members disagree and so where the emulator is least trustworthy.
#' @param ... Ignored.
#'
#' @return A data frame of predicted targets for `type = "mean"`, or a list of
#' matrices for `type = "members"`.
#'
#' @export
predict.neural_emulator <- function(object, newdata, type = c("mean", "members"), ...) {
  type <- match.arg(type)
  th <- as.matrix(newdata)
  if (!is.null(object$parm_names) && !is.null(colnames(th))) {
    th <- th[, object$parm_names, drop = FALSE]
  }
  preds <- lapply(neural_nets(object), function(f) f(th))
  if (type == "members") {
    return(preds)
  }
  out <- as.data.frame(Reduce(`+`, preds) / length(preds))
  names(out) <- object$target_names
  out
}

#' Save a fitted emulator
#'
#' @param x a `neural_emulator`.
#' @param path character(1). File to write.
#'
#' @return `path`, invisibly.
#'
#' @noRd
neural_write <- function(x, path) {
  torch::torch_save(x[setdiff(names(x), "cache")], path)
  invisible(path)
}

#' Read a saved emulator
#'
#' @param path character(1). File written by `neural_write()`.
#'
#' @return An object of class `neural_emulator`.
#'
#' @noRd
neural_read <- function(path) {
  x <- torch::torch_load(path)
  x$cache <- new.env(parent = emptyenv())
  structure(x, class = "neural_emulator")
}
