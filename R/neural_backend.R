# Wiring between imabc's main loop and the emulator.
#
# imabc evaluates a whole batch of draws through backend_fun, in the calling
# process, so a closure over one environment can hold the emulator across
# iterations and the main loop needs no emulator-specific code. In "replace"
# mode the first batch is run by the model and becomes the training set; every
# later batch is a forward pass.

#' Set up emulator state for a run
#'
#' @param mode character(1). The `emulator` argument of `imabc()`.
#' @param backend_fun function or NULL. The user's own way of running the real
#' model, kept here because the model iteration still needs it.
#' @param previous_results_dir character(1) or NULL. Continuing a run is
#' rejected: the emulator is not part of the saved run state.
#' @param verbose logical(1).
#'
#' @return An environment holding the settings, the training data and, once
#' trained, the emulator.
#'
#' @noRd
neural_init <- function(mode, backend_fun, previous_results_dir, verbose) {
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("emulator = \"", mode, "\" requires the torch package: install.packages(\"torch\")", call. = FALSE)
  }
  if (!is.null(previous_results_dir)) {
    stop("emulator modes cannot continue a previous run: the emulator is not part of the saved run state.", call. = FALSE)
  }
  list2env(
    list(
      mode = mode, cfg = neural_config(), user_backend = backend_fun,
      X = NULL, Y = NULL, emulator = NULL, verbose = verbose
    ),
    new.env(parent = emptyenv())
  )
}

#' Build the backend imabc evaluates draws through
#'
#' @param neural environment from `neural_init()`.
#'
#' @return A function with the `backend_fun` signature: it runs the model until
#' the emulator exists, then predicts.
#'
#' @noRd
neural_backend <- function(neural) {
  function(parms_to_run, all_parm_names, target_fun, other_inputs) {
    th <- as.matrix(parms_to_run[, all_parm_names, with = FALSE])
    if (!is.null(neural$emulator)) {
      return(stats::predict(neural$emulator, th))
    }
    # run_handler does the dispatching, so the user's own backend is honoured
    # for the model iteration and NULL still means the default foreach path
    res <- do.call(run_handler, c(
      list(
        parms_to_run = parms_to_run, all_parm_names = all_parm_names, target_fun = target_fun,
        custom_function = neural$user_backend, verbose = neural$verbose
      ),
      other_inputs
    ))
    neural$X <- th
    neural$Y <- as.matrix(as.data.frame(res))
    neural$emulator <- neural_train(neural)
    res
  }
}

#' Draw member seeds from the run's RNG stream
#'
#' Taking them from imabc's own stream means the `seed` argument reproduces the
#' emulator along with the rest of the run.
#'
#' @param n integer(1). Number of seeds.
#'
#' @return An integer vector.
#'
#' @noRd
neural_seeds <- function(n) sample.int(.Machine$integer.max, n)

#' Train the emulator on the draws the model has simulated
#'
#' @param neural environment from `neural_init()`, with `X` and `Y` set.
#'
#' @return An object of class `neural_emulator`.
#'
#' @noRd
neural_train <- function(neural) {
  cfg <- neural$cfg
  if (nrow(neural$X) < cfg$min_rows) {
    stop(sprintf(
      "the emulator needs at least %d draws to train on, iteration 1 simulated %d: raise N_start",
      cfg$min_rows, nrow(neural$X)
    ), call. = FALSE)
  }
  if (neural$verbose) {
    cat(sprintf(
      "\nTraining emulator: %d draws x %d targets, %d members\n",
      nrow(neural$X), ncol(neural$Y), cfg$members
    ))
  }
  emulator <- neural_fit(neural$X, neural$Y, cfg, neural_seeds(cfg$members))
  if (neural$verbose) {
    cat(sprintf(
      "Emulator holdout MSE (standardised): %s\n",
      paste(sprintf("%.5f", emulator$val_mse), collapse = ", ")
    ))
  }
  emulator
}

#' Collect what the run should report about the emulator
#'
#' @param neural environment from `neural_init()`.
#' @param output_directory character(1) or NULL. The emulator is written here
#' when a directory is given, so a finished run can be re-examined.
#' @param output_tag character(1) or NULL. Tag used for the file name.
#'
#' @return A list for the `neural` element of the `imabc()` result, or NULL if
#' the run ended before any draw was simulated.
#'
#' @noRd
neural_result <- function(neural, output_directory, output_tag) {
  emulator <- neural$emulator
  if (is.null(emulator)) {
    return(NULL)
  }
  file <- NULL
  if (!is.null(output_directory)) {
    file <- file.path(output_directory, ifelse(
      is.null(output_tag), "Emulator.pt", sprintf("Emulator_%s.pt", output_tag)
    ))
    neural_write(emulator, file)
  }
  list(
    mode = neural$mode, emulator = emulator, val_mse = emulator$val_mse,
    n_train = emulator$n_train, file = file
  )
}
