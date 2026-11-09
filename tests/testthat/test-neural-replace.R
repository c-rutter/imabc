# emulator = "replace" end to end on the same two-parameter problem the other
# example tests use, where x1 = 0.5 and x2 = 1 are the values being recovered.

skip_if_not_installed("torch")

# Small enough to keep the suite quick; min_rows is lowered because N_start here
# is 2000 rather than the tens of thousands a real model would need.
use_fast_emulator <- function() {
  withr::local_options(
    imabc.neural_config = list(
      hidden = 64L, depth = 2L, members = 2L, max_epochs = 150L, workers = 2L, min_rows = 100L
    ),
    .local_envir = parent.frame()
  )
}

toy_priors <- function() {
  define_priors(
    x1 = add_prior(parameter_name = "x1", dist_base_name = "unif", min = 0.4, max = 0.6),
    x2 = add_prior(parameter_name = "x2", dist_base_name = "unif", min = 0.8, max = 1.2)
  )
}

toy_targets <- function() {
  define_targets(G1 = group_targets(
    Y1 = add_target(
      scale = 1, target = 1.5,
      starting_range = c(0.015, 2.985), stopping_range = c(1.47, 1.53)
    ),
    Y2 = add_target(
      scale = 1, target = 0.5,
      starting_range = c(0.005, 0.995), stopping_range = c(0.49, 0.51)
    )
  ))
}

toy_model <- function(counter = NULL) {
  function(x1, x2) {
    if (!is.null(counter)) counter$n <- counter$n + 1L
    c(Y1 = x1 + x2, Y2 = x1 * x2)
  }
}

run_toy <- function(target_fun, priors, targets, ...) {
  imabc(
    target_fun = target_fun, priors = priors, targets = targets,
    seed = 54321, N_start = 2000, max_iter = 100, N_centers = 2, Center_n = 500,
    N_cov_points = 50, N_post = 100, improve_method = "percentile", verbose = FALSE, ...
  )
}

# The weighted mean of the accepted draws is the vector a user would report.
posterior_mean <- function(res, parms = c("x1", "x2")) {
  w <- res$good_parm_draws$sample_wt
  colSums(as.matrix(res$good_parm_draws[, parms, with = FALSE]) * w) / sum(w)
}

inside_stopping_bounds <- function(targets, sim) {
  nm <- names(targets$stopping_lower_bounds)
  sim[nm] >= targets$stopping_lower_bounds[nm] & sim[nm] <= targets$stopping_upper_bounds[nm]
}

test_that("replace mode calibrates, and runs the model for the first iteration only", {
  use_fast_emulator()
  counter <- new.env()
  counter$n <- 0L
  priors <- toy_priors()
  targets <- toy_targets()
  model <- toy_model(counter)
  target_fun <- define_target_function(targets, priors, FUN = model, use_seed = FALSE)

  res <- run_toy(target_fun, priors, targets, emulator = "replace")

  expect_equal(counter$n, 2000)
  expect_equal(res$neural$mode, "replace")
  expect_equal(res$neural$n_train, 2000)
  expect_true(nrow(res$good_parm_draws) > 0)

  # Every tolerance interval reached its stopping bounds, i.e. the run converged
  expect_equal(res$targets$current_lower_bounds, res$targets$stopping_lower_bounds)
  expect_equal(res$targets$current_upper_bounds, res$targets$stopping_upper_bounds)

  # The emulator is a stand-in for the model, so the posterior is only as good
  # as what the real model says at the answer it produced
  theta <- posterior_mean(res)
  checks <- replicate(5, inside_stopping_bounds(targets, model(theta[["x1"]], theta[["x2"]])))
  expect_true(all(checks))
})

test_that("the result keeps everything a run without an emulator returns", {
  use_fast_emulator()
  priors <- toy_priors()
  targets <- toy_targets()
  target_fun <- define_target_function(targets, priors, FUN = toy_model(), use_seed = FALSE)

  plain <- run_toy(target_fun, priors, targets)
  with_emulator <- run_toy(target_fun, priors, targets, emulator = "replace")

  expect_equal(setdiff(names(with_emulator), names(plain)), "neural")
  expect_equal(setdiff(names(plain), names(with_emulator)), character(0))
  for (element in names(plain)) {
    if (is.data.frame(plain[[element]])) {
      expect_identical(names(with_emulator[[element]]), names(plain[[element]]), info = element)
    }
  }
})

test_that("the model backend a user supplies is the one that runs the model iteration", {
  use_fast_emulator()
  used <- new.env()
  used$n <- 0L
  priors <- toy_priors()
  targets <- toy_targets()
  target_fun <- define_target_function(targets, priors, FUN = toy_model(), use_seed = FALSE)

  my_backend <- function(parms_to_run, all_parm_names, target_fun, other_inputs) {
    used$n <- used$n + 1L
    rows <- lapply(seq_len(nrow(parms_to_run)), function(i) {
      target_fun(
        unlist(parms_to_run[i, all_parm_names, with = FALSE]),
        targets = other_inputs$targets, priors = other_inputs$priors
      )
    })
    as.data.frame(do.call(rbind, rows))
  }

  res <- run_toy(target_fun, priors, targets, emulator = "replace", backend_fun = my_backend)

  expect_equal(used$n, 1L)
  expect_equal(res$neural$n_train, 2000)
})

test_that("the emulator is saved alongside the other run outputs", {
  use_fast_emulator()
  priors <- toy_priors()
  targets <- toy_targets()
  target_fun <- define_target_function(targets, priors, FUN = toy_model(), use_seed = FALSE)
  out_dir <- withr::local_tempdir()

  res <- run_toy(
    target_fun, priors, targets,
    emulator = "replace", output_directory = out_dir, output_tag = "test"
  )

  expect_true(file.exists(res$neural$file))
  expect_equal(basename(res$neural$file), "Emulator_test.pt")
  expect_equal(
    predict(neural_read(res$neural$file), cbind(x1 = 0.5, x2 = 1)),
    predict(res$neural$emulator, cbind(x1 = 0.5, x2 = 1))
  )
})

test_that("too few starting draws to train on is reported as such", {
  withr::local_options(imabc.neural_config = list(members = 2L, min_rows = 5000L))
  priors <- toy_priors()
  targets <- toy_targets()
  target_fun <- define_target_function(targets, priors, FUN = toy_model(), use_seed = FALSE)

  expect_error(
    run_toy(target_fun, priors, targets, emulator = "replace"),
    "needs at least 5000 draws"
  )
})

test_that("continuing a previous run with an emulator is refused", {
  priors <- toy_priors()
  targets <- toy_targets()
  target_fun <- define_target_function(targets, priors, FUN = toy_model(), use_seed = FALSE)

  expect_error(
    imabc(
      target_fun = target_fun, priors = priors, targets = targets,
      emulator = "replace", previous_results_dir = withr::local_tempdir()
    ),
    "cannot continue a previous run"
  )
})
