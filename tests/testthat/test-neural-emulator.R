# The emulator on its own, without imabc: a 2 -> 2 problem whose answer is known
# exactly, so a bad fit shows up as a prediction error rather than as a strange
# calibration.

skip_if_not_installed("torch")

fast_cfg <- function(...) neural_config(hidden = 64L, depth = 2L, members = 2L, max_epochs = 150L, workers = 2L, ...)

sim_xy <- function(n = 2000, seed = 1) {
  set.seed(seed)
  x <- cbind(x1 = runif(n, 0.4, 0.6), x2 = runif(n, 0.8, 1.2))
  list(x = x, y = cbind(Y1 = x[, 1] + x[, 2], Y2 = x[, 1] * x[, 2]))
}

truth <- function(x) cbind(Y1 = x[, 1] + x[, 2], Y2 = x[, 1] * x[, 2])

new_x <- cbind(x1 = c(0.45, 0.5, 0.55), x2 = c(1.1, 1.0, 0.9))

test_that("the ensemble recovers a known function", {
  d <- sim_xy()
  emulator <- neural_fit(d$x, d$y, fast_cfg(), seeds = 1:2)

  expect_s3_class(emulator, "neural_emulator")
  expect_equal(emulator$n_train, 2000)
  expect_lt(max(emulator$val_mse), 0.01)
  expect_lt(max(abs(as.matrix(predict(emulator, new_x)) / truth(new_x) - 1)), 0.01)
})

test_that("predict returns a data frame of named targets, and the members behind it", {
  d <- sim_xy()
  emulator <- neural_fit(d$x, d$y, fast_cfg(), seeds = 1:2)
  pred <- predict(emulator, new_x)

  # imabc reorders the backend's result by name, which silently drops every
  # column if it is handed a data.table instead of a data frame
  expect_s3_class(pred, "data.frame")
  expect_false(inherits(pred, "data.table"))
  expect_identical(names(pred), c("Y1", "Y2"))
  expect_identical(nrow(pred), 3L)

  members <- predict(emulator, new_x, type = "members")
  expect_length(members, 2)
  expect_equal(Reduce(`+`, members) / 2, as.matrix(pred), ignore_attr = TRUE)
})

test_that("parameter columns are matched by name, not by position", {
  d <- sim_xy()
  emulator <- neural_fit(d$x, d$y, fast_cfg(), seeds = 1:2)

  expect_equal(predict(emulator, new_x), predict(emulator, new_x[, c("x2", "x1")]))
})

test_that("missing targets are masked out of the loss rather than fitted", {
  d <- sim_xy()
  gappy <- d$y
  set.seed(2)
  gappy[sample.int(nrow(gappy), 0.4 * nrow(gappy)), "Y2"] <- NA

  masked <- predict(neural_fit(d$x, gappy, fast_cfg(na = "mask"), seeds = 1:2), new_x)
  dropped <- predict(neural_fit(d$x, gappy, fast_cfg(na = "drop"), seeds = 1:2), new_x)

  expect_lt(max(abs(as.matrix(masked) / truth(new_x) - 1)), 0.02)
  expect_lt(max(abs(as.matrix(dropped) / truth(new_x) - 1)), 0.02)
})

test_that("reading a missing target as zero moves the fit, as that setting implies", {
  d <- sim_xy()
  gappy <- d$y
  set.seed(2)
  gappy[sample.int(nrow(gappy), 0.4 * nrow(gappy)), "Y2"] <- NA
  zeroed <- predict(neural_fit(d$x, gappy, fast_cfg(na = "zero"), seeds = 1:2), new_x)

  # 40% of the Y2 column is now a false zero, so the fit must be pulled toward
  # it: this is a domain choice, right only when a gap really does mean zero
  expect_lt(max(zeroed$Y2), 0.4)
})

test_that("a target no draw ever reported is an error, not a NaN fit", {
  d <- sim_xy()
  blank <- d$y
  blank[, "Y2"] <- NA

  expect_error(neural_fit(d$x, blank, fast_cfg(), seeds = 1:2), "no draw reported a value for Y2")
})

test_that("a parameter that never varies does not break standardising", {
  d <- sim_xy()
  fixed <- cbind(d$x, fixed = 3)
  emulator <- neural_fit(fixed, d$y, fast_cfg(), seeds = 1:2)

  pred <- predict(emulator, cbind(new_x, fixed = 3))
  expect_true(all(is.finite(as.matrix(pred))))
  expect_lt(max(abs(as.matrix(pred) / truth(new_x) - 1)), 0.01)
})

test_that("a saved emulator predicts exactly as the one in memory did", {
  d <- sim_xy()
  emulator <- neural_fit(d$x, d$y, fast_cfg(), seeds = 1:2)
  path <- withr::local_tempfile(fileext = ".pt")
  neural_write(emulator, path)

  expect_equal(predict(neural_read(path), new_x), predict(emulator, new_x))
})

test_that("the development option overrides the defaults", {
  withr::local_options(imabc.neural_config = list(members = 7L))

  expect_equal(neural_config()$members, 7L)
  expect_equal(neural_config(members = 2L)$members, 2L)
  expect_error(neural_config(na = "nonsense"))
})
