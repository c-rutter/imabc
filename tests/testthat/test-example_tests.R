# Testing IMABC with a simple test:

# This is a simple test:
# x1 = 0.5
# x2 = 1
# y1 = x1 + x2 = 1.5
# y2 = x1 * x2 = 0.5

# This function creates an example calibration exercise for test purposes.
simple_calibration_test = function(x = c(0.5,1), y = c(0.5,1.5), target_biases = c(0,0), stopping_bounds_margin_of_error = 0.1, initial_bounds_margin_of_error = 0.9, priors_relative_width = 0.1, with_randomness = F, omit.y1 = F, omit.y2 = F, improve_method = "direct", max_iter = 10) {

  # Target eval function:
  fn1 <- function(x1, x2) { x1 + x2 + with_randomness * sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
  fn2 <- function(x1, x2) { x1 * x2 + with_randomness * sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
  fn <- function(x1, x2) {
    res <- c()
    res["Y1"] <- fn1(x1, x2)
    res["Y2"] <- fn2(x1, x2)
    return(res)
  }

  # Priors
  priors <- define_priors(
    # x1: Uniform Prior (from base R)
    x1 = add_prior(
      parameter_name = "x1",
      dist_base_name = "unif",
      min = x[1]*(1-priors_relative_width), max = x[1]*(1+priors_relative_width)
    ),
    # x1: Uniform Prior (from base R)
    x2 = add_prior(
      parameter_name = "x2",
      dist_base_name = "unif",
      min = x[2]*(1-priors_relative_width), max = x[2]*(1+priors_relative_width)
    )
  )

  # Targets:
  targets <- define_targets(
    # G1: Grouped targets include T1 and T2
    G1 = group_targets(
      Y1 = add_target(
        target = y[1] + target_biases[1],
        starting_range = c(y[1]*(1-initial_bounds_margin_of_error), y[1]*(1+initial_bounds_margin_of_error)),
        stopping_range = c(y[1]*(1-stopping_bounds_margin_of_error), y[1]*(1+stopping_bounds_margin_of_error))
      ),
      Y2 = add_target(
        target = y[2] + target_biases[2],
        starting_range = c(y[2]*(1-initial_bounds_margin_of_error), y[2]*(1+initial_bounds_margin_of_error)),
        stopping_range = c(y[2]*(1-stopping_bounds_margin_of_error), y[2]*(1+stopping_bounds_margin_of_error))
                           )
                          )
  )

  # define the target function
  target_fun <- define_target_function(
    targets, priors, FUN = fn, use_seed = FALSE
  )

  # call imabc and return the resulting object
  imabc_results <- imabc(
    improve_method = improve_method,
    priors = priors,
    targets = targets,
    target_fun = target_fun,
    seed = 54321,
    N_start = 2000,
    max_iter = 100,
    N_centers = 2,
    Center_n = 500,
    N_cov_points = 50,
    N_post = 100
  )

}

# Simplest possible test with the direct method:
direct_simple_test = simple_calibration_test(
                                x = c(0.5,1),
                                y = c(1.5,0.5),
                                target_biases = c(0,0),
                                stopping_bounds_margin_of_error = 0.1,
                                initial_bounds_margin_of_error = 0.99,
                                priors_relative_width = 0.2,
                                with_randomness = F,
                                omit.y1 = F,
                                omit.y2 = F,
                                improve_method = "direct",
                                max_iter = 10
)

test_that("simple test with the direct method", {
  expect_s3_class(direct_simple_test, class = "imabc")
})

# Percentile method:
percentile_simple_test = simple_calibration_test(
  x = c(0.5,1),
  y = c(1.5,0.5),
  target_biases = c(0,0),
  stopping_bounds_margin_of_error = 0.1,
  initial_bounds_margin_of_error = 0.99,
  priors_relative_width = 0.2,
  with_randomness = F,
  omit.y1 = F,
  omit.y2 = F,
  improve_method = "percentile",
  max_iter = 10
)

test_that("simple test with the percentile method", {
  expect_s3_class(percentile_simple_test, class = "imabc")
})

