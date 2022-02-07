
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IMABC

<!-- badges: start -->

[![Build & R-CMD
Check](https://github.com/carolyner/imabc/workflows/R-CMD-check/badge.svg)](https://github.com/carolyner/imabc/actions)
<!-- badges: end -->

## Installation

You can install the release version from CRAN with:

``` r
# install.packages("imabc")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("carolyner/imabc")
# library(imabc)
```

Other packages needed are: \* MASS \* data.table \* tidyverse \*
parallel \* foreach \* doParallel \* lhs \* truncnorm

## Running the Code

### Define Model Parameters/Priors

Parameters must have unique names (a unique name will be supplied if not
given)

``` r
priors <- define_priors(
  # x1: Uniform Prior (from base R)
  x1 = add_prior(
    dist_base_name = "unif",
    min = 0.2, max = 0.9
  ),
  # x2: Truncated Normal (from truncnorm package)
  add_prior(
    parameter_name = "x2",
    density_fn = "dtruncnorm",
    mean = 0.5, sd = 0.05, a = 0.4, b = 0.8, # a = min and b = max
    min = 0.4, max = 0.8 # User must specify both in truncnorm
  ),
  # V3: Fixed parameter (not calibrated)
  add_prior(0.5)
)
```

### Define Target Values

Targets must have unique names (a unique name will be supplied if not
given)

``` r
targets <- define_targets(
  # G1: Grouped targets include T1 and T2
  G1 = group_targets(
    T1 = add_target(
      target = 1.5,
      starting_range = c(1.0, 2.0),
      stopping_range = c(1.49, 1.51)
    ),
    add_target(
      target_name = "T2",
      target = 0.5,
      starting_range = c(0.2, 0.9),
      stopping_range = c(0.49, 0.51)
    )
  )
)
```

### Define Target Function

The target function must return a vector whose length is equal to the
number of targets. If the return vector is named using the target names
then the order of the vector will be corrected by `imabc()`. If the
return vector is not named, the targets should be added to the vector in
the same order they were added to the targets object. Finalize the
target function using the define\_target\_function to ensure the
function is specified correctly for `imabc()`.

``` r
fn1 <- function(x1, x2) { x1 + x2 + sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
fn2 <- function(x1, x2) { x1 * x2 + sample(c(-1, 1), 1)*rnorm(1, 0, 0.1) }
fn <- function(x1, x2) {
  res <- c()
  res["T2"] <- fn1(x1, x2)
  res["T1"] <- fn2(x1, x2)
  return(res)
}
target_fun <- define_target_function(
  targets, priors, FUN = fn, use_seed = FALSE
)
```

### Calibrate Model

The primary function is imabc(). The inputs and their descriptions are
as follows:

``` r
calibration_results <- imabc(
  priors = priors,
  targets = targets_nogroup,
  target_fun = target_fun,
  seed = 54321,
  N_start = 2000,
  N_centers = 2,
  Center_n = 500,
  N_cov_points = 50,
  N_post = 100
)
```

-   target\_fun: A function that generate target values given parameters
    (i.e., \`the model’). The use of define\_target\_function is
    stronlgy advised to ensure that the function takes in the correct
    values and correctly returns results.
-   priors: A priors object created using define\_priors. This contains
    information regarding the parameters that are being calibrated. Is
    ignored if starting from previous results.
-   targets: A targets object created using define\_targets. This
    contains information regarding the target values which will be used
    to evaluate simulated parameters. Is ignored if starting from
    previous results.
-   N\_start: numeric(1). The number of draws to simulate for the first
    iteration.
    -   This number should be large relative to the number of parameters
        in order to ensure as much of the parameter space is explored as
        possible. Setting this number too low could result in the model
        getting stuck in a local minima. A good rule of thumb is to set
        this to 1000 x Number of Parameters
-   N\_centers: numeric(1). The number of centers to use for exploring
    the parameter space.
    -   See note with Center\_n.
-   Center\_n: numeric(1). The number of points to add around each
    center
    -   This work in tandem with N\_centers to define the number of
        centers around which new parameter vectors will be generated.
        During each iteration of imabc, the N\_centers best points will
        be selected from the current set of good draws. Then imabc will
        simulate Center\_n draws based on one of the N\_center points.
-   N\_cov\_points: numeric(1). The minimum number of points used to
    estimate the covariance matrix of valid parameters nearest each
    center point. The covariance matrix is used when simulating new
    parameter draws around the center. If 0 (default), uses 25\*number
    of parameters.
    -   Sets the minimum number of good parameters sets required in
        order to simulate new parameter vectors based on the covariance
        from the good parameter sets. If the calibration has not
        achieved this many good parameters, new parameters will be
        simulated around the N\_centers with the assumption that the
        parameters are independent of each other. This is also the
        minimum number of good parameter sets required before the model
        will attempt to narrow the current target bounds towards the
        stopping bounds. Setting N\_cov\_points too low can result in
        poor performance of the algorithm. A good rule of thumb is at
        least 10 x Number of Parameters.
-   N\_post: numeric(1). The weighted sample size that must be achieved
    using valid parameter values in order to stop algorithm.
    -   This is the final number of accepted parameter sets that produce
        target values within the stopping bound ranges required before
        the calibration will complete.
-   sample\_inflate: numeric(1). When generating new results for a given
    center, how many additional samples should be simulated to ensure
    enough valid (within range) parameters draws are simulated for the
    center.
-   max\_iter: numeric(1). The maximum number of iterations to attempt.
-   seed: numeric(1). The seed to set for reproducibility.
-   latinHypercube: logical(1). Should algorithm use a Latin Hypercube
    to generate first set of parameters.
-   backend\_fun: function. For advanced users only. Lets to user
    evaluate the target function(s) using their own backend, i.e.,
    simulate targets with an alternative parallel method. Only necessary
    if the backend method is not compatible with foreach. See details
    for requirements.
-   output\_directory: character(1). Path to save results to. If NULL
    (default), no results are saved. If a path is provided results are
    saved/updated every iteration. See details for more information.
-   output\_tag: character(1). Tag to add to result files names.
    “timestamp” (default) is a special code that adds the time and date
    the code was executed.
-   previous\_results\_dir: Optional character(1). Path to results
    stored during a previous run. If the user wishes to restart a run
    that didn’t complete the calibration, they can continue by using the
    outputs stored during the previous run.
-   previous\_results\_tag: Optional character(1). The tag that was
    added to the previous run output files.
-   verbose: logical(1). Prints out progress messages and additional
    information as the model works.
-   validate\_run: logical(1). If this is TRUE and an output\_directory
    is specified, the function will save all parameters generated by the
    model - even ones that were deemed invalid based on their simulated
    targets.

Some additional notes: \* Regarding parallelization: If the user wishes
to take advantage of parallelization they can register a parallel
backend before running the imabc function (e.g. registerDoParallel(cores
= detectCores() - 1)). imabc() uses foreach to submit parameters to the
target function so what works with it will work with imabc. If the
register function the user wishes to use requires that additional inputs
beyond the parameters data.table, the target function, and the special
other values listed in the backend\_fun description be passed to foreach
specifically the user must handle the entire submission of runs
themselves by defining the backend\_fun option in imabc. \* At the
moment, The distance between simulated targets and set target values
(desired targets) is calculated following a chisquare ((desired -
simulated)<sup>2</sup>)/(desired<sup>2</sup>)) except in the case where
the desired target is 0. For desired targets = 0, the function replaces
the denominator with 0.5 x range(stopping bounds):
((simulated)<sup>2</sup>)/((0.5 x (upper\_bound\_stop -
lower\_bound\_stop))<sup>2</sup>). If the user wishes to use the
stopping range to scale all the target distances they can set the global
option imabc.target\_eval\_distance to “stopping\_range” (i.e. before
running imabc, run
`options(imabc.target_eval_distance = "stopping_range")`)

## To do

-   Allow for continuing runs
    -   target modification
    -   parameter modification
