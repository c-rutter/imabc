
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IMABC

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("jozik/imabc")
# library(imabc)
```

If working on the code, with the repo as your working directory, you can
also use:

``` r
# If working directory is the imabc directory you can use:
# devtools::load_all()
# or if working somewhere else you can put the path to the imabc directory
# devtools::load_all("imabc directory path")
```

There is no need to point load\_all() to the R folder within imabc as R
will recognize it as a package.

Other packages needed are: \* MASS \* data.table \* tidyverse \*
parallel \* foreach \* doParallel \* lhs \* truncnorm

## Running the Code

In the dev branch is an R script simple\_setup.R that goes through a
basic example of the code but below will be a brief explanation of the
components as well.

The primary function is imabc(). The inputs and their descriptions are
as follows:

  - priors - list. A list of information on the prior distributions. Two
    helper functions exist for properly defining this list. See below
    for more details.
  - targets - list. A list of information on the targets. Two helper
    functions exist for properly defining this list as well. See below
    for more details.
  - target\_fun - function. A function that returns a data set of the
    predicted targets from simulated parameters. See below for more
    details.
  - N\_start - integer. The sample size used for the first set of
    simulated parameters.
  - seed - integer. The seed value for reproducability.
  - latinHypercube - boolean. Should first simulated set of paramters
    use a latinHypercube to generate parameters.
  - N\_centers - integer. The number of centers to use for simulating
    parameters.
  - Center\_n - integer. Sample size per center for simulating
    parameters (used to be called B).
  - N\_post - integer. The minimum sample required for stopping the
    simulation before max\_iter is reached.
  - max\_iter - integer. The maximum number of iterations the code will
    attempt if N\_post is never reached.
  - N\_cov\_points - integer. The number of points needed to move from a
    independent assumption.
  - sample\_inflate - numeric. When simulating results under certain
    conditions, how much more sample should be simulated to make sure
    there are parameters within the appropriate bounds. (See
    R/get\_B\_draws.R)
  - recalc\_centers - boolean. Should the centers be recalculated each
    iteration.
  - continue\_runs - boolean. FALSE is the only valid option for now.
  - verbose - boolean. Should various information be printed while the
    algorithm is running.

### priors

There are two functions to help the user define the appropriate priors
list.

add\_prior(): Using the function the user defines all the information
associated with a specific parameter that is being calibrated. For the
current version of the code the user should specify at least the
following things:

  - FUN - string. This is a character string for the R function that
    will be used to generate random values. (e.g. “runif”, “qtruncnorm”,
    etc.). While add\_prior can currently handle this value not being
    given the rest of the code will not work properly in that scenario.
    In the future, not giving a value for FUN will result in a
    non-calibrated parameter from the models perspective.
  - use\_length - boolean. This tells imabc whether the prior expects a
    scalar input or a vector of inputs. Functions like runif expect a
    scalar value n and then return a vector of length n, while others
    expect a vector of values and return a vector with the same length.
    Setting use\_length = TRUE tells imabc that the function being used
    behaves like runif and only accepts a scalar value as input.
  - dtruncnorm - boolean. This tells imabc that when calculating the
    weights (R/get\_weight.R), the parameter will use a dtruncnorm
    function for the log\_prior\_d (R/get\_log\_prior\_d.R). If
    dtruncnorm = FALSE, log\_prior\_d will just be 0 for all values of
    that simulated paramter.
  - min - numeric. The minimum value that parameter can be.
  - max - numeric. The maximum value that parameter can be.
  - sd - numeric. The standard deviation that the parameter follows.
  - mean - numeric. the average value that the parameter should have.
  - … - Any inputs that are required by FUN that are not defined
    already.

min, max, and sd are used beyond the initial simulation of parameters
and are required regardless of the inputs into FUN. mean is only
absolutely required when dtruncnorm is equal to TRUE. If FUN requires
uses a min and max value there is no need to specify these values
multiple times - imabc will use the same value for FUN and for the rest
of the code. However if the names are different the user must specify a
value or the default will be used. For example, qtruncnorm uses a, b,
mean, and sd to specify the size and shape of the distribution it
simulates. Since our function add\_prior already requires a mean and sd,
the user just has to specify those once. However, while a and b are
technically min and max values of the distribution, since they are not
called min and max the user must specify values for a, min, b, and max
even if they are the same.

The only input the user does not specify from FUN is the input that
determines how to generate values. In runif, the user does not specify a
value for n. In qtruncnorm the user does not specify p. imabc will be
responsible for inserting those values (via the LatinHypercube draws)

define\_priors(): this is a wrapper function that takes one or more
priors generated by calls to add\_prior() and moves some of the
information returned by add\_prior() around so that the code is a little
bit more efficient. The only inputs into this function are objects
created by add\_prior() calls. The only thing the use must do is make
sure that each parameter that is being simulated for the target
functions is named as an add\_prior object.

For example, if the user wishes to make target functions f1(parm1,
parm2) and f2(parm2, parm3). the user would have to create the priors
list with something like:

``` r
priors <- define_priors(
  parm1 = add_prior(FUN = "runif", min = 0, max = 1, sd = ((1 - 0)^2)/12, use_length = TRUE, dtruncnorm = FALSE),
  parm2 = add_prior(FUN = "qtruncnorm", a = 0, b = 1, min = 0, max = 1, sd = 0.1, mean = 0.6, use_length = FALSE, dtruncnorm = TRUE),
  parm3 = add_prior(FUN = "runif", min = -1, max = 99, sd = ((99 - -1)^2)/12, use_length = TRUE, dtruncnorm = FALSE)
)
```

### targets

Just like the prior distribution information the user has a couple
functions for helping them specify the target values and structure of
the targets.

add\_targets(): this lets the user define a group of subtargets
associated with a main target. Each subtarget must be a list of the
following values: target, low\_bound\_start, up\_bound\_start,
low\_bound\_stop, and up\_bound\_stop. There can be as many subtargets
as you want.

define\_targets(): takes the collection of main targets and organizes
the information so that imabc can use the information correctly and
efficiently.

As the model gets better at hitting the targets with the parameters it
simulates, it will begin to close the bounds on the targets. Once all
the subtargets under a main target have bounds equal to the stopping
bounds the algorithm will stop trying to improve the main target and
simulate the parameters according to their current distributions. It
will then focus on improving the other main targets. I’m not 100% sure
but I think it is fine for subtargets in different main targets to share
the same name but I have not tested that yet. For now, I would recommend
that the complete list of subtargets be unique.

For example, the user can specify something like:

``` r
targets <- define_targets(
  m1 = add_targets(
    t1 = list(target = 1, low_bound_start = 0, up_bound_start = 2, low_bound_stop = 0.99, and up_bound_stop = 1.01),
    t2 = list(target = -3, low_bound_start = -5, up_bound_start = -2, low_bound_stop = -3.001, and up_bound_stop = -2.999)
  ),
  m2 = add_targets(
    t3 = list(target = 8.5, low_bound_start = 8, up_bound_start = 8.8, low_bound_stop = 8.4, and up_bound_stop = 8.6)
  )
)
```

### target\_fun

a user defined function that takes in a vector of all parameters being
calibrated and returns a simulated value for each subtarget as a vector.
The current version of the code converts each row the simulate
parameters data frame (parm\_draws) into a vector that is passed to this
function. This means that the order of the parameters can matter. The
order that the parameters are given to this function is determined by
the order they are defined in define\_priors(). On the opposite side the
order that imabc expected the simulated target values is the same as the
order they are defined in define\_targets(). This is something that we
could change with some moderate tweaks to the code but will be partially
determined by how we want the user to define this function.

What the user does with the parameters and how it calculates results
within this function is completely up to them.

For example, the user can specify something like:

``` r
fn1 <- function(parm1, parm2) {parm1 + parm2 + rnorm(1, 0, 0.01)}
fn2 <- function(parm2, parm3) {parm2 * parm3 + rnorm(1, 0, 0.01)}
target_fun <- function(x) { 
  res <- c()

  res[1] <- fn1(x[1], x[2])
  res[2] <- fn2(x[2], x[3])
  res[3] <- res[1] + res[2]^2

  return(res)
}
```

## To do

  - Determine where we should place warnings vs errors vs simple prints
  - Clean code and better name conventions
  - Document code
  - Save results during main loop as we go
  - Allow for continuing runs
      - straight restart
      - target modification
      - parameter modification
  - Helper functions for proper saving and reading in of prior and
    target objects as well as previous results (this will help both with
    continuing runs and with saving results during the main loop)
  - Allow for fixed vs calibrated parameters - This can be avoided if
    desired by having all fixed parameter values just be properly
    defined within the target\_fun by the user.
  - testing
      - small noise
      - large noise (visually inspect which points are good vs bad)
      - Deterministic function for testing
