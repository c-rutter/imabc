
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IMABC

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("carolyner/imabc")
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

In the dev directory is an R script simple\_setup.R that goes through a
basic example of the code but below will be a brief explanation of the
components as well.

The primary function is imabc(). The inputs and their descriptions are
as follows:

-   priors - list. A list of information on the prior distributions. Two
    helper functions exist for properly defining this list. See below
    for more details.
-   targets - list. A list of information on the targets. Four helper
    functions exist for properly defining this list as well. See below
    for more details.
-   target\_fun - function. A function that returns a data set of the
    predicted targets from simulated parameters. A helper function
    exists for help defining this. See below for more details.
-   previous\_results - list. A list of results saved during the last
    run. A helper function exist for helping read in all results from a
    previous run. See below for more details.
-   N\_start - integer. The sample size used for the first set of
    simulated parameters.
-   seed - integer. The seed value for reproducibility.
-   latinHypercube - boolean. Should the first simulated set of
    parameters use a latinHypercube to generate parameters.
-   N\_centers - integer. The number of centers to use for simulating
    parameters.
-   Center\_n - integer. Sample size per center for simulating
    parameters (used to be called B).
-   N\_post - integer. The minimum sample required for stopping the
    simulation before max\_iter is reached.
-   max\_iter - integer. The maximum number of iterations the code will
    attempt if N\_post is never reached.
-   N\_cov\_points - integer. The number of points needed to move from
    an independent covariance assumption.
-   sample\_inflate - numeric. When simulating results under certain
    conditions, how much more should the sample be simulated to make
    sure there are parameters within the appropriate bounds. (See
    R/get\_B\_draws.R)
-   recalc\_centers - boolean. Should the centers be recalculated in
    each iteration.
-   backend\_fun - function. If the user wishes to use something other
    than foreach () %dopar% {} to apply the target function to simulated
    parameters in parallel they may define the function here. The
    minimum requirements are:
    -   The first input is expected to be a data.table where the first
        column is called seed followed by columns for the parameters in
        the order and with the names they were given when initialized by
        define\_priors().
    -   The second input is the target function which should be defined
        using define\_target\_function().
    -   The third input is a list of other inputs of potential use to
        the evaluation of the parameters and targets. As of right now
        all that is included in this list is all\_parm\_names (which is
        a vector of the parameter names), lower\_bounds and
        upper\_bounds (which are named vectors with the current bounds
        of each target, the names are the target IDs created when using
        define\_targets()).
    -   The return value is a data.table with the estimated targets
        values for all simulated parameters
-   continue\_runs - DEPRECATED. boolean. previous\_results input lets
    function know when to continue a run. Only does a simple restart (no
    altering targets or parameters between runs).
-   output\_directory - character. The path where you would like results
    saved. Defaults to NULL which indicates the current directory is
    where results should be saved. If path doesn’t exist, imabc will try
    to create it.
-   output\_tag - character. This value is affixed to the end of the
    output files to make tracking multiple sets of results stored in the
    same directory easy to differentiate. The default, “timestamp”, is a
    special case where the date and time the run begins is affixed to
    the end of the output files.
-   verbose - boolean. Should verbose information be printed while the
    algorithm is running.

Some additional notes:

-   Regarding parallelization: If the user wishes to take advantage of
    parallelization they can register a parallel backend before running
    the imabc function (e.g. registerDoParallel(cores = detectCores() -
    1)). imabc() uses foreach to submit parameters to the target
    function so what works with it will work with imabc. If the register
    function the user wishes to use requires that additional inputs
    beyond the parameters data.table, the target function, and the
    special other values listed in the backend\_fun description be
    passed to foreach specifically the user must handle the entire
    submission of runs themselves by defining the backend\_fun option in
    imabc.
-   At the moment, The distance between simulated targets and set target
    values (desired targets) is calculated following a chisquare
    ((desired - simulated)<sup>2</sup>)/(desired<sup>2</sup>)) except in
    the case where the desired target is 0. For desired targets = 0, the
    function replaces the denominator with 0.5 x range(stopping bounds):
    ((simulated)<sup>2</sup>)/((0.5 x (upper\_bound\_stop -
    lower\_bound\_stop))<sup>2</sup>). If the user wishes to use the
    stopping range to scale all the target distances they can set the
    global option imabc.target\_eval\_distance to “stopping\_range”
    (i.e. before running imabc, run
    `options(imabc.target_eval_distance = "stopping_range")`)

### priors

There are two functions to help the user define the appropriate priors
list.

add\_prior(): Using the function the user defines all the information
associated with a specific parameter that is being calibrated. For the
current version of the code the user should specify at least the
following things:

-   … - The input(s) to feed into the distribution functions selected,
    if any input is not provided, the default value from the
    distribution functions will be used in its place. Additionally,
    imabc uses min and max values of a parameter for certain
    calculations. If not already setting these values as a part of the
    inputs for the distribution functions the user can specify them
    separately here. If these aren’t supplied by the user, the function
    will first check if the distribution functions have default values
    for them and use those if they exist. If they don’t have default
    values, imabc will use -Inf/Inf respectively as defaults. The
    function looks specifically for the option names of “min” and “max”.
    This is important as some functions may use different names for
    their min/max inputs (e.g. the truncnorm package uses a and b
    respectively. If using truncnorm functions, you will need to specify
    a, b, min, and max to have all these values set properly).
-   dist\_base\_name, density\_fn, quantile\_fn - character. optional.
    The names of the functions that will be used to generate random
    values for the parameters. If the RNG functions follow most standard
    RNG function sets (i.e. follow the same naming scheme as the uniform
    or normal distribution functions, see ?dnorm and ?dunif), than only
    one of these is required to be input by the user. The
    dist\_base\_name is the function name without the first letter
    (e.g. norm for dnorm, unif for dunif). If none of these are
    supplied, a fixed parameter is created, however, it is not the most
    efficient way to create/use a fixed parameter and is not
    recommended.
-   parameter\_name - character. optional. The name used to reference
    this parameter in the target function and in the results files. This
    can also be supplied via define\_priors().

define\_priors(): this is a wrapper function that takes one or more
priors generated by calls to add\_prior() and moves some of the
information returned by add\_prior() around so that the code is a little
bit more efficient. The only inputs into this function are objects
created by add\_prior() calls. This is required to set up the prior
distribution functions and every parameter in the model must have a
prior defined with this function. If the user doesn’t provide a name to
add\_prior(), they can still name the parameter by setting the parameter
name equal to the add\_prior() call within the define\_priors() call
(e.g. define\_priors(name\_of\_parm = add\_prior())). Can also create a
target object (without FUN defined for the targets) from the meta data
returned by the imabce function.

If no name is provided between add\_prior or define\_priors, a unique
one will be generated following the format V\[0-9\]+. In the target
function(s), the user can call the parameters using the names that are
set for each parameter or they can use the order they were created in.
See below for details.

``` r
# If dist_base_name, density_fn, and quantile_fn are NULL, parameter will be fixed
# If not provided by user, min/max default to the min/max defaults of the density function if they exist. If they do not
#   exist as defaults in the density function they then default to -Inf/Inf respectively.
# Names can be specified via:
#   define_priors(name = add_prior(...))
#   define_priors(add_prior(parameter = name, ...))
# Names must be unique
# If both name specifications are used for a given prior (e.g. define_priors(name1 = add_prior(parameter = name2, ...)))
#   then the name specified with add_prior (in this case, name2) will be used over the other method
# define_priors will assign a unique name, of the format V[0-9]+, for any parameter that isn't given a name.
# The prior distribution information is returned in the same order they are input and can be called without their names
#   using their index (e.g. priors[[1]]$density_function)
priors <- define_priors(
  x1 = add_prior(
    density_fn = "unif",
    min = 0.1, max = 0.9 # Would otherwise default to 0/1
  ),
  add_prior(
    parameter_name = "x2",
    quantile_fn = "truncnorm",
    mean = 0.75, sd = 0.05, a = 0.6, b = 0.9, # Inputs into *truncnorm
    min = 0.6, max = 1.1 # Would otherwise default to -Inf/Inf
  ),
  add_prior(
    dist_base_name = "norm",
  ) # Given the name V3, min/max default to -Inf/Inf
)
```

### targets

Just like the prior distribution information the user has the following
functions for helping them specify the target values and structure of
the targets.

add\_target(): This lets the user define a target. Each target must be a
list of the following values: target, starting\_range, stopping\_range.
starting\_range\[1\] &lt;= stopping\_range\[1\] &lt;= target &lt;=
stopping\_range\[2\] &lt;= starting\_range\[2\] must be satisfied by
these values. Optionally the user can give the target a name using
target\_name and a target function using FUN. See below for details.

group\_targets(): This lets the user group a set of targets that are to
be calibrated together. As the calibration improves its estimation of
the best parameter space, accepcted target values will be restricted to
tighter ranges (up until the range is identical to the stopping\_range).
In a group of targets, the ranges are tightened together according to
the improvement of the least improved target (e.g. if you have two
targets in a group and a set of parameters generate values all within
the stopping range of one target but whose values could only improve the
acceptable range of the second target by 10%, then both targets will
have their acceptable ranges reduced by 10%). Optionally the user can
give the target group a name using group\_name.

define\_targets(): Takes the collection of individual targets and target
groups and organizes the information so that imabc can use the
information correctly and efficiently. Just like the prior functions,
names can be assigned when inserting the lower target object into the
higher target function (e.g. define\_targets(name1 =
group\_targets(name2 = add\_target()))). Can also create a target object
(without FUN defined for the targets) from the meta data returned by the
imabc function.

For tracking purposes, even if targets aren’t grouped they are
implicitly given a group. This is important if the user wants to
reference the values in the target function or in the results. If not
defined by the user, group names will follow the format G\[0-9\]+ and
target names will follow the format T\[0-9\]+. The final target ID is
then returned as <group name>\_<target name>. The user can also
reference the names using the order that the targets are added in

as.targets(): This can convert a data frame of just target information
to a target list (without FUN defined for the targets). The data frame
must have columns for group names, target names, target value, start
min, start max, stop min, stop max. There are default values for what
these columns should be called but the user can also select which
columns go with which value.

For example, the user can specify something like:

``` r
# If no name is provided, function assigns name based on group and target.
#   Groups are of the format G[0-9]+
#   sub-targets are of the format T[0-9]+
# So the below generates two targets with IDs G1_T1 and G2_T1.
#   if the group_targets had two targets in it the second one would be G1_T2 and so on.
# There are checks to make sure the name is completely unique
# If names are provided then the target ID is just equal to sprintf(%s_%s, group_target name, add_target name)
# FUN (optional) can be defined as a function with only single input or as a function whose inputs are named for parameters
#   defined by the priors object. In the former, a named vector is passed to the function and the user can either use the
#   parameter names (e.g. x["V3"]) or the index (e.g. x[3]) to manipulate the simulated draws into an expected target value.
#   The parameters are placed into the input vector in the same order they are defined by the user when building a priors
#   object. If the inputs are named for parameters created in the priors object the function will only pass parameters
#   that share a name with the input (i.e. if the priors object defines V1, V2, and V3 and a target function is defined
#   as function(V1, V4) {...} the function will produce an error, as function(V1, V2) {V3*V2} the function will produce
#   an error, but function(V1, V2) {V1*V2} will work just fine).
#   regardless of the inputs the user defines, the functions will also have access to the seed and iteration current lower
#   and upper bounds imabc has deemed as acceptable. these must be added as inputs to the target function as seed,
#   lower_bound, and upper_bound. Use build_target_function() to create the final target function.
targets <- define_targets(
  group_targets(
    group_name = "G1",
    T1 = add_target(
      target = 1.5,
      starting_range = c(1.0, 2.0),
      stopping_range = c(1.49, 1.51)
    )
  ),
  add_target(
    target = 0.5,
    starting_range = c(0.2, 0.9),
    stopping_range = c(0.49, 0.51)
  )
)
# df <- data.frame(
#   group = c("G1", "G2", "G1"), name = c("T1", "T1", "T2"), target = c(1.5, 0.5, -1.5),
#   lower_bounds_start = c(1, 0.2, -2), upper_bounds_start = c(2, 0.9, -1),
#   lower_bounds_stop = c(1.49, 0.49, -1.51), upper_bounds_stop = c(1.51, 0.51, -1.49)
# )
# ex <- as.targets(df)
```

### target\_fun

define\_target\_function(): A helper function that lets the user create
a function that can be mapped to the appropriate simulated parameters.
Requires the target object created by define\_targets() and the prior
object created by define\_priors(). If the user defined the target
functions using add\_target() this can create a function that will apply
the inputs to all the target functions and return the vector of results.
If the user doesn’t use add\_target() to create individual target
functions the user can create their own target function and insert it
into this function using the FUN option. The last option the user can
set is use\_seed. imabc generates a random set of seed values for each
simulated set of parameters. This lets the user control the RNG within
each analysis of the target function(s) and thus be able to reproduce
any set of results after the run has finished. In order to set the seed
during the execution of the final target function, set use\_seed = TRUE.

One feature of imabc is that the user can use three pieces of
information from the simulation outside of the parameter values
themselves. The first is a unique seed for each evaluation of the target
function that was just discussed, this can be used using the use\_seed
option. The second two values are the current min/max values targets can
fall within at a particular iteration of the calibration. As the
calibration gets better and picking parameters, it will tighten the
bounds on the targets in order to make sure only “good” parameters are
used for generating alternative parameters to try. When target functions
require heavy computation it can be advantageous to use the target range
values to fail out of a given test early. To utilize these, the user can
add inputs for them in their target function(s). If the user is creating
target functions individually using add\_target(), the user should use
the input names lower\_bound and upper\_bound (e.g. ). If the user is
creating a single target function using define\_target\_function() they
would use the plural form, lower\_bounds and upper\_bounds. When
add\_target() is used, imabc will map the appropriate bounds to the
function when running the result. When the target function is defined
with define\_target\_function(FUN = …) the user must take note of the
target IDs or order in which they were defined.

When defining a single target function with define\_target\_function()
the user can define the function anyway they prefer but the returned
result must a vector whose length is equal to the number of targets
defined by the define\_targets() function. Just like the lower and upper
bounds the user can either place the results into the vector in the
order they are defined or they can use use the target IDs to assign
values.

For example, the user can specify something like:

``` r
# Target functions
fn1 <- function(x1, x2) { x1 + x2 + rnorm(1, 0, 0.01) }
fn2 <- function(x1, x2, lower_bound) { max(lower_bound, x1 * x2 + rnorm(1, 0, 0.01)) }
fn <- function(x1, x2, lower_bounds) {
  res <- c()

  res["G2_T1"] <- fn2(x1, x2, lower_bound[2])
  # OR
  # res[2] <- fn2(x1, x2, lower_bound["G2_T1"])
  # lower/upper bounds are now accessible in target_fun.
  # Either using the name of sim parm or the order they are created in define_targets
  res["G1_T1"] <- fn1(x1, x2)

  return(res)
}

target_fun <- define_target_function(targets, priors, FUN = fn, use_seed = FALSE)
```

## To do

-   Allow for continuing runs
    -   target modification
    -   parameter modification
-   testing
    -   small noise
    -   large noise (visually inspect which points are good vs bad)
    -   Deterministic function for testing
