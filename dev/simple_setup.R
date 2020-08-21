#########################################################################################################################
# To do #################################################################################################################
### Fixed Parms
# fixed parameters are not dealt with right now. Just assumed all parameters want to be calculated

### Continue
# if (continue.runs) {} pre-Main loop

### Writing results
# writing results out

### Better names
# Many places noted in code
# draw_parms() vs get_B_draws() vs parms_from_priors() and sort of related is parm_draws

# CM NOTE:
# In future we could have the results of imabc be all that is needed for continue_runs
#   that means storing all the inputs and appropriate outputs
#   we could still make a version that takes the componenents individually as well

# I like the idea of trying for a framework more in line with modern machine learning methods I.e a run would look like:
# imabc(
#   general options such as seed, N_start, verbose, output_directory, etc.
#   AND/OR
#   an imabc object (a previous run)
#   if general options and previous runs contradict than warnings are given that the new options are prioritized
# ) %>%
#   define_priors( OPTIONAL IF PREVIOUS RUN PROVIDED
#     like current version but with ability to pull from previous results if changes are desired
#   ) %>%
#   define_targets( OPTIONAL IF PREVIOUS RUN PROVIDED
#     like current version but instead of previous_run_targets, function would get previous results from imabc()
#   ) %>%
#   define_target_function( OPTIONAL IF PREVIOUS RUN PROVIDED
#     The input would be target_fun (where the user includes early stopping) or a list of target functions
#   ) %>%
#   # Do we need a define_mean_cov() %>% ?
#   # Nothing runs until the next line is added
#   estimate_parameters(
#     if previous run provided than don't need define_priors, define_targets and define_target_function
#     would include an optional input that lets the user change parallel backend
#   )
# CM NOTE: Should have option to save intermediate results as an R object or csv
# CM NOTE: Additionally should have function that can read all intermediate csv files and convert to imabc object

# TO DO:
#   TESTING
#   TESTING
#   TESTING
#   functions to save objects to R file or csv
#   functions to read objects from R file or csv (specifically the targets and prior objects)
#   continue_runs
#   fixed_parms and calibrate parms
#   print vs warning vs error
#   TESTING
#   TESTING

# Test with
# small noise
# large noise (visually inspect which points are good vs bad)
# Deterministic function for testing

# straight restart
# target modification
# parameter modification

#########################################################################################################################
# Setup R ###############################################################################################################
# Clear environment as much as possible
rm(list = ls(all = TRUE))
gc()

# Load required libraries
devtools::load_all()
# Some of these will be loaded by library(imabc):
library(MASS) # Must be loaded before tidyverse
library(data.table)
# CM NOTE: My computer for some reason no longer has OpenMP support (even though it used to and OpenMP is installed.).
#   Just need to figure out what is wrong. Calculations are a little slower but otherwise work all the same.
# data.table 1.12.8 using 1 threads (see ?getDTthreads).  Latest news: r-datatable.com
# **********
#   This installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.
#   If this is a Mac, please ensure you are using R>=3.4.0 and have followed our Mac instructions here:
#   https://github.com/Rdatatable/data.table/wiki/Installation. This warning message should not occur on Windows or
#   Linux. If it does, please file a GitHub issue.
# **********
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(lhs)
library(truncnorm)

#########################################################################################################################
# Priors ################################################################################################################
# If dist_base_name, density_fn, and quantile_fn are NULL, parameter will be fixed
# If not provided by user, min/max default to the min/max defaults of the density function if they exist. If they do not
#   exist as defaults in the density function they then default to -Inf/Inf respectively.
# Names can be specified via:
#   define_priors(name = add_prior(...))
#   define_priors(add_prior(parameter = name, ...))
# Names must be unique
# If both name specifications are used for a given prior (e.g. define_priors(name1 = add_prior(parameter = name2, ...)))
#   then the name specified with add_prior (in this case, name2) will be used over the other method
# define_priors will assign a unique name for any parameter that isn't given a name
# The prior distribution information is returned in the same order they are input and can be called without their names
#   using their index (e.g. priors[[1]]$density_function)
x1_min <- 0.1
x1_max <- 0.9
x2_min <- 0.7
x2_max <- 0.8
priors <- define_priors(
  x1 = add_prior(
    dist_base_name = "unif",
    min = x1_min, max = x1_max # Would otherwise default to 0/1
  ),
  add_prior(
    parameter_name = "x2",
    dist_base_name = "truncnorm",
    mean = 0.75, sd = 0.05, a = x2_min, b = x2_max, # Inputs into *truncnorm
    min = x2_min, max = x2_max # Would otherwise default to -Inf/Inf
  )
)

# If no name is provided, function assigns name based on group and target.
#   Groups are of the format G[0-9]+
#   sub-targets are of the format T[0-9]+
# So the below generates two targets with IDs G1T1 and G2T1.
#   if the group_targets had two targets in it the second one would be G1T2 and so on.
# There are checks to make sure the name is completely unique
# If names are provided then the target ID is just equal to sprintf(%s%s, group_target name, add_target name)
targets <- define_targets(
  group_targets(
    add_target(
      target = 1.5,
      starting_range = c(1.0, 2.0),
      stopping_range = c(1.49, 1.51)
    ),
    add_target(
      target = -0.5,
      starting_range = c(-0.9, -0.2),
      stopping_range = c(-0.51, -0.49)
    )
  ),
  add_target(
    target = 0.5,
    starting_range = c(0.2, 0.9),
    stopping_range = c(0.49, 0.51)
  )
)

# Target functions
fn1 <- function(x1, x2) {x1 + x2 + rnorm(1, 0, 0.01)}
fn2 <- function(x1, x2) {x1 * x2 + rnorm(1, 0, 0.01)}

# CM NOTE: lower_bounds and upper_bounds must be inputs even if they are used. Need to adjust code so that they are
#   optional inputs rather than required inputs
target_fun <- function(x, lower_bounds, upper_bounds) {
  res <- c()

  # lower/upper bounds are now accessible in target_fun.
  # Either using the name of sim parm or the order they are created in define_targets
  res[1] <- fn1(x[1], x[2]) # lower_bounds[1], upper_bounds[1] # OR # lower_bounds["m1s1"], upper_bounds["m1s1"]
  res[2] <- -0.5 # lower_bounds[2], upper_bounds[2] # OR # lower_bounds["m2s1"], upper_bounds["m2s1"]
  res[3] <- fn2(x[1], x[2])

  return(res)
}

priors = priors
targets = targets
target_fun = target_fun
previous_results = NULL
N_start = 1000
seed = 1234
latinHypercube = TRUE
N_centers = 2
Center_n = 50
N_post = 90
max_iter = 1000
max_iter = 50
N_cov_points = 50
sample_inflate = 1.5
recalc_centers = TRUE
verbose = TRUE
output_directory = "dev/outputs"

results <- imabc(
  priors = priors,
  targets = targets,
  target_fun = target_fun,
  N_start = N_start,
  seed = 1234,
  latinHypercube = TRUE,
  N_centers = N_centers,
  Center_n = Center_n,
  N_post = N_post,
  max_iter = max_iter,
  N_cov_points = 0,
  sample_inflate = 1.5,
  recalc_centers = TRUE,
  verbose = TRUE,
  output_directory = output_directory
)

# Test continue runs
prev_run_meta <- read.csv(paste(output_directory, "RunMetadata_20200820_1629PDT.csv", sep = "/"))
new_targets <- define_targets(previous_run_targets = prev_run_meta)
new_priors <- define_priors(previous_run_priors = prev_run_meta)

previous_results <- list(
  prev_run_meta = prev_run_meta,
  good_parm_draws = read.csv(paste(output_directory, "Good_SimulatedParameters_20200820_1629PDT.csv", sep = "/")),
  good_sim_parm = read.csv(paste(output_directory, "Good_SimulatedTargets_20200820_1629PDT.csv", sep = "/")),
  good_target_dist = read.csv(paste(output_directory, "Good_SimulatedDistances_20200820_1629PDT.csv", sep = "/")),
  mean_cov = read.csv(paste(output_directory, "MeanCovariance_20200820_1629PDT.csv", sep = "/"))
)
new_results <- imabc(
  priors = new_priors, # from last calculation (includes empirical SD from beginning of first run)
  targets = new_targets, # from the end of the last calculation
  target_fun = target_fun, # same as before
  previous_results = previous_results, # NEW
  N_start = N_start,
  seed = 12345,
  latinHypercube = TRUE,
  N_centers = N_centers,
  Center_n = Center_n,
  N_post = N_post,
  max_iter = max_iter,
  N_cov_points = 0,
  sample_inflate = 1.5,
  recalc_centers = TRUE,
  verbose = TRUE,
  output_directory = output_directory
)

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
