### Fixed Parms
# fixed parameters are not dealt with right now. Just assumed all parameters want to be calculated

### Continue
# if (continue.runs) {} pre-Main loop

### Writing results
# writing results out

### Better names
# Many places noted in code
# draw_parms() vs get_B_draws() vs parms_from_priors() and sort of related is parm_draws
devtools::load_all()


rm(list = ls(all = TRUE))
gc()

library(MASS) # Must be loaded before tidyverse
library(data.table)

# CM NOTE: My computer for some reason no longer as OpenMP support (even though it used to and OpenMP is installed.). Just
#   need to figure out what is wrong. Calculations are a little slower but otherwise work all the same.
# data.table 1.12.8 using 1 threads (see ?getDTthreads).  Latest news: r-datatable.com
# **********
#   This installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.
#   If this is a Mac, please ensure you are using R>=3.4.0 and have followed our Mac instructions here:
#   https://github.com/Rdatatable/data.table/wiki/Installation. This warning message should not occur on Windows or
#   Linux. If it does, please file a GitHub issue.
# **********


library(tidyverse)
library(parallel)
# library(future.apply)
library(foreach)
library(doParallel)
library(lhs)
library(truncnorm)

# OLD INPUTS
# N_start <- 500 # starting points (from priors)
# N_centers <- 4 # how many different points to add mass around
# B <- 50 # number of points around each center to sample. CM NOTE: now Center_n
# N_post <- 100 # number of parmeters within final tolerance intervals needed to quit, maybe as high as 100
# max_iter <- 1000
# seed=1234
# N_cov_points <- 0 # number of points needed to move from a independent assumption
# sample_inflate <- 1.5
# inputs.dir="calib_inputs"
# prior.file="parmpriors.csv"
# default.file="default_parm_values.csv"
# target.sims="targetspecs_allrace_ukfssoverall.csv"
# target.file = "calibrationtargets.xlsx"
# model.parm.order = "model_parm_order.csv"
# userfn.dir = "calib_userfn"
# outfile.modelparms="modelparms.csv"
# outfile.dist="distance.csv"
# outfile.simparms="simparms.csv"
# outfile.sampling="meancov.csv"
# output.directory="."
# latinHypercube <- TRUE
# continue.runs <- FALSE
# reeval = FALSE
# prevruns.dir = "."
# print.status = FALSE
# recalc_centers <- TRUE

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
  res[2] <- fn2(x[1], x[2]) # lower_bounds[2], upper_bounds[2] # OR # lower_bounds["m2s1"], upper_bounds["m2s1"]


  return(res)
}

# Rename lower_bounds_start to lower_bounds_now
# If no name is provided, function assigns name based on group and target.
#   Groups are of the format m[0-9]+
#   sub-targets are of the format s[0-9]+
# So the below generates two targets with IDs m1s1 and m2s1.
#   if the group_targets had two targets in it the second one would be m1s2 and so on.
# There are checks to make sure the name is completely unique
# If names are provided then the target ID is just equal to sprintf(%s%s, group_target name, add_target name)
targets <- define_targets(
  group_targets(
    add_target(
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

# Priors
x1_min <- 0.1
x1_max <- 0.9
x2_min <- 0.7
x2_max <- 0.8
# If a function is specified, add_prior requires min and max. If those are inputs into the specified FUN function
#   there is no need to supply them twice, but if dist_base_name uses a different name for those inputs, they must be
#   provided with both the names that FUN is looking for and min, max, and/or sd.
priors <- define_priors(
  x1 = add_prior(
    dist_base_name = "unif",
    min = x1_min, max = x1_max # Inputs into runif
  ),
  x2 = add_prior(
    dist_base_name = "truncnorm",
    mean = 0.75, sd = 0.05, a = x2_min, b = x2_max, # Inputs into *truncnorm
    min = x2_min, max = x2_max # Info required for imabc not defined in *truncnorm
  )
)



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
max_iter = 100
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
# last_target_list <- read.csv(paste(output_directory, "TargetList_20200715_1254PDT.csv", sep = "/"))
# new_targets <- define_targets(previous_run_targets = last_target_list)
#
# previous_results <- list(
#   parm_draws = read.csv(paste(output_directory, "SimulatedParameters_20200715_1254PDT.csv", sep = "/")),
#   sim_parm = read.csv(paste(output_directory, "SimulatedTargets_20200715_1254PDT.csv", sep = "/")),
#   target_dist = read.csv(paste(output_directory, "SimulatedDistances_20200715_1254PDT.csv", sep = "/")),
#   good_parm_draws = read.csv(paste(output_directory, "Good_SimulatedParameters_20200715_1254PDT.csv", sep = "/")),
#   good_sim_parm = read.csv(paste(output_directory, "Good_SimulatedTargets_20200715_1254PDT.csv", sep = "/")),
#   good_target_dist = read.csv(paste(output_directory, "Good_SimulatedDistances_20200715_1254PDT.csv", sep = "/")),
#   mean_cov = read.csv(paste(output_directory, "MeanCovariance_20200715_1254PDT.csv", sep = "/"))
# )
# new_results <- imabc(
#   priors = priors, # same as before
#   targets = new_targets, # from the end of the last calculation
#   target_fun = target_fun, # same as before
#   previous_results = previous_results, # NEW
#   N_start = N_start,
#   seed = 1234,
#   latinHypercube = TRUE,
#   N_centers = N_centers,
#   Center_n = Center_n,
#   N_post = N_post,
#   max_iter = max_iter,
#   N_cov_points = 0,
#   sample_inflate = 1.5,
#   recalc_centers = TRUE,
#   verbose = TRUE,
#   output_directory = output_directory
# )

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
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
