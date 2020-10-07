#########################################################################################################################
# To do #################################################################################################################
### Fixed Parms
# fixed parameters are not dealt with right now. Just assumed all parameters want to be calculated

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
devtools::document()
devtools::load_all()

# Some of these will be loaded by library(imabc):
# library(data.table)
# CM NOTE: My computer for some reason no longer has OpenMP support (even though it used to and OpenMP is installed.).
#   Just need to figure out what is wrong. Calculations are a little slower but otherwise work all the same.
# data.table 1.12.8 using 1 threads (see ?getDTthreads).  Latest news: r-datatable.com
# **********
#   This installation of data.table has not detected OpenMP support. It should still work but in single-threaded mode.
#   If this is a Mac, please ensure you are using R>=3.4.0 and have followed our Mac instructions here:
#   https://github.com/Rdatatable/data.table/wiki/Installation. This warning message should not occur on Windows or
#   Linux. If it does, please file a GitHub issue.
# **********
library(truncnorm)
library(doParallel) # I don't think this is required by imabc specifically (it is only needed if using doParallel to manage
#   the backend parallel process handling)

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
# define_priors will assign a unique name, of the format V[0-9]+, for any parameter that isn't given a name.
# The prior distribution information is returned in the same order they are input and can be called without their names
#   using their index (e.g. priors[[1]]$density_function)
# Fixed parameters has not been implemented yet
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
    dist_base_name = "truncnorm",# "truncnorm",
    mean = 0.75, sd = 0.05, a = x2_min, b = x2_max, # Inputs into *truncnorm
    min = x2_min, max = x2_max # Would otherwise default to -Inf/Inf
  ),
  add_prior(
    dist_base_name = "norm"
  )
)
priors
print(priors, detail = T)

# df <- data.frame(
#   parameter_name = c("x1", "x2", "x3"),
#   dist_base_name = c("unif", NA, NA),
#   density_fn = c(NA, "dtruncnorm", NA),
#   quantile_fn = c(NA, NA, "qnorm"),
#   mean = c(NA, 0.75, NA),
#   sd = c(NA, 0.05, NA),
#   min = c(x1_min, x2_min, NA),
#   max = c(x1_max, x2_max, NA),
#   a = c(NA, x2_min, NA),
#   b = c(NA, x2_max, NA)
# )
# ex <- as.priors(
#   df,
#   name_var = "parameter_name",
#   dist_var = "dist_base_name",
#   density_var = "density_fn",
#   quantile_var = "quantile_fn"
# )
# print(ex, detail = T)


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
    # group_name = "T3",
    add_target(
      target = 1.5,
      starting_range = c(1.0, 2.0),
      stopping_range = c(1.49, 1.51)#,
      # FUN = function(x1, x2) { x1 + x2 + rnorm(1, 0, 0.01) }
    ),
    add_target(
      target = 0,
      starting_range = c(-1.0, 1.0),
      stopping_range = c(-0.1, 0.1),
    #   FUN = function(x1, x2) { -1*(x1 + x2 + rnorm(1, 0, 0.01)) }
    )
  ),
  add_target(
    target = 0.5,
    starting_range = c(0.2, 0.9),
    stopping_range = c(0.49, 0.51)#,
    # FUN = function(x, lower_bound) { max(lower_bound, x[1] * x[2] + rnorm(1, 0, 0.01)) }
  )
)
targets

# as.targets example
# df <- data.frame(
#   target_groups = c("G1", "G1", NA, NA, "G2", "G2", "G2", NA),
#   target_names = c("T1", "T3", "T2", "t4", "T5", "q", "baby", "Hey"),
#   targets = c(1.5, 0.5, -1.5, 1:4, 0),
#   current_lower_bounds = c(1, 0.2, -2, -11:-14, -2),
#   current_upper_bounds = c(2, 0.9, -1, 11:14, 2),
#   stopping_lower_bounds = c(1.49, 0.49, -1.51, -1:-4, -1),
#   stopping_upper_bounds = c(1.51, 0.51, -1.49, 2:5, 1),
#   update = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
# )
# ex <- as.targets(df)


# Target functions
fn1 <- function(x1, x2) { x1 + x2 + rnorm(1, 0, 0.01) }
fn2 <- function(x1, x2) { x1 * x2 + rnorm(1, 0, 0.01) }
fn <- function(x1, x2, targets, priors) {
  res <- c()
  res["T3"] <- fn2(x1, x2)

  # lower/upper bounds are now accessible in target_fun.
  # Either using the name of sim parm or the order they are created in define_targets
  res["T1"] <- fn1(x1, x2)
  res["T2"] <- rnorm(1, 0, 0.01)

  return(res)
}

target_fun <- define_target_function(targets, priors, FUN = fn, use_seed = FALSE)
target_fun(c(x1 = 0.75, x2 = 0.75), seed = 12345, targets = targets, priors = priors)


priors = priors
targets = targets
target_fun = target_fun
previous_results = NULL
N_start = 1000
seed = 12345
latinHypercube = TRUE
N_centers = 2
Center_n = 50
N_post = 90
max_iter = 10
N_cov_points = 50
sample_inflate = 1.5
recalc_centers = TRUE
backend_fun = NULL
verbose = TRUE
output_directory = "dev/outputs/out"
output_tag = "timestamp"

# Setup parallel handling
registerDoParallel(cores = detectCores() - 1) # cluster auto-closed with foreach

results <- imabc(
  priors = priors,
  targets = targets,
  target_fun = target_fun,
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
  backend_fun = backend_fun,
  verbose = TRUE,
  output_directory = output_directory,
  output_tag = "test2"
)

# # Test continue runs
# prev_run_meta <- read.csv(paste(output_directory, "RunMetadata_test1.csv", sep = "/"))
# new_targets <- define_targets(previous_run_targets = prev_run_meta)
# new_priors <- define_priors(previous_run_priors = prev_run_meta)
#
# previous_results <- list(
#   prev_run_meta = prev_run_meta,
#   good_parm_draws = read.csv(paste(output_directory, "Good_SimulatedParameters_test1.csv", sep = "/")),
#   good_sim_parm = read.csv(paste(output_directory, "Good_SimulatedTargets_test1.csv", sep = "/")),
#   good_target_dist = read.csv(paste(output_directory, "Good_SimulatedDistances_test1.csv", sep = "/")),
#   mean_cov = read.csv(paste(output_directory, "MeanCovariance_test1.csv", sep = "/"))
# )
last_run <- read_previous_results(path = output_directory, tag = "test2")

priors = last_run$new_priors # from last calculation (includes empirical SD from beginning of first run)
targets = last_run$new_targets # from the end of the last calculation
target_fun = target_fun # same as before
previous_results = last_run$previous_results # NEW
N_start = N_start
seed = 12345
latinHypercube = TRUE
N_centers = N_centers
Center_n = Center_n
N_post = N_post
max_iter = max_iter
N_cov_points = 0
sample_inflate = 1.5
recalc_centers = TRUE
verbose = TRUE
output_directory = output_directory
output_tag = "test3"

new_results <- imabc(
  priors = last_run$new_priors, # from last calculation (includes empirical SD from beginning of first run)
  targets = last_run$new_targets, # from the end of the last calculation
  target_fun = target_fun, # same as before
  previous_results = last_run$previous_results, # NEW
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
  output_directory = output_directory,
  output_tag = "test3"
)


#########################################################################################################################
# NEW TO DO #############################################################################################################
# Test define_priors() when using previous_run_priors

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
