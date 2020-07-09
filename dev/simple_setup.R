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


fn1 <- function(x1, x2) {x1 + x2 + rnorm(1, 0, 0.01)}
fn2 <- function(x1, x2) {x1 * x2 + rnorm(1, 0, 0.01)}

targets <- define_targets(
  m1 = add_targets(
    t1 = list(
      target = 1.5,
      low_bound_start = 1.0,
      up_bound_start = 2.0,

      # low_bound_start = 1.4,
      # up_bound_start = 2.6,

      low_bound_stop = 1.49,
      up_bound_stop = 1.51
    )
  ),
  m2 = add_targets(
    t2 = list(
      target = 0.5,
      low_bound_start = 0.2,
      up_bound_start = 0.9,

      # low_bound_start = 0.4,
      # up_bound_start = 0.6,

      low_bound_stop = 0.49,
      up_bound_stop = 0.51
    )
  )
)

target_fun <- function(x) {
  res <- c()

  res[1] <- fn1(x[1], x[2])
  res[2] <- fn2(x[1], x[2])

  return(res)
}

# x1_min <- 0.5
# x1_max <- 0.99
#
# x2_min <- 0.25
# x2_max <- 0.95

x1_min <- 0.7
x1_max <- 0.8

x2_min <- 0.7
x2_max <- 0.8
# If a function is specified, add_prior requires min, max, and sd. If those are inputs into the specified FUN function
#   there is no need to supply them twice, but if FUN uses a different name for those inputs, they must be provided
#   with both the names that FUN is looking for and min, max, and/or sd.
priors <- define_priors(
  x1 = add_prior(
    FUN = "runif",
    min = x1_min, max = x1_max, # Inputs into runif
    sd = ((x1_max - x1_min)^2)/12, # Info required for imabc not defined in runif
    dtruncnorm = FALSE # this is for get_weights. Might want to specify it differently (or let user provide function e.g. dunif)
  ),
  x2 = add_prior(
    FUN = "qtruncnorm",
    mean = 0.75, sd = 0.2, a = x2_min, b = x2_max, # Inputs into qtruncnorm
    min = x2_min, max = x2_max, use_length = FALSE # Info required for imabc not defined in qtruncnorm
  )#,
  # x2 = add_prior(quantile_f = "qtruncnorm", density_f = "dtruncnorm", mean = 0.14, sd = 0.20, a = 0, b = 0.95, use_length = FALSE),
  # For a fixed parameter we expect an "n" but we will give it a vector so use_length = TRUE is needed
  # x3 = add_prior(0.5, use_length = FALSE) # Fixed Param
)


priors = priors
targets = targets
target_fun = target_fun
N_start = 1000
seed = 1234
latinHypercube = TRUE
N_centers = 2
Center_n = 50
N_post = 100
max_iter = 1000
N_cov_points = 0
sample_inflate = 1.5
recalc_centers = TRUE
continue_runs = FALSE
verbose = TRUE

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
  continue_runs = FALSE,
  verbose = TRUE
)

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
