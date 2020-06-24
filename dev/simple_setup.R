### Fixed Parms
# fixed parameters are not dealt with right now. Just assumed all parameters want to be calculated

### Alpha vs bounds
# calib.targets and target.specs are not in target_list
# CM NOTE: Need alpha, alpha.target, update.alpha
# CM NOTE: calib.targets
# calibration targets vs target specs
#   Anything with update.alpha
# if((N.in>=2*N.cov.points) & any(target.specs$update.alpha)){ Main loop
# if((N.in>=N.post | !any(target.specs$update.alpha)) | (iter>=end.iter & N.in>0)){ Main loop
# if(ESS>=N.post & !any(target.specs$update.alpha)){ Main loop

### Continue
# if (continue.runs) {} pre-Main loop

### Writing results
# writing results out

### Printing info during loop


rm(list = ls(all = TRUE))
gc()
devtools::load_all("R/")

library(data.table)
library(tidyverse)
library(parallel)
# library(future.apply)
library(foreach)
library(doParallel)
library(lhs)
library(truncnorm)

N_start <- N0 <- 1000 # starting points (from priors)
N_centers <- N.centers <- 2 # how many different points to add mass around
B <- 10 # number of points around each center to sample
N_post <- N.post <- 10 # number of parmeters within final tolerance intervals needed to quit, maybe as high as 100
max_iter <- max.iter <- 2
seed=1234
N_cov_points <- N.cov.points <- 0 # number of points needed to move from a independent assumption
sample.inflate=1
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
latinHypercube <- LHS <- TRUE
continue.runs <- FALSE
reeval = FALSE
prevruns.dir = "."
print.status = FALSE
recalc_centers <- recalc.centers <- TRUE

#: target goal, upper bound, lower bound
targets <- target_list <- list(
  t1 = list(
    m = 1,
    target = 1.5,
    low_bound_start = 1.0,
    up_bound_start = 2.0,
    low_bound_stop = 1.4,
    up_bound_stop = 1.6,
    se = 0.2 # Standard error (se.obs in calc.alpha.R)
  ),
  t2 = list(
    m = 1,
    target = 0.5,
    low_bound_start = 0.2,
    up_bound_start = 0.9,
    low_bound_stop = 0.48,
    up_bound_stop = 0.52
  )
)

fn1 <- function(x1, x2) {x1 + x2 + rnorm(1,0,0.1)}
fn2 <- function(x1, x2) {x1 * x2 + rnorm(1,0,0.1)}

fn_crc <- function(parm1, parm2) { CRCSPIN(parm = c(parm1, parm2), inputs)}

parm_names <- c("x1", "x2")
dist_func <- function(x, targets) { # optional thresholds
  res <- c()
  # res[1] <- get.p.dist(results1, targets_list$t1$pct.male[targets_list$t1$data == "parm1"])
  res[1] <- get.p.dist(fn1(x[1], x[2]), targets$t1$target)
  res[2] <- get.p.dist(fn2(x[1], x[2]), targets$t2$target)

  return(res) # Returns distance for each target
}

x1_min <- 0
x1_max <- 0.99

x2_min <- 0
x2_max <- 0.95
# If a function is specified, add_prior requires min, max, and sd. If those are inputs into the specified FUN function
#   there is no need to supply them twice, but if FUN uses a different name for those inputs, they must be provided
#   with both the names that FUN is looking for and min, max, and/or sd.
priors <- list(
  # runif expects an "n" but we will give it a vector so use_length = TRUE (default) is needed
  x1 = add_prior(
    FUN = "runif",
    min = x1_min, max = x1_max, # Inputs into runif
    sd = ((x1_max - x1_min)^2)/12 # Info required for imabc not defined in runif
  ),
  # qtruncnorm expects a vector of quantiles which is what we supply so use_length = FALSE is needed
  x2 = add_prior(
    FUN = "qtruncnorm",
    mean = 0.14, sd = 0.20, a = x2_min, b = x2_max, # Inputs into qtruncnorm
    min = x2_min, max = x2_max, use_length = FALSE # Info required for imabc not defined in qtruncnorm
  )#,
  # x2 = add_prior(quantile_f = "qtruncnorm", density_f = "dtruncnorm", mean = 0.14, sd = 0.20, a = 0, b = 0.95, use_length = FALSE),
  # For a fixed parameter we expect an "n" but we will give it a vector so use_length = TRUE is needed
  # x3 = add_prior(0.5, use_length = FALSE) # Fixed Param
)


#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
