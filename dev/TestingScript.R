#########################################################################################################################
#                                                                                                                       #
#  Title: Testing imabc package code
#  Author: Chris Maerzluft
#  Last Edit:
#                                                                                                                       #
#########################################################################################################################
# Description ###########################################################################################################
#
# Need to be in imabc directory to run
#
# Setup R ###############################################################################################################
# Clean environment
rm(list = ls(all = TRUE))
gc()

# Libraries
library(dplyr) # >= 1.0.0

# Function
load_all()

# Toy Model to get code working with
# x is a vector of length 2
toy_model <- function(x) {
  # F1,                           F2
  c(x[1] + x[2] + rnorm(1,0,0.1), x[1]*x[2] + rnorm(1,0,0.1))
}

toy_prior=list(c("unif",0,1),c("normal",1,2))


