library(tidyverse)
library(nimble)
library(bayesplot)


source("Summary Figures/visualize_sims.R")


# Read in the simulation results object. This is a datafame. To see what it
# contains, either run the next line of code, or type colnames(theta1) in the 
# console
theta1 <-readRDS("Testing/simulation/theta_scenario_1.rds")

# Assuming that the user is in R Studio, this will bring up the summary table from the simulations.
View(theta1)

# To view a traceplot of a set of parameters from an individual fit, run the following. 
# The format for the file name is 
# Testing/simulation/Theta ScenarioNumber/fits/fit_ScenarioNumber_DatasetNumber.rds
# For example:
theta1_scenario2_dataset3 <- readRDS("Testing/simulation/fits/fit_2_3.rds")

# Create the traceplot using Bayesplot
mcmc_trace(theta1_scenario2_dataset3, regex_pars = "lambda") # Traceplot for all lambdas
mcmc_trace(theta1_scenario2_dataset3, pars = c("theta[1, 2]", "psi[1]")) # Note the space between the 

# To create overall summary figures of the simulation study, use the visualize_parameter_group function: 
visualize_parameter_group(theta1, pars = "psi", theta_scenario = "1", scenarios = 1:3)
