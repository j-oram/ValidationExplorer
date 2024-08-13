# This script runs an entire miniature simulation study with 2 datasets 
# from a two species assemblage masked under 4 different validation scenarios. 
# In addition of serving as an example about how one would use the functions in 
# this repo to conduct a simulation study, the output from this script is required
# to knit the vignette.Rmd file. See this file for more detailed instructions on 
# how to use these functions. 

library(tidyverse)
library(nimble) 

source("Data Simulation/simulate_BySpeciesValidation.R")
source("Model Fitting & Simulation/run_sims.R")
source("Summary Figures/visualize_sims.R")

test_theta <- t(apply(18*diag(nrow = 2) + 2, 1, function(x) nimble::rdirch(alpha = x)))

fake_data <- simulate_BySpeciesValidation(n_datasets = 2, 
                             scenarios_dataframe = tibble(spp1 = rep(c(0.5, 0.25), 2), spp2 = rep(c(.5, .25), each = 2)),
                             nsites = 10, nspecies = 2, nvisits = 2, psi = c(.8, .35), lambda = c(4, 13), 
                             theta = test_theta,
                             save_datasets = FALSE, 
                             save_masked_datasets = FALSE, directory = paste0(here::here(), "/Testing"))

# This line will take 2-4 minutes to run depending on machine capacity
sims_output <- run_sims(data_list = fake_data$masked_dfs, zeros_list = fake_data$zeros, 
         DGVs = list(lambda = c(4,13), psi = c(.8, .35), theta = test_theta), 
         theta_scenario_id = 1, parallel = FALSE, 
         save_fits = TRUE, 
         save_individual_summaries_list = FALSE, 
         directory = paste0(here::here(), "/Testing"))


visualize_parameter_group(sims_output, pars = "lambda", theta_scenario = 1, scenarios = 1:2)
visualize_parameter_group(sims_output, pars = "psi", theta_scenario = 1, scenarios = 1:2)

# Note: if only one dataset yielded a fitted model with all parameters below the convergence_threshold argument, 
# There will be no grey background errorbars -- the one that exists is the average, and is overlaid with the 
# "average" one. These overlays will always be bright blue or bright red, since whether it captured the 
# true value will either be 0 or 1. If only a black point appears, then the average posterior mean is exactly 
# equal to the true value.
visualize_single_parameter(sims_output, par = "theta[2, 1]", theta_scenario = 1, scenarios = 1:4)
