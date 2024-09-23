## ----setup, include=FALSE, message=FALSE---------------------------------------------
library(tidyverse)
library(nimble)
library(coda)
library(rstan)
library(parallel)
library(here)
library(kableExtra)
theme_set(theme_bw())

# getting a message to "Please start a new R session in the new project directory."
# after running the next line 
here::set_here(path = ".")
## ----eval=FALSE, echo=TRUE-----------------------------------------------------------
## install.packages("your_package_name_here")


## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------------------
## library(tidyverse)
## library(nimble)
## library(coda)
## library(rstan)
## library(parallel)
## library(here)

# source not working...
# Error in file(filename, "r", encoding = encoding) : 
#   cannot open the connection
# In addition: Warning message:
#   In file(filename, "r", encoding = encoding) :
#   cannot open file '../Data Simulation/count_detection_sim.R': No such file or directory
# seems there are cousre calls in the file that is getting sourced...
# best coding practice is to source all scripts in the testing file, so make that change... 
## ------------------------------------------------------------------------------------
source("Data Simulation/simulate_validatedData.R")
source("Data Simulation/count_detection_sim.R")
source("Data Simulation/mask_by_spp.R")
source("Data Simulation/mask_FE.R")

## ------------------------------------------------------------------------------------
psi <- c(0.3, 0.6, 0.9)
lambda <- c(11, 2, 4)

# Define sites and visits 
nspecies <- length(psi)
nsites <- 30
nvisits <- 5


## ------------------------------------------------------------------------------------
test_theta1 <- matrix(c(0.9, 0.05, 0.05,
                       0.1, 0.85, 0.05, 
                       0.02, 0.03, 0.95), byrow = TRUE, nrow = 3)

test_theta1


## ------------------------------------------------------------------------------------
test_theta2 <- t(apply(18*diag(nspecies) + 2, 1, function(x) nimble::rdirch(alpha = x)))
test_theta2


## ------------------------------------------------------------------------------------
## Now a built in test within the simulate_ValidatedData function, per Katie's comment
rowSums(test_theta1)
rowSums(test_theta2)


## ------------------------------------------------------------------------------------
# old way: do expand.grid yourself: 
#val_scenarios <- expand.grid(spp1 = c(.75, .5), spp2 = c(.25, .5), spp3 = c(.25, .75))

# simulate_ValidatedData now includes a call to expand.grid call internally, so all that 
# needs to be supplied is a list (when `validation_design = "BySpecies"`)

val_scenarios <- list(spp1 = c(.75, .5), spp2 = c(.25, .5), spp3 = c(.25, .75))


## SOMETHING WRONG HERE...GETTING ERROR AND CANNOT SIMULATE DATA. PERHAPS test is messed up?
# Error in simulate_validatedData(n_datasets = 5, validation_design = "BySpecies",  : 
#                                   The rows of theta do not sum to 1.
# CANNOT TEST Lines 98-133 due to this error, resuming testing below...
## ----message=FALSE-------------------------------------------------------------------
fake_data <- simulate_validatedData(
  n_datasets = 5, 
  validation_design = "BySpecies",
  scenarios = val_scenarios, 
  nsites = nsites, 
  nvisits = nvisits, 
  nspecies = nspecies,
  psi = psi, 
  lambda = lambda,
  theta = test_theta2, 
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = paste0(here::here("Testing"))
)


## ------------------------------------------------------------------------------------
# investigate the validation scenarios created by enumerating the species validation levels
fake_data$scenarios_df

# investigate the number of recordings validated under each scenario
source("Data Simulation/summarize_n_validated.R")
validation_summary <- summarize_n_validated(
  data_list = fake_data$masked_dfs, 
  scenario_names = as.character(1:nrow(fake_data$scenarios_df)), 
  theta_scenario = "1"
)

validation_summary

## ------------------------------------------------------------------------------------
full_dfs <- fake_data$full_datasets
head(full_dfs[[1]])


## ------------------------------------------------------------------------------------
site_visits_without_calls <- fake_data$zeros
head(site_visits_without_calls[[1]])


## ------------------------------------------------------------------------------------
masked_dfs <- fake_data$masked_dfs

# View dataset 3 with scenario 7 validation effort.
head(masked_dfs[[7]][[3]])



## -------------------------------------------------------------------------------------

# SAME ISSUE AS ABOVE...can fix by loading all functions from one place
source("Model Fitting & Simulation/run_sims.R")
source("Model Fitting & Simulation/MCMC_sum.R")
source("Model Fitting & Simulation/runMCMC_fit.R")
# Run time will vary: with 5 datasets, 30 sites, 5 visits, 3 species and the assigned 
# psi and lambda values, this takes ~ 2 minutes per scenario. With the 8 scenarios above,
# this amounts to ~ 16-18 minutes when fitting in parallel. 

# STOPPING HERE FOR NOW, CAN'T TEST B/C CANT GENERATE FAKE_DATA

sims_output <- run_sims(
         data_list = fake_data$masked_dfs,
         zeros_list = fake_data$zeros,
         DGVs = list(lambda = lambda, psi = psi, theta = test_theta2),
         theta_scenario_id = 1, 
         parallel = TRUE,
         niter = 2000, thin = 1,
         save_fits = FALSE,
         save_individual_summaries_list = FALSE,
         directory = here("Testing")
)


## ------------------------------------------------------------------------------------
# read in fit object -- my sneaky way of making the vignette knit faster was to save the
# output from the previous chunk and then read in the results, hiding this code block
# fit_1_1 <- readRDS("../Testing/Theta1/fits/fit_1_1.rds")

# visualize using bayesplot (if `save_fits = TRUE`)
# bayesplot::mcmc_dens_overlay(fit_1_1, pars = "lambda[1]")


## ------------------------------------------------------------------------------------
# A traceplot, if you like (and you saved fits)
# bayesplot::mcmc_trace(fit_1_1, regex_pars = "lambda")


## ----eval=TRUE, echo=FALSE, message=FALSE--------------------------------------------
# Sneaky way to get around the knitting/compilation problem
# biglist <- list()
# for(i in 1:4){
#   biglist[[i]] <- readRDS(paste0(here("Testing", "Theta1"), "/summary_df_for_scenario_",i,".rds"))
# }
# 
# sims_output <- do.call("bind_rows", biglist)


## ------------------------------------------------------------------------------------
source("Summary Figures/visualize_sims.R")
visualize_parameter_group(sim_summary = sims_output, 
                          pars = "lambda", 
                          theta_scenario = 1, 
                          scenarios = 1:4, 
                          convergence_threshold = 1.1)


## ------------------------------------------------------------------------------------
# note the space between the indices for theta[2, 1]
visualize_single_parameter(sims_output, par = "theta[2, 1]", 
                           theta_scenario = 1, 
                           scenarios = 1:3, 
                           convergence_threshold = 1.2)

# New plotting functions, feedback welcome! (Also, not I'm not wed to either of these; 
# if you think it would be better to get rid of these functions and stick with what we 
# have, let me know! My reason for thinking of these is that this is likely the kind of
# really simple check that practitioners will be looking for.)
plot_coverage_vs_calls(
  sims_output, 
  validation_summary,
  regex_pars = "lambda",
  scenarios = 1:8, 
  theta_scenario = 1,
  convergence_threshold = 1.1
)

plot_bias_vs_calls(
  sims_output, 
  validation_summary, 
  pars = c("psi[1]", "psi[2]", "psi[3]"),
  scenarios = 1:8, 
  theta_scenario = 1,
  convergence_threshold = 1.1
)


## ----message=FALSE-------------------------------------------------------------------
psi <- c(0.633, 0.612, 0.849, 0.898)
lambda <- c(5.934, 4.160, 14.25, 28.25)

Theta_FE <- matrix(
  c(
    0.7906, 0.1785, 0.027, 0.0039, 
    0.010425, 0.984025, 0.003225, 0.002325, 
    0.007625, 0.005625, 0.983425, 0.003325,
    0.008775, 0.008875, 0.008775, 0.973575
  ), 
  nrow = 4, 
  byrow = TRUE
)

nsites <- 100
nvisits <- 4
nspecies <- length(psi)

FE_data <- simulate_validatedData(
  n_datasets = 5, 
  validation_design = "FixedPercent",
  scenarios = c(0.05, .25,  0.5, 0.75), # Note this is now a **vector** of possible scenarios. Also note the extremely small level of validation effort in the first entry!
  nsites = nsites, 
  nvisits = nvisits, 
  nspecies = nspecies,
  psi = psi, 
  lambda = lambda,
  theta = Theta_FE, 
  save_datasets = TRUE,
  save_masked_datasets = TRUE,
  directory = paste0(here::here("Testing", "FixedEffortExample"))
)


## ------------------------------------------------------------------------------------
# Runtime: with the specified number of sites, visits, species, validation scenarios
# and parameter settings, this takes ~ 9 minutes per scenario, so around 36 minutes for
# this small test case. Plan accordingly! 
FE_model_fits <- run_sims(
  data_list = FE_data$masked_dfs,
  zeros_list = FE_data$zeros,
  theta_scenario_id = "FE",
  save_fits = FALSE,
  DGVs = list(lambda = lambda, psi = psi, theta = Theta_FE),
  save_individual_summaries_list = FALSE,
  directory = here("Testing", "FixedEffortExample")
)


## ----echo=FALSE----------------------------------------------------------------------
# For faster knitting. Note that eval=FALSE in previous chunk. If you change 
# this setting and refit the model, your results may change! 
#saveRDS(FE_model_fits, paste0(here("Testing", "FixedEffortExample"), "/FE_model_fits.rds"))
# FE_model_fits <- readRDS(paste0(here("Testing", "FixedEffortExample"), "/FE_model_fits.rds"))


## ------------------------------------------------------------------------------------
visualize_parameter_group(FE_model_fits, pars = "lambda", theta_scenario = 1, scenarios = 1:4)


## ------------------------------------------------------------------------------------
visualize_parameter_group(FE_model_fits, pars = "psi", theta_scenario = 1, scenarios = 1:4)


## ------------------------------------------------------------------------------------
visualize_parameter_group(FE_model_fits, pars = "theta", theta_scenario = 1, scenarios = 1:4)


## ------------------------------------------------------------------------------------
summarize_n_validated(FE_data$masked_dfs, theta_scenario = "FE", scenario_names = as.character(1:4))

## ------------------------------------------------------------------------------------
plot_bias_vs_calls(
  FE_model_fits,
  summarize_n_validated(FE_data$masked_dfs, theta_scenario = "FE", scenario_names = as.character(1:4)), 
  theta_scenario = 1, 
  scenarios = 1:4, convergence_threshold = 1.1
)
