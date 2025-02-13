devtools::load_all()

## ------------------------------------------------------------------------------------
psi <- c(0.3, 0.6)
lambda <- c(11, 2)

# Define sites and visits
nspecies <- length(psi)
nsites <- 30
nvisits <- 5


## ------------------------------------------------------------------------------------
test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85),
                      byrow = TRUE, nrow = 2)

## ------------------------------------------------------------------------------------

test_theta2 <- t(apply(18*diag(nspecies) + 2, 1,
                       function(x) nimble::rdirch(alpha = x)))

## ------------------------------------------------------------------------------------

val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)

## ----message=FALSE-------------------------------------------------------------------
# Default settings
fake_data <- simulate_validatedData(
  n_datasets = 5,
  design_type = "BySpecies",
  scenarios = val_scenarios,
  nsites = nsites,
  nvisits = nvisits,
  nspecies = nspecies,
  psi = psi,
  lambda = lambda,
  theta = test_theta1,
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = paste0(here::here("Testing"))
)

# User-supplied dataframe of scenarios

BSV_scenarios <- dplyr::tibble(
  spp1 = c(0.5, 0.5, 0.3, 0.3),
  spp2 = c(1, 0.75, 0.5, 0.4)
)


fake_data2 <- simulate_validatedData(
  n_datasets = 5,
  design_type = "BySpecies",
  scenarios = val_scenarios,
  nsites = nsites,
  nvisits = nvisits,
  nspecies = nspecies,
  psi = psi,
  lambda = lambda,
  theta = test_theta1,
  scen_expand = FALSE, 
  scen_df = BSV_scenarios,
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = paste0(here::here("Testing"))
)

# Check to see how it did with the user-supplied df
fake_data2$scenarios_df

summarize_n_validated(data_list = fake_data2$masked_dfs, scenario_numbers = 1:4)

## ------------------------------------------------------------------------------------
# investigate the validation scenarios created by enumerating the species validation levels
fake_data$scenarios_df

# investigate the number of recordings validated under each scenario
validation_summary <- summarize_n_validated(
  data_list = fake_data$masked_dfs,
  scenario_numbers = 1:nrow(fake_data$scenarios_df),
  theta_scenario = "1"
)

validation_summary

#example_val_sum <- validation_summary
#usethis::use_data(example_val_sum)

## ------------------------------------------------------------------------------------
full_dfs <- fake_data$full_datasets
head(full_dfs[[1]])

## ------------------------------------------------------------------------------------
site_visits_without_calls <- fake_data$zeros
head(site_visits_without_calls[[1]]) # notice that counts = 0 for all rows


## ------------------------------------------------------------------------------------
masked_dfs <- fake_data$masked_dfs

head(masked_dfs[[1]][[1]])


## -------------------------------------------------------------------------------------
## Testing for the MCMC_tuning function: use the "worst case" scenario, which is scenario 1
## for these scenarios because it has the lowest average number of calls validated per
## dataset

tuning_list <- tune_mcmc(dataset = masked_dfs[[1]][[5]], zeros = fake_data$zeros[[5]], return_fit = TRUE)

min_iters <- tuning_list$min_iter
warmup <- tuning_list$min_warmup
expected_time <- tuning_list$max_iter_time

shortened_fit <- lapply(tuning_list$fit, function(x) {x[(warmup+1):min_iters,]})
bayesplot::mcmc_trace(shortened_fit, regex_pars = "lambda")\

mcmc_sum(shortened_fit, truth = rep(0, ncol(shortened_fit[[1]]))) %>% 
  dplyr::select(parameter, Rhat, ess_tail, ess_bulk)

## -------------------------------------------------------------------------------------
# Run time will vary: with 5 datasets, 30 sites, 5 visits, 2 species and the assigned
# psi and lambda values, this takes ~ 1:30 per scenario. With the 2 scenarios above,
# this amounts to ~ 3 minutes when fitting in parallel.

sims_output <- run_sims(
         data_list = fake_data$masked_dfs,
         zeros_list = fake_data$zeros,
         DGVs = list(lambda = lambda, psi = psi, theta = test_theta1),
         theta_scenario_id = 1,
         parallel = TRUE,
         niter = min_iters,
         nburn = warmup,
         thin = 1,
         save_fits = FALSE,
         save_individual_summaries_list = FALSE,
         directory = here::here("Testing")
)

# example_output <- sims_output
# usethis::use_data(example_output)

## ------------------------------------------------------------------------------------
visualize_parameter_group(sim_summary = sims_output,
                          pars = "lambda",
                          theta_scenario = 1,
                          scenarios = 1:2,
                          convergence_threshold = 1.1)


## ------------------------------------------------------------------------------------
# note the space between the indices for theta[2, 1]
visualize_single_parameter(sims_output, par = "lambda[2]",
                           theta_scenario = 1,
                           scenarios = 1:2,
                           convergence_threshold = 1.03)

plot_coverage_vs_calls(
  sims_output,
  validation_summary,
  regex_pars = "lambda",
  scenarios = 1:2,
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

plot_width_vs_calls(
  sims_output,
  validation_summary,
  pars = c("lambda[1]", "psi[1]"),
  scenarios = 1:2,
  theta_scenario = 1,
  convergence_threshold = 1.05
)

## ----message=FALSE-------------------------------------------------------------------
psi <- c(0.633, 0.612, 0.849)
lambda <- c(5.934, 4.160, 14.25)

nspecies <- length(psi)
nsites <- 30
nvisits <- 4

Theta_FE <- t(apply(12*diag(nspecies) + 2, 1, function(x) nimble::rdirch(alpha = x)))

FE_data <- simulate_validatedData(
  n_datasets = 5,
  design_type = "FixedPercent",
  scenarios = c(0.05, .35, 0.65), # Note this is now a **vector** of possible scenarios. Also note the extremely small level of validation effort in the first entry!
  nsites = nsites,
  nvisits = nvisits,
  nspecies = nspecies,
  psi = psi,
  lambda = lambda,
  theta = Theta_FE,
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = here::here("Testing", "FixedEffortExample")
)

# scenario 1 is lowest effort, not surprisingly
summarize_n_validated(data_list = FE_data$masked_dfs, scenario_numbers = 1:3)

FE_tune_list <- tune_mcmc(dataset = FE_data$masked_dfs[[1]][[3]], zeros = FE_data$zeros[[3]], return_fit = TRUE)

## ------------------------------------------------------------------------------------
# Runtime: with the specified number of sites, visits, species, validation scenarios
# and parameter settings, this takes ~ 2 minutes per scenario, so around 5-6 minutes for
# this small test case. Plan accordingly!

start <- Sys.time()
FE_model_fits <- run_sims(
  data_list = FE_data$masked_dfs,
  zeros_list = FE_data$zeros,
  theta_scenario_id = "FE",
  niter = 2000, # based on results from tune_mcmc
  nburn = 1000, 
  save_fits = FALSE,
  DGVs = list(lambda = lambda, psi = psi, theta = Theta_FE),
  save_individual_summaries_list = FALSE,
  directory = here::here("Testing", "FixedEffortExample")
)

Sys.time() - start


## ------------------------------------------------------------------------------------
visualize_parameter_group(FE_model_fits, pars = "lambda", theta_scenario = 1, scenarios = 1:3)


## ------------------------------------------------------------------------------------
visualize_parameter_group(FE_model_fits, pars = "theta", theta_scenario = 1, scenarios = 1:3)


## ------------------------------------------------------------------------------------
plot_bias_vs_calls(
  FE_model_fits,
  summarize_n_validated(FE_data$masked_dfs, theta_scenario = "FE", scenario_numbers = 1:3),
  theta_scenario = 1,
  scenarios = 1:4, convergence_threshold = 1.1
)
