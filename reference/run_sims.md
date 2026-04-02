# run_sims: conduct simulations easily

run_sims: conduct simulations easily

## Usage

``` r
run_sims(
  data_list,
  zeros_list,
  DGVs,
  theta_scenario_id,
  parallel = TRUE,
  niter = 2000,
  nburn = floor(niter/2),
  thin = 1,
  nchains = 3,
  save_fits = FALSE,
  save_individual_summaries_list = FALSE,
  directory = tempdir()
)
```

## Arguments

- data_list:

  nested list of masked dataframes (datasets nested within scenarios –
  this is the format of `masked_dfs` output from
  [simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md))

- zeros_list:

  list of dataframes containing the site/visit/true_spp/id_spp
  combinations where no calls were observed.

- DGVs:

  A named list with entries psi, lambda and theta containing the true
  values of the respective parameters.

- theta_scenario_id:

  A character string ID for the simulations being run.

- parallel:

  Should models be fit in parallel? Default value is TRUE.

- niter:

  Number of iterations per MCMC chain.

- nburn:

  Number of warmup iterations.

- thin:

  Thinning interval for the MCMC chains.

- nchains:

  The number of chains.

- save_fits:

  Should individual model fits be saved? This could require large
  amounts of disk space if you are fitting many large models to big
  datasets. Default value is FALSE.

- save_individual_summaries_list:

  Should summaries for individual model fits be saved? While this
  requires much less space than `save_fits`, we still recommend keeping
  this at the default value of FALSE. Only use it if you anticipate that
  simulations may be interrupted.

- directory:

  The directory to save objects. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), but users should
  specify a permanent location for real simulation studies as
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) is cleared at the
  end of the R session.

## Value

a dataframe with the summaries (from
[mcmc_sum](https://j-oram.github.io/ValidationExplorer/reference/mcmc_sum.md))
for all scenarios and datasets. A copy of this output is also saved to
the current working directory.

## Examples

``` r
# :::::::::::: Simulate data ::::::::::::: #
psi <- c(0.3, 0.6)
lambda <- c(11, 2)

nspecies <- length(psi)
nsites <- 30
nvisits <- 5

test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)

td <- withr::local_tempdir()

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
  directory = td
)

# ::::::::::::: run simulations on sim'd data ::::::::::: #

if (interactive()){

out <- run_sims(
  data_list = fake_data$masked_dfs,
  zeros_list = fake_data$zeros,
  DGVs = list(lambda = lambda, psi = psi, theta = test_theta1),
  theta_scenario_id = 'StratBySpecies_1',
  parallel = FALSE,
  nchains = 2,
  niter = 500,
  nburn = 250,
  thin = 1,
  save_fits = FALSE,
  save_individual_summaries_list = FALSE,
  directory = td
)
}
```
