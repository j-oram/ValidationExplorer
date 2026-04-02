# Get suggested MCMC settings prior to starting your simulations

Get suggested MCMC settings prior to starting your simulations

## Usage

``` r
tune_mcmc(dataset, zeros, return_fit = TRUE)
```

## Arguments

- dataset:

  A dataframe containing the validated and ambiguous data to be fit.
  Expected format is that of a single masked dataframe contained in the
  output from
  [simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md).
  We recommend using a dataset from your lowest-effort validation
  scenario.

- zeros:

  A dataframe containing the site/visit/true_spp/id_spp combinations
  that were never observed (count = 0). This will be one of the elements
  of the zeros object ouput from
  [simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md).

- return_fit:

  A logical indicating whether the draws from the posterior should be
  returned. Default value is set to TRUE to encourage visual inspection
  of chains during the tuning process.

## Value

A list containing the expected time to fit a single dataset with 10,000
iterations per chain, the draws for each chain, a guess for the minimum
number of iterations and warmup required to to ensure Rhat \<= 1.1 for
all model parameters, and a dataframe containing effective sample sizes
for each parameter.

## Examples

``` r
psi <- c(0.3, 0.6)
lambda <- c(11, 2)

nspecies <- length(psi)
nsites <- 30
nvisits <- 5

test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)

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
  directory = withr::local_tempdir()
)
# scenario 1 has the lowest effort, so use the 5th dataset from that scenario
# Not run during checks
if (interactive()) {
# note the index of the zeros matches the index of the dataset
tune_mcmc(dataset = fake_data$masked_dfs[[1]][[5]], zeros = fake_data$zeros[[5]])
}
```
