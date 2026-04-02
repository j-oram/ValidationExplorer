# Simulate many datasets under candidate validation designs

Simulate many datasets under candidate validation designs

## Usage

``` r
simulate_validatedData(
  n_datasets,
  design_type = c("BySpecies", "FixedPercent"),
  scenarios = NULL,
  nsites = 100,
  nspecies = 8,
  nvisits = 3,
  psi = runif(nspecies, 0.1, 0.9),
  lambda = abs(rnorm(nspecies, 0, 5)),
  theta = t(apply(diag(18, nrow = nspecies) + 2, 1, function(x) {
    
    nimble::rdirch(alpha = x)
 })),
  confirmable_limits = NULL,
  scen_expand = TRUE,
  scen_df = NULL,
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = tempdir()
)
```

## Arguments

- n_datasets:

  The number of datasets you would like to have simulated. Each of these
  simulated datasets will be subjected to all candidate validation
  designs.

- design_type:

  Character string, either "BySpecies" for a stratified-by-species
  design, or "FixedPercentage" for a fixed effort design (see Oram et
  al., in review for more details on each of these)

- scenarios:

  if `design_type = "BySpecies"`, the `scenarios` argument must be a
  list with each entry corresponding to the potential levels of effort
  for a particular autoID label. If `design_type == "FixedPercent"`,
  then the `scenarios` argument must be a vector with each entry
  corresponding to a potential percent of calls to be sampled from the
  first visit at each site. See vignette for an example.

- nsites:

  number of sites in each dataset

- nspecies:

  size of the species assemblage

- nvisits:

  the number of visits to each site. Note that these simulations assume
  a balanced design.

- psi:

  a vector of length nspecies with the assumed occurrence probabilities
  for each species

- lambda:

  a vector of length nspecies with the assumed relative activity levels
  for each species. Make sure the order is correct and matches psi.

- theta:

  a matrix containing the (mis)classification probabilities. The rows of
  this matrix must sum to 1. See vignette for an example.

- confirmable_limits:

  A numeric vector containing the lower and and upper bounds on the
  site-visit probabilities that a recording can be validated
  ("confirmed").

- scen_expand:

  If `design_type = "BySpecies"`, should `simulate_validatedData` expand
  the list of `scenarios`? If TRUE (the default value), then `scenarios`
  must be a list; if FALSE, then `simulate_validatedData` expects a
  user-supplied dataframe object through the `scen_df` argument.

- scen_df:

  If `scen_expand = FALSE`, a user-supplied dataframe object with each
  row corresponding to the validation scenario and each column to the
  species. Default value is NULL.

- save_datasets:

  logical. If TRUE, the datasets without any masking of true species
  labels (i.e., corresponding to complete validation of all recordings)
  will be saved. Default value is FALSE.

- save_masked_datasets:

  logical. If TRUE, the masked datasets (i.e., the simulated datasets
  with partial validation according to the simulation scenario) will be
  saved. This means that there will be n_datasets x
  nrow(scenarios_dataframe) datasets saved: one for each dataset under
  each validation scenario. Default value is FALSE.

- directory:

  character. Required if save_datasets = TRUE or save_masked_datasets =
  TRUE. This is where the datasets will be saved. By default, a
  temporary directory will be used. This *must* be changed if access to
  saved datasets is desired after the end of the R session, as tempdir()
  is cleared at the end of the session.

## Value

A list containing three elements:

1.  `full_datasets`: A list of length n_datasets with unmasked datasets
    (i.e., full validation of all recordings). If
    `save_datasets = TRUE`, then these will be saved individually in
    `directory` as dataset_n.rds, where n is the dataset number.

2.  `zeros`: A list of length n_datasets containing all of the
    site-visits where no recordings of a certain classification were
    observed. For example, if, in dataset 10, there were no calls from
    species 1 that were classified as 3 on visit 4 to site 156, then the
    10th entry of this list would contain a dataset with a row
    corresponding tosite = 156, visit = 4, true_spp = 1, id_spp = 3,
    with count = 0. These zeros are necessary for housekeeping in the
    model-fitting process. If `save_datasets = TRUE`, the zeros for each
    each dataset will be saved in `directory` individually as
    zeros_in_dataset_n.rds, where n is the dataset number.

3.  `masked_dfs`: A nested list containing each dataset masked under
    each scenario. masked_dfs\\\\9\\\\\\\\27\\\\ contains dataset 27,
    assuming validation scenario 9. If `save_masked_datasets = TRUE`,
    then each dataset/scenario scenario combination is saved
    individually in `directory` as
    dataset_n_masked_under_scenario_s.rds, where n is the dataset number
    and s is the scenario number.

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
  directory = tempdir()
)

```
