# Simulate data from the count-detection model with counts per site-visit

Simulate data from the count-detection model with counts per site-visit

## Usage

``` r
sim_dat(
  nsites = 100,
  nspecies = 8,
  nvisits = 4,
  seed = NULL,
  psi = stats::runif(nspecies, 0.4, 0.9),
  lambda = abs(stats::rnorm(nspecies, 0, 100)),
  theta = t(apply(18 * diag(nspecies) + 2, 1, function(x) nimble::rdirch(1, x)))
)
```

## Arguments

- nsites:

  the number of sites assumed in the design. Default value is 100.

- nspecies:

  the number of species in the assemblage. Default is 8.

- nvisits:

  the number of visits (detector nights) assumed for each site. Default
  is 4.

- seed:

  optional seed if you would like to reproduce the data simulation.

- psi:

  a vector of length nspecies that contains the occurrence probabilities
  for each species in the assemblage. These values must be in \\0,1\\.
  Default is to draw a random vector from a U(.4, .9) distribution.

- lambda:

  vector of length nspecies that contains the relative activity
  parameters for each species. Note these values need to be positive. By
  default, lambda values are the absolute value of normal(0, 100) random
  variables.

- theta:

  n nspecies x nspecies matrix containing the (mis)classification
  probabilities for each species. All entries must be in (0,1\], with
  the rows of the matrix summing to 1. The default draws rows from a
  dirichlet distribution with concentrations determined by location in
  the matrix (diagonal values have higher concentrations).

## Value

A list containing `full_df`, a complete dataframe simulated under the
user's specified parameter settings. et_sim_datasets.R. The second list
element is `params`, the parameters used to simulate data in list form.

## Examples

``` r
fake_data <- sim_dat(
   nsites = 30,
   nspecies = 2,
   nvisits = 3,
   seed = 101,
   psi = c(.3, .6),
   lambda = c(8, 3)
)

head (fake_data$full_df)
#> # A tibble: 6 × 9
#>    site visit true_spp id_spp lambda   psi  theta     z count
#>   <int> <int>    <int>  <int>  <dbl> <dbl>  <dbl> <int> <int>
#> 1     1     1        1      1      8   0.3 0.899      1     5
#> 2     1     1        2      1      3   0.6 0.0425     0     0
#> 3     1     1        1      2      8   0.3 0.101      1     2
#> 4     1     1        2      2      3   0.6 0.958      0     0
#> 5     1     2        1      1      8   0.3 0.899      1     7
#> 6     1     2        2      1      3   0.6 0.0425     0     0
```
