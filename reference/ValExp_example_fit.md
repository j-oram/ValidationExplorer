# ValExp_example_fit

Draws from an MCMC algorithm fit to simulated data. This object exists
solely for easing examples in documentation of
[mcmc_sum](https://j-oram.github.io/ValidationExplorer/reference/mcmc_sum.md).

## Format

A list of length 3 matrices with columns and rows as described here.

## Source

Simulated data using
[simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md),
then isolated one dataset and the corresponding zeros. Draws were
obtained from model fitting using the code inside
[tune_mcmc](https://j-oram.github.io/ValidationExplorer/reference/tune_mcmc.md).

## Details

- Columns: Parameters (lambda, psi, and theta for a two species
  assemblage)

- Rows; Iterations of the MCMC. There are 1000 post-warmup draws for
  each.

## Examples

``` r
str(ValExp_example_fit)
#> List of 3
#>  $ : num [1:1000, 1:8] 9.92 9.84 9.84 9.84 10.07 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:8] "lambda[1]" "lambda[2]" "psi[1]" "psi[2]" ...
#>  $ : num [1:1000, 1:8] 10.9 10.9 10.3 10.3 10.3 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:8] "lambda[1]" "lambda[2]" "psi[1]" "psi[2]" ...
#>  $ : num [1:1000, 1:8] 11.1 11.1 11 10.8 10.8 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : NULL
#>   .. ..$ : chr [1:8] "lambda[1]" "lambda[2]" "psi[1]" "psi[2]" ...
```
