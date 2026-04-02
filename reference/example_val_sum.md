# example_val_sum: Example summaries of validated data

Example output from
[summarize_n_validated](https://j-oram.github.io/ValidationExplorer/reference/summarize_n_validated.md).

## Format

A dataframe with 2 rows and 4 columns

## Source

Data was simulated using
[simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md)
and summarized using
[summarize_n_validated](https://j-oram.github.io/ValidationExplorer/reference/summarize_n_validated.md).

## Details

- theta_scenario. The ID of the classifier scenario.

- scenario. The validation scenario index.

- n_validated. The expected number of validated calls in an average
  dataset simulated under each scenario.

## Examples

``` r
example_val_sum
#> # A tibble: 2 × 4
#>   theta_scenario scenario n_selected n_validated
#>   <chr>          <chr>         <dbl>       <dbl>
#> 1 Theta_example  1              212.        212.
#> 2 Theta_example  2              385.        385.
```
