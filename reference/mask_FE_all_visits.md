# Mask a proportion of all visits: A function for simulating a fixed effort validation design.

Mask a proportion of all visits: A function for simulating a fixed
effort validation design.

## Usage

``` r
mask_FE_all_visits(df, effort_prop, seed = NULL)
```

## Arguments

- df:

  A dataframe object in the format of `full_dfs` output from
  [simulate_validatedData](https://j-oram.github.io/ValidationExplorer/reference/simulate_validatedData.md)

- effort_prop:

  The proportion of recordings to be randomly sampled from the first
  visit for validation

- seed:

  An optional random seed to make masking reproducible

## Value

A dataframe object that is a copy of the input `df`, but with the
appropriate level of effort according to a fixed effort validation
design.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

cd_data <- sim_dat()$full_df %>% tidyr::uncount(weights = count, .remove = FALSE)
FE_data <- mask_FE_all_visits(cd_data, effort_prop = 0.1)

head(cd_data)
#> # A tibble: 6 × 9
#>    site visit true_spp id_spp  lambda   psi  theta     z count
#>   <int> <int>    <int>  <int>   <dbl> <dbl>  <dbl> <int> <int>
#> 1     1     1        4      1   0.557 0.888 0.0986     1     1
#> 2     1     1        5      1  62.2   0.545 0.0307     1     3
#> 3     1     1        5      1  62.2   0.545 0.0307     1     3
#> 4     1     1        5      1  62.2   0.545 0.0307     1     3
#> 5     1     1        6      1 115.    0.739 0.0456     1     5
#> 6     1     1        6      1 115.    0.739 0.0456     1     5
head(FE_data)
#> # A tibble: 6 × 11
#>    site visit true_spp id_spp  lambda   psi  theta     z count selected
#>   <int> <int>    <int>  <int>   <dbl> <dbl>  <dbl> <int> <int>    <dbl>
#> 1     1     1       NA      1   0.557 0.888 0.0986     1     1        0
#> 2     1     1       NA      1  62.2   0.545 0.0307     1     3        0
#> 3     1     1       NA      1  62.2   0.545 0.0307     1     3        0
#> 4     1     1       NA      1  62.2   0.545 0.0307     1     3        0
#> 5     1     1       NA      1 115.    0.739 0.0456     1     5        0
#> 6     1     1       NA      1 115.    0.739 0.0456     1     5        0
#> # ℹ 1 more variable: unique_call_id <chr>

FE_data %>% 
  group_by(site, visit) %>% 
  summarize(
    prop_validated = sum(!is.na(true_spp))/n()
  )
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by site and visit.
#> ℹ Output is grouped by site.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(site, visit))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
#> # A tibble: 400 × 3
#> # Groups:   site [100]
#>     site visit prop_validated
#>    <int> <int>          <dbl>
#>  1     1     1          0.1  
#>  2     1     2          0.102
#>  3     1     3          0.102
#>  4     1     4          0.1  
#>  5     2     1          0.101
#>  6     2     2          0.102
#>  7     2     3          0.1  
#>  8     2     4          0.101
#>  9     3     1          0.100
#> 10     3     2          0.100
#> # ℹ 390 more rows
```
