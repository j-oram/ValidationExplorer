# mask_by_spp: simulate a validation design

mask_by_spp: simulate a validation design

## Usage

``` r
mask_by_spp(data, props_to_val)
```

## Arguments

- data:

  A dataframe containing the columns `site`, `visit`, `true_spp`,
  `id_spp`, `count`

- props_to_val:

  a vector containing the proportion of recordings to validate for each
  species

## Value

A list containing two elements: `final_df` and `data_sum`. `final_df` is
a copy of the input `data` masked according to the validation design
supplied by `props_to_val`. The second output, `data_sum` is a dataframe
containing a summary of the number and proportion of ambiguous (i.e.,
not validated) recordings. It provides a check that the masking function
is working correctly.

## Examples

``` r
library(dplyr)

dat <- sim_dat()$full_df

head(dat)
#> # A tibble: 6 × 9
#>    site visit true_spp id_spp lambda   psi  theta     z count
#>   <int> <int>    <int>  <int>  <dbl> <dbl>  <dbl> <int> <int>
#> 1     1     1        1      1   25.9 0.607 0.648      0     0
#> 2     1     1        2      1   10.3 0.717 0.0192     1     1
#> 3     1     1        3      1  157.  0.509 0.0963     0     0
#> 4     1     1        4      1   23.2 0.444 0.0977     0     0
#> 5     1     1        5      1   91.6 0.479 0.0759     0     0
#> 6     1     1        6      1   24.6 0.505 0.0382     1     2

dat <- dat %>% tidyr::uncount(weights = count, .remove = FALSE)
val_dat <- mask_by_spp(dat, props_to_val = c(rep(.1, 4), rep(.4, 4)))
#> Joining with `by = join_by(site, visit, id_spp, lambda, psi, theta, z, count,
#> call)`

val_dat$final_df %>% group_by(id_spp) %>%
  summarize(prop_vald = sum(!is.na(true_spp))/n())
#> # A tibble: 8 × 2
#>   id_spp prop_vald
#>    <int>     <dbl>
#> 1      1     0.100
#> 2      2     0.100
#> 3      3     0.100
#> 4      4     0.100
#> 5      5     0.400
#> 6      6     0.400
#> 7      7     0.4  
#> 8      8     0.400
```
