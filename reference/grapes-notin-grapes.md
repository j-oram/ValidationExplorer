# Negation of %in%

`%notin%` checks if elements are *not* in a vector, acting as the
opposite of `%in%`.

## Usage

``` r
x %notin% table
```

## Arguments

- x:

  Vector of values to check

- table:

  Vector to check against

## Value

A logical vector indicating if there are values not in `table`.

## Examples

``` r
1 %notin% c(2, 3, 4)   # TRUE
#> [1] TRUE
2 %notin% c(2, 3, 4)   # FALSE
#> [1] FALSE
```
