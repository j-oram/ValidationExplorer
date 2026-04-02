# MCMC_sum: A custom function for summarizing MCMC posterior sampling

MCMC_sum: A custom function for summarizing MCMC posterior sampling

## Usage

``` r
mcmc_sum(out, thin = 1, truth)
```

## Arguments

- out:

  Draws from a model fit using a probabilistic programming language
  (e.g., Stan, NIMBLE or JAGS). The expected format of this input is a
  list, where each entry is a Markov chain.

- thin:

  An optional thinning interval.

- truth:

  A vector with the true parameter values organized alphanumerically by
  parameter value (e.g., lambda\\1\\, lambda\\2\\, psi\\1\\, psi\\2\\,
  theta\\1,1\\, theta\\1,2\\, theta\\2,1\\, theta\\2,2\\)

## Value

A dataframe object summarizing the MCMC draws, including diagnostics,
quantiles and posterior means.

## Examples

``` r
# An example fit of one dataset
draws <- ValExp_example_fit

# The data generating values
truth <- c(11,2,0.3, 0.6, 0.9, 0.15, 0.10, 0.85)

mcmc_sum(draws, truth = truth)
#>     parameter        Mean         SD     Naive SE Time-series SE       2.5%
#> 1   lambda[1] 10.80797746 0.51593052 0.0094195595   0.0185594014 9.83409754
#> 2   lambda[2]  4.06651065 0.22338883 0.0040785034   0.0087178870 3.64962802
#> 3      psi[1]  0.33167381 0.07147375 0.0013049262   0.0013052123 0.20344514
#> 4      psi[2]  0.67109060 0.07313445 0.0013352463   0.0013356436 0.52333194
#> 5 theta[1, 1]  0.97146813 0.01051888 0.0001920475   0.0003446017 0.94892405
#> 6 theta[2, 1]  0.06318674 0.01514101 0.0002764358   0.0004134156 0.03724551
#> 7 theta[1, 2]  0.02853187 0.01051888 0.0001920475   0.0003446017 0.01025339
#> 8 theta[2, 2]  0.93681326 0.01514101 0.0002764358   0.0004134156 0.90356911
#>           25%         50%         75%       97.5%      Rhat  ess_bulk  ess_tail
#> 1 10.46373092 10.81257122 11.16679356 11.83725680 1.0084673  700.4426  848.0550
#> 2  3.92156497  4.05445307  4.20088227  4.52428419 1.0014827  622.7861  628.2963
#> 3  0.28299750  0.32709750  0.37813063  0.48188893 1.0006074 2876.8872 2838.0309
#> 4  0.62293206  0.67472997  0.72344612  0.80175584 0.9998777 2739.8059 2904.7877
#> 5  0.96461876  0.97217656  0.97889100  0.98974661 1.0066763  615.3272 1009.7992
#> 6  0.05240405  0.06235643  0.07267621  0.09643089 1.0020734 1277.6067 1094.3764
#> 7  0.02110900  0.02782344  0.03538124  0.05107595 1.0066763  615.3272 1009.7992
#> 8  0.92732379  0.93764357  0.94759595  0.96275449 1.0020734 1277.6067 1083.1770
#>   truth capture converge
#> 1 11.00       1        1
#> 2  2.00       0        1
#> 3  0.30       1        1
#> 4  0.60       1        1
#> 5  0.90       0        1
#> 6  0.15       0        1
#> 7  0.10       0        1
#> 8  0.85       0        1
```
