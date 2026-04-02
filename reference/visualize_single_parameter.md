# visualize_single_parameter

See detailed simulation study results for a parameter of interest

## Usage

``` r
visualize_single_parameter(
  sim_summary,
  par,
  theta_scenario,
  scenarios,
  convergence_threshold = 1.1
)
```

## Arguments

- sim_summary:

  Summary output in the format of
  [run_sims](https://j-oram.github.io/ValidationExplorer/reference/run_sims.md).

- par:

  The parameter to be visualized.

- theta_scenario:

  The theta scenario IDs. This should match the `theta_scenario_id`
  argument
  of[`run_sims()`](https://j-oram.github.io/ValidationExplorer/reference/run_sims.md).

- scenarios:

  Scenarios to be visualized.

- convergence_threshold:

  If the Gelman-Rubin statistic is below this value, consider an MCMC to
  have converged. Default value is 1.1, but we recommend 1.05.

## Value

A ggplot object summarizing simulation results.

## Examples

``` r
visualize_single_parameter(
  example_output,
  par = "lambda[1]",
  theta_scenario = "1",
  scenarios = 1:2,
  convergence_threshold = 1.05
)

```
