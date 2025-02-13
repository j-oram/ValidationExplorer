## Testing for the class of plot_x_vs_calls functions 

fit_sum <- example_output
calls_sum <- example_val_sum

test_that(
  "can plot bias against validation effort", 
  {
    expect_no_error(
      plot_bias_vs_calls(
        sim_summary = fit_sum, 
        calls_summary = calls_sum, 
        theta_scenario = "1",
        scenarios = as.character(1:2)
      )
    )
  }
)

test_that(
  "can plot width against validation effort",
  {
    expect_no_error(
      plot_width_vs_calls(
        sim_summary = fit_sum, 
        calls_summary = calls_sum, 
        theta_scenario = "1",
        scenarios = as.character(1:2),
        regex_pars = "lambda"
      )
    )
  }
)

test_that(
  "can plot coverage against validation effort",
  {
    expect_no_error(
      plot_coverage_vs_calls(
        sim_summary = fit_sum, 
        calls_summary = calls_sum, 
        theta_scenario = "1",
        scenarios = as.character(1:2),
        regex_pars = "psi"
      )
    )
  }
)

