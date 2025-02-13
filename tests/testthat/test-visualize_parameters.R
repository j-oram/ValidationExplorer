## Test the visualize_parameter_group 

fit_sum <- example_output

test_that("can visualize_parameter_group", {
  expect_no_error(
    visualize_parameter_group(
      fit_sum, 
      pars = "lambda", 
      theta_scenario = 1, 
      scenarios = 1:2, 
      convergence_threshold = 1.1
    )
  )
})

test_that("can visualize one parameter", {
  expect_no_error(
    visualize_single_parameter(
      fit_sum, 
      par = "theta[2, 1]", 
      theta_scenario = 1, 
      scenarios = 1:2, 
      convergence_threshold = 1.1
    )
  )
})
