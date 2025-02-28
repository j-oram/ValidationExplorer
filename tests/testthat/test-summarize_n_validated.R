# Testing for summarize_n_validated 
K <- 2
theta <- t(apply(diag(18, K) + 2, 1, function(x) {nimble::rdirch(alpha = x)}))
test_dfs <- simulate_validatedData(5, "FixedPercent", scenarios = c(.05, .1), 10, K, 4, psi = c(0.6, 0.35), lambda = c(4,2), theta = theta)

test_that("can summarize number of calls validated", {
  expect_no_error(summarize_n_validated(test_dfs, scenario_numbers = 1:2, theta_scenario = "FP"))
})
