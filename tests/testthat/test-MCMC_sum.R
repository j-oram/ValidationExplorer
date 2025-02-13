fit <- ValExp_example_fit

test_that("can summarize a fit using mcmc_sum", {
  expect_no_error(
    mcmc_sum(fit, truth = rep(0, ncol(fit[[1]])))
  )
})
