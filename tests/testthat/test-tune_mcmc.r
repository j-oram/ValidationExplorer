# tests for tune_MCMC
devtools::load_all()

test_data <- simulate_validatedData(
    n_datasets = 2,
    nsites = 10,
    nvisits = 5,
    nspecies = 2,
    design_type = 'FixedPercent',
    scenarios = .1,
    lambda = c(1, 2),
    psi = c(0.8, 0.9),
    directory = withr::local_tempdir()
)

test_that('tune_MCMC returns expected output', {

  local_mocked_bindings(
    runMCMC_fit = function(...){
        out <- list()
        for (l in 1:3) {
            out[[l]] <- matrix(rnorm(10000 * 2), ncol = 2)
            colnames(out[[l]]) <- c('alpha', 'beta')
        }
        return(out)
    },
    .package = 'ValidationExplorer'
  )

  result <- tune_mcmc(dataset = test_data$masked_dfs[[1]][[1]], zeros = test_data$zeros[[1]])
  
  # check that the output has required components
  expect_true('max_iter_time' %in% names(result))
  expect_true('min_iter' %in% names(result))
  expect_true('min_warmup' %in% names(result))
  expect_equal(nrow(result$fit), 10000)
  expect_s3_class(results$MCMC_diagnostics, 'data.frame')
  expect_true(all(c('Rhat', 'ess_bulk', 'ess_tail', 'parameter') %in% names(result$MCMC_diagnostics)))

})
