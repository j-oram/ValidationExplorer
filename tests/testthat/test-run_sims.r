DGVs <- list(
    psi = c(0.5, 0.75, 0.25),
    lambda = c(1, 2, 3),
    theta = t(apply(diag(18, 3) + 2, 1, function(x) {nimble::rdirch(alpha = x)}))
)

sim_inputs <- simulate_validatedData(
  n_datasets = 2, 
  design_type = "BySpecies",
  scenarios = list(spp1 = 0.5, spp2 = c(0.5, 0.75), spp3 = 0.25),
  nsites = 60,
  nspecies = 3, 
  nvisits = 4,
  psi = DGVs$psi, 
  lambda = DGVs$lambda, 
  theta = DGVs$theta, 
  scen_expand = TRUE, # Will yield 2 scenarios: spp1 = 0.5, spp2 = 0.5 and spp1 = 0.5, spp2 = 0.75
  directory = here::here("Testing")
)

test_that("run_sims returns a dataframe with expected columns (parallel = FALSE)", {
  skip_on_cran()
  skip_if_not_installed("nimble")

  tmp_dir     <- withr::local_tempdir()

  result <- run_sims(
    data_list             = sim_inputs$masked_dfs,
    zeros_list            = sim_inputs$zeros,
    DGVs                  = DGVs,
    theta_scenario_id     = "test_scenario",
    parallel              = FALSE,
    niter                 = 100,
    nburn                 = 50,
    thin                  = 1,
    nchains               = 2, 
    save_fits             = FALSE,
    save_individual_summaries_list = FALSE,
    directory             = tmp_dir
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)

  # Columns added by run_sims
  expect_true("scenario"       %in% names(result))
  expect_true("dataset"        %in% names(result))
  expect_true("theta_scenario" %in% names(result))
  expect_equal(unique(result$theta_scenario), "test_scenario")
})

test_that("run_sims parallel = TRUE produces same structure as parallel = FALSE", {
  skip_on_cran()
  skip_if_not_installed("nimble")
  skip_if(.Platform$OS.type == "windows" && parallel::detectCores() < 2,
          "Not enough cores on this Windows machine")

  tmp1 <- withr::local_tempdir()
  tmp2 <- withr::local_tempdir()

  base_args <- list(
    data_list         = sim_inputs$masked_dfs,
    zeros_list        = sim_inputs$zeros,
    DGVs              = DGVs,
    theta_scenario_id = "test_scenario",
    niter             = 100,
    nburn             = 50,
    thin              = 1,
    nchains           = 2,
    save_fits         = FALSE,
    save_individual_summaries_list = FALSE
  )

  res_seq <- do.call(run_sims, c(base_args, list(parallel = FALSE, directory = tmp1)))
  res_par <- do.call(run_sims, c(base_args, list(parallel = TRUE,  directory = tmp2)))

  # Both should return a data frame with the same columns and row count
  expect_equal(nrow(res_seq), nrow(res_par))
  expect_equal(sort(names(res_seq)), sort(names(res_par)))
})

test_that("run_sims saves summary RDS files to disk", {
  skip_on_cran()
  skip_if_not_installed("nimble")

  tmp_dir <- withr::local_tempdir()

  res <- run_sims(
    data_list         = sim_inputs$masked_dfs,
    zeros_list        = sim_inputs$zeros,
    DGVs              = DGVs,
    theta_scenario_id = "save_test",
    parallel          = FALSE,
    niter             = 100,
    nburn             = 50,
    nchains           = 2,
    save_fits         = FALSE,
    directory         = tmp_dir
  )

  expected_file <- file.path(tmp_dir, "Thetasave_test", "summary_df_for_scenario_1.rds")
  expect_true(file.exists(expected_file))

  saved <- readRDS(expected_file)
  expect_s3_class(saved, "data.frame")
})

test_that("run_sims saves fit RDS files when save_fits = TRUE", {
  skip_on_cran()
  skip_if_not_installed("nimble")
    
  tmp_dir     <- withr::local_tempdir()

  run_sims(
    data_list         = sim_inputs$masked_dfs,
    zeros_list        = sim_inputs$zeros,
    DGVs              = DGVs,
    theta_scenario_id = "fits_test",
    parallel          = TRUE,
    niter             = 100,
    nburn             = 50,
    nchains           = 2,
    save_fits         = TRUE,
    directory         = tmp_dir
  )

  expected_fit <- file.path(tmp_dir, "Thetafits_test", "fits", "fit_1_1.rds")
  expect_true(file.exists(expected_fit))
})


# ============================================================================
# Mocked tests for orchestration logic (without actual model fitting)
# ============================================================================

test_that("run_sims loops correctly over scenarios and datasets without fitting", {
  
  tmp_dir <- withr::local_tempdir()
  
  # construct the mock functions to return simple outputs without fitting
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                       dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1)),
    .package = "ValidationExplorer"
  )
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])

  # Mock the functions using with_mocked_bindings
  result <- run_sims(
      data_list         = sim_inputs$masked_dfs,
      zeros_list        = sim_inputs$zeros,
      DGVs              = DGVs,
      theta_scenario_id = "mock_test",
      parallel          = FALSE,
      niter             = 100,
      nburn             = 50,
      nchains           = 1,
      save_fits         = FALSE,
      save_individual_summaries_list = FALSE,
      directory         = tmp_dir
    )
  
  # Verify structure: should have rows for all scenarios x datasets
  expect_equal(nrow(result), nscenarios * ndatasets * 2)
  expect_true("theta_scenario" %in% names(result))
  expect_true("scenario" %in% names(result))
  expect_true("dataset" %in% names(result))
  expect_equal(unique(result$theta_scenario), "mock_test")
})

test_that("run_sims saves individual scenario summaries correctly (mocked)", {
  
  tmp_dir <- withr::local_tempdir()
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])
  
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                       dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1)),
    .package = "ValidationExplorer"
  )

  res <- run_sims(
    data_list         = sim_inputs$masked_dfs,
    zeros_list        = sim_inputs$zeros,
    DGVs              = list(psi = c(0.5), lambda = c(1), theta = matrix(1, 1, 1)),
    theta_scenario_id = "save_test",
    parallel          = FALSE,
    niter             = 100,
    nburn             = 50,
    nchains           = 2,
    save_fits         = FALSE,
    save_individual_summaries_list = FALSE,
    directory         = tmp_dir
  )
  
  # Check that scenario summary files were created
  for(scenario in 1:nscenarios) {
    expected_file <- file.path(tmp_dir, paste0("Thetasave_test"), paste0("summary_df_for_scenario_", scenario, ".rds"))
    expect_true(file.exists(expected_file), info = paste("Missing:", expected_file))
    
    # Verify the saved file is a data frame
    loaded <- readRDS(expected_file)
    expect_s3_class(loaded, "data.frame")
    expect_equal(nrow(loaded), ndatasets * 2) # 2 parameters in mocked mcmc_sum
  }
})

test_that("run_sims saves individual summaries per dataset when save_individual_summaries_list = TRUE (mocked)", {
  
  tmp_dir <- withr::local_tempdir()
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])
  
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                        dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1)),
    .package = "ValidationExplorer"
  )

  res <- run_sims(
      data_list         = sim_inputs$masked_dfs,
      zeros_list        = sim_inputs$zeros,
      DGVs              = list(psi = c(0.5), lambda = c(1), theta = matrix(1, 1, 1)),
      theta_scenario_id = "ind_test",
      parallel          = FALSE,
      niter             = 100,
      nburn             = 50,
      nchains           = 2,
      save_fits         = FALSE,
      save_individual_summaries_list = TRUE,
      directory         = tmp_dir
  )
  
  # Check that individual summary files were created for each scenario
  for(scenario in 1:nscenarios) {
    expected_file <- file.path(tmp_dir, paste0("Thetaind_test"), 
                               "individual_summaries", paste0("list_", scenario, ".rds"))
    expect_true(file.exists(expected_file), info = paste("Missing:", expected_file))
  }
})

test_that("run_sims saves fit files correctly with save_fits = TRUE (mocked)", {
  
  tmp_dir <- withr::local_tempdir()
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])
  
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                        dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1)),
    .package = "ValidationExplorer"
  )
  
  res <- run_sims(
      data_list         = sim_inputs$masked_dfs,
      zeros_list        = sim_inputs$zeros,
      DGVs              = DGVs,
      theta_scenario_id = "fit_save_test",
      parallel          = FALSE,
      niter             = 100,
      nburn             = 50,
      nchains           = 2,
      save_fits         = TRUE,
      save_individual_summaries_list = FALSE,
      directory         = tmp_dir
  )
  
  # Check that fit files were created for each dataset
  for(dataset in 1:ndatasets) {
    expected_file <- file.path(tmp_dir, "Thetafit_save_test", "fits", paste0("fit_1_", dataset, ".rds"))
    expect_true(file.exists(expected_file), info = paste("Missing:", expected_file))
  }
})


test_that("run_sims correctly assigns scenario and dataset identifiers (mocked)", {
  
  tmp_dir <- withr::local_tempdir()
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])
  
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                        dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1)),
    .package = "ValidationExplorer"
  )
  
  result <- run_sims(
      data_list         = sim_inputs$masked_dfs,
      zeros_list        = sim_inputs$zeros,
      DGVs              = DGVs,
      theta_scenario_id = "fit_save_test",
      parallel          = FALSE,
      niter             = 100,
      nburn             = 50,
      nchains           = 2,
      save_fits         = FALSE,
      save_individual_summaries_list = FALSE,
      directory         = tmp_dir
  )
  
  # Verify all scenario and dataset combinations are represented
  expect_equal(nrow(result), nscenarios * ndatasets * 2) # 2 parameters in mocked mcmc_sum
  
  # Check that all scenarios are represented
  scenarios_in_result <- sort(unique(result$scenario))
  expect_equal(scenarios_in_result, 1:nscenarios)
  
  # Check that all datasets are represented within each scenario
  for(scen in 1:nscenarios) {
    datasets_in_scenario <- unique(result[result$scenario == scen, "dataset"])
    expect_equal(length(datasets_in_scenario), ndatasets)
  }
})

test_that("run_sims output columns match expected format (mocked)", {
  
  tmp_dir <- withr::local_tempdir()
  
  nscenarios <- length(sim_inputs$masked_dfs)
  ndatasets <- length(sim_inputs$masked_dfs[[1]])
  
  local_mocked_bindings(
    runMCMC_fit = function(...) matrix(runif(20), nrow = 10,
                                        dimnames = list(NULL, c("psi[1]", "lambda[1]"))),
    mcmc_sum    = function(...) data.frame(parameter = c("psi[1]", "lambda[1]"), mean = c(0.5, 1), Rhat = c(1.0, 1.0)),
    .package = "ValidationExplorer"
  )
  
  result <- run_sims(
      data_list         = sim_inputs$masked_dfs,
      zeros_list        = sim_inputs$zeros,
      DGVs              = DGVs,
      theta_scenario_id = "col_test",
      parallel          = FALSE,
      niter             = 100,
      nburn             = 50,
      nchains           = 2,
      save_fits         = FALSE,
      save_individual_summaries_list = FALSE,
      directory         = tmp_dir
  )
  
  # Check required columns from mcmc_sum
  expect_true("parameter" %in% names(result))
  expect_true("mean" %in% names(result))
  expect_true("Rhat" %in% names(result))
  
  # Check added columns from run_sims
  expect_true("theta_scenario" %in% names(result))
  expect_true("scenario" %in% names(result))
  expect_true("dataset" %in% names(result))
  
  # Check values
  expect_true(all(result$theta_scenario == "col_test"))
})
