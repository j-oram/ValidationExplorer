# Automated testing for simulate_ValidatedData
# set up arguments for the simulations that won't change
D <- 5 # number of datasets
list_scenarios <-  list(
  spp1 = c(1:3*0.3), 
  spp2 = c(.2, .4)
)

my_scenarios <- dplyr::tibble(
  spp1 = c(0.2, 0.4), 
  spp2 = c(0.5, 0.5)
)
n <- 10 # number of sites
K <- 2 # number of species
J <- 3 # number of visits
psi <- c(0.6, 0.43)
lambda <- c(4, 2)
theta <- t(apply(diag(18, K) + 2, 1, function(x) {nimble::rdirch(alpha = x)}))
scen_expand <- TRUE
scen_df <- NULL
save_datasets <- FALSE
save_masked_datasets <- FALSE
directory <- here::here("Testing")


test_that("number of scenarios is correct", {
  expect_equal(length(simulate_validatedData(
    n_datasets = D, 
    design_type = "BySpecies",
    scenarios = list_scenarios, 
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta, 
    scen_expand = TRUE, 
    directory = here::here("Testing")
  )$masked_dfs), nrow(expand.grid(list_scenarios))
  )
})

test_that("number of datasets is correct", {
  expect_equal(unlist(unique(lapply(simulate_validatedData(
    n_datasets = D, 
    design_type = "BySpecies",
    scenarios = list_scenarios, 
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta, 
    scen_expand = TRUE, 
    directory = here::here("Testing")
  )$masked_dfs, length))), D)
})

test_that("scen_expand = FALSE fails if no dataframe provided", {
  expect_error(simulate_validatedData(
    n_datasets = D, 
    design_type = "BySpecies",
    scenarios = list_scenarios, 
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta, 
    scen_expand = FALSE, 
    directory = here::here("Testing")
  ), regexp = "`scen_df` must not be NULL if `scen_expand = FALSE`")
})

test_that("rows of Theta are required to be 1", {
  expect_error(
    simulate_validatedData(
      n_datasets = D, 
      design_type = "BySpecies",
      scenarios = list_scenarios, 
      nsites = n,
      nspecies = K, 
      nvisits = J,
      psi = psi, 
      lambda = lambda, 
      theta = theta - matrix(c(0.02,0,0, 0), nrow = K), 
      scen_expand = TRUE, 
      directory = here::here("Testing")
    ), 
    regexp = "rows of theta do not sum to 1"
  )
})

test_that("user can supply their own scenarios ", {
  expect_no_error(
    simulate_validatedData(
      n_datasets = D, 
      design_type = "BySpecies",
      nsites = n,
      nspecies = K, 
      nvisits = J,
      psi = psi, 
      lambda = lambda, 
      theta = theta,
      scen_expand = FALSE,
      scen_df = my_scenarios,
      directory = here::here("Testing")
    )
  )
})

test_that("output is correct when user supplies a fixed effort design", {
  expect_equal(names(simulate_validatedData(
    n_datasets = D, 
    design_type = "FixedPercent",
    scenarios = c(0.05, 0.10),
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta,
    scen_expand = FALSE,
    scen_df = my_scenarios,
    directory = here::here("Testing")
  )), c("full_datasets", "zeros", "masked_dfs")
  )
})

test_that("error when incorrect design type specified", {
  expect_error(
    simulate_validatedData(
      n_datasets = D, 
      design_type = "FixedEffort",
      scenarios = c(0.05, 0.10),
      nsites = n,
      nspecies = K, 
      nvisits = J,
      psi = psi, 
      lambda = lambda, 
      theta = theta,
      scen_expand = FALSE,
      scen_df = my_scenarios,
      directory = here::here("Testing")
    ), 
    regexp = "design_type must be one of"
  )
})
