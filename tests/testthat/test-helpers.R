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




test_that(
  'All calls are retained after confirmation process is simulated (by species case)', {
    
    ## ----- User supplied scenarios ----- ## 
    md <- simulate_validatedData(
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
    )$masked_dfs[[1]][[1]]
    
    # With site-night confirmability
    md_after <- make_not_confirmable(md, confirmable_limits = c(.8, 1), phi_vec = NULL)
    
    expect_equal(all(md_after$unique_call_id %in% md$unique_call_id), TRUE)
    expect_equal(all(md$unique_call_id %in% md_after$unique_call_id), TRUE)
    
    # with by-species confirmability
    md_after2 <- make_not_confirmable(md, confirmable_limits = NULL, phi_vec = c(.8, 1))
    
    expect_equal(all(md_after2$unique_call_id %in% md$unique_call_id), TRUE)
    expect_equal(all(md$unique_call_id %in% md_after2$unique_call_id), TRUE)
    
    
    ## ----- With expand.grid for list of scenarios ----- ## 
    md2 <- simulate_validatedData(
      n_datasets = D, 
      design_type = "BySpecies",
      nsites = n,
      nspecies = K, 
      nvisits = J,
      psi = psi, 
      lambda = lambda, 
      theta = theta,
      scenarios = list_scenarios,
      directory = here::here("Testing")
    )$masked_dfs[[5]][[1]]
    
    # With site-night confirmability
    md_after3 <- make_not_confirmable(md2, confirmable_limits = c(.8, 1), phi_vec = NULL)
    
    expect_equal(all(md_after3$unique_call_id %in% md2$unique_call_id), TRUE)
    expect_equal(all(md2$unique_call_id %in% md_after3$unique_call_id), TRUE)
    
    # with by-species confirmability
    md_after4 <- make_not_confirmable(md2, confirmable_limits = NULL, phi_vec = c(.8, 1))
    
    expect_equal(all(md_after4$unique_call_id %in% md2$unique_call_id), TRUE)
    expect_equal(all(md2$unique_call_id %in% md_after4$unique_call_id), TRUE)
    
  }
)

test_that('All calls are retained after confirmation process is simulated (fixedeffort case)', {
  md <- simulate_validatedData(
    n_datasets = D, 
    design_type = "FixedPercent",
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta,
    scenarios = c(.3, .5),
    directory = here::here("Testing")
  )$masked_dfs[[1]][[1]]
  
  # With site-night confirmability
  md_after <- make_not_confirmable(md, confirmable_limits = c(.8, 1), phi_vec = NULL)
  
  expect_equal(all(md_after$unique_call_id %in% md$unique_call_id), TRUE)
  expect_equal(all(md$unique_call_id %in% md_after$unique_call_id), TRUE)
  
  # With by-spp confirmability 
  # with by-species confirmability
  md_after2 <- make_not_confirmable(md, confirmable_limits = NULL, phi_vec = c(.8, 1))
  
  expect_equal(all(md_after2$unique_call_id %in% md$unique_call_id), TRUE)
  expect_equal(all(md$unique_call_id %in% md_after2$unique_call_id), TRUE)
})

test_that('The number of calls with true_spp labels is less than or equal to the number before simulating confirmability', {
  md <- simulate_validatedData(
    n_datasets = D, 
    design_type = "FixedPercent",
    nsites = n,
    nspecies = K, 
    nvisits = J,
    psi = psi, 
    lambda = lambda, 
    theta = theta,
    scenarios = c(.3, .5),
    directory = here::here("Testing")
  )$masked_dfs[[1]][[1]]
  
  md_summary <- md %>% 
    dplyr::group_by(site, visit) %>% 
    dplyr::summarize(prop_with_true_label = sum(!is.na(true_spp))/dplyr::n(), n = dplyr::n())
  
  md_after1 <- make_not_confirmable(md, confirmable_limits = c(.8, 1), phi_vec = NULL)
  md_after2 <- make_not_confirmable(md, confirmable_limits = NULL, phi_vec = c(.8, 1))
  
  md_after1_summary <- md_after1 %>% 
    dplyr::group_by(site, visit) %>% 
    dplyr::summarize(prop_with_true_label = sum(!is.na(true_spp))/dplyr::n(), n = dplyr::n())
  
  md_after2_summary <- md_after2 %>% 
    dplyr::group_by(site, visit) %>% 
    dplyr::summarize(prop_with_true_label = sum(!is.na(true_spp))/dplyr::n(), n = dplyr::n())
  
  expect_equal(all(md_summary$prop_with_true_label >= md_after1_summary$prop_with_true_label) & 
                 all(md_summary$prop_with_true_label >= md_after2_summary$prop_with_true_label), TRUE)
  
  
})
