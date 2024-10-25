summarize_n_validated <- function(data_list, scenario_names, theta_scenario) {

  n_validated <- lapply(data_list, function(x) { # x is the scenario
    lapply(x, function(y) { # y is an individual dataset
      sum(!is.na(y$true_spp)) # find the number of validated calls in y
    }) %>% # output is a list.
      unlist() %>% # Take this, make it a vector and
      mean() # take the average -- this is the expected number of validated recordings under scenario x
  }) %>%
    unlist() %>%
    return() # return the list of averages (one for each scenario)

  return(dplyr::tibble(theta_scenario = theta_scenario, scenario = scenario_names, n_validated = n_validated))

}
