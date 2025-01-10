#' Summarize the number of validated recordings
#'
#' @param data_list A list of masked datasets in the format output from
#'   \link{simulate_validatedData}.
#' @param scenario_numbers An integer vector denoting the scenarios to be summarized.
#' @param theta_scenario The identifier for the classifier scenario as a character string.
#'
#' @return A dataframe object that contains the theta scenario, validation scenario,
#'   and number of validated calls.
#' @export
#'
#' @examples
#'
#' psi <- c(0.3, 0.6)
#' lambda <- c(11, 2)
#'
#' nspecies <- length(psi)
#' nsites <- 30
#' nvisits <- 5
#'
#' test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
#' val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)
#'
#' fake_data <- simulate_validatedData(
#'   n_datasets = 5,
#'   design_type = "BySpecies",
#'   scenarios = val_scenarios,
#'   nsites = nsites,
#'   nvisits = nvisits,
#'   nspecies = nspecies,
#'   psi = psi,
#'   lambda = lambda,
#'   theta = test_theta1,
#'   save_datasets = FALSE,
#'   save_masked_datasets = FALSE,
#'   directory = paste0(here::here("Testing"))
#' )
#'
#' summarize_n_validated(
#'   data_list = fake_data$masked_dfs,
#'   scenario_numbers = 1:nrow(fake_data$scenarios_df),
#' )
#'
summarize_n_validated <- function(data_list, scenario_numbers, theta_scenario=NULL) {
  
  # if the user only wants to see a subset of scenarios, pull those from the data_list
  data_list <- data_list[scenario_numbers]
  
  n_validated <- lapply(data_list, function(x) { # x is the scenario
    lapply(x, function(y) { # y is an individual dataset
      sum(!is.na(y$true_spp)) # find the number of validated calls in y
    }) %>% # output is a list.
      unlist() %>% # Take this, make it a vector and
      mean() # take the average (over datasets within the scenario) -- this is the expected number of validated recordings under scenario x
  }) %>%
    unlist() %>% # turn the list of averages into a vector
    return() # return the vector
  
  # If the user specifies the additional scenario id, create a column in the final df 
  # with this ID for all entries. This is useful for housekeeping when many different
  # simulations are being run under different classifiers. 
  if(is.null(theta_scenario)){
    return(dplyr::tibble(scenario = as.character(scenario_numbers), 
                         n_validated = n_validated))
  } else {
    return(dplyr::tibble(theta_scenario = as.character(theta_scenario), 
                         scenario = as.character(scenario_numbers), 
                         n_validated = n_validated))
  }
  
  

}
