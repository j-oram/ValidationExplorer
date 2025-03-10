% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_n_validated.R
\name{summarize_n_validated}
\alias{summarize_n_validated}
\title{Summarize the number of validated recordings}
\usage{
summarize_n_validated(data_list, scenario_numbers, theta_scenario)
}
\arguments{
\item{data_list}{A list of masked datasets in the format output from
\link{simulate_validatedData}.}

\item{scenario_numbers}{An integer vector denoting the scenarios to be summarized.}

\item{theta_scenario}{The identifier for the classifier scenario as a character string.}
}
\value{
A dataframe object that contains the theta scenario, validation scenario,
and number of validated calls.
}
\description{
Summarize the number of validated recordings
}
\examples{

psi <- c(0.3, 0.6)
lambda <- c(11, 2)

nspecies <- length(psi)
nsites <- 30
nvisits <- 5

test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)

fake_data <- simulate_validatedData(
  n_datasets = 5,
  design_type = "BySpecies",
  scenarios = val_scenarios,
  nsites = nsites,
  nvisits = nvisits,
  nspecies = nspecies,
  psi = psi,
  lambda = lambda,
  theta = test_theta1,
  save_datasets = FALSE,
  save_masked_datasets = FALSE,
  directory = paste0(here::here("Testing"))
)

summarize_n_validated(
  data_list = fake_data$masked_dfs,
  scenario_numbers = 1:nrow(fake_data$scenarios_df),
  theta_scenario = '1'
)

}
