% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_vs_calls.R
\name{plot_bias_vs_calls}
\alias{plot_bias_vs_calls}
\title{plot_bias_vs_calls: Compare validation designs based on estimation error and
expected level of effort}
\usage{
plot_bias_vs_calls(
  sim_summary,
  calls_summary,
  pars = NULL,
  regex_pars = NULL,
  theta_scenario,
  scenarios,
  convergence_threshold = 1.1
)
}
\arguments{
\item{sim_summary}{Simulation summary from many datasets under many validation
scenarios in the format output by \link{mcmc_sum}.}

\item{calls_summary}{Summary of the validation design in the format as output
from \link{summarize_n_validated}}

\item{pars}{A character vector of parameters to visualize.}

\item{regex_pars}{String containing the name of a group of parameters to visualize.
Must be one of "lambda", "psi", or "theta".}

\item{theta_scenario}{String containing the theta scenario ID that
was used to run simulations in \link{run_sims}. This string must match
between \code{calls_summary} and \code{sim_summary}.}

\item{scenarios}{A vector of integers corresponding to the validation designs
you would like to visualize.}

\item{convergence_threshold}{A threshold for the Gelman-Rubin statistic; values
below this threshold indicate that a parameter has converged.}
}
\description{
plot_bias_vs_calls: Compare validation designs based on estimation error and
expected level of effort
}
\examples{

sim_summary <- example_output
calls_summary <- example_val_sum

plot_bias_vs_calls(sim_summary, calls_summary, regex_pars = "lambda",
                   theta_scenario = "1", scenarios = 1:2,
                   convergence_threshold = 1.05)

}
