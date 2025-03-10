% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_dat.R
\name{sim_dat}
\alias{sim_dat}
\title{Simulate data from the count-detection model with counts per site-visit}
\usage{
sim_dat(
  nsites = 100,
  nspecies = 8,
  nvisits = 4,
  seed = NULL,
  psi = stats::runif(nspecies, 0.4, 0.9),
  lambda = abs(stats::rnorm(nspecies, 0, 100)),
  theta = t(apply(18 * diag(nspecies) + 2, 1, function(x) nimble::rdirch(1, x)))
)
}
\arguments{
\item{nsites}{the number of sites assumed in the design. Default value is 100.}

\item{nspecies}{the number of species in the assemblage. Default is 8.}

\item{nvisits}{the number of visits (detector nights) assumed for each site. Default is 4.}

\item{seed}{optional seed if you would like to reproduce the data simulation.}

\item{psi}{a vector of length nspecies that contains the occurrence probabilities
for each species in the assemblage. These values must be in \[0,1\]. Default
is to draw a random vector from a U(.4, .9) distribution.}

\item{lambda}{vector of length nspecies that contains the relative activity
parameters for each species. Note these values need to be positive. By default,
lambda values are the absolute value of normal(0, 100) random variables.}

\item{theta}{n nspecies x nspecies matrix containing the (mis)classification
probabilities for each species. All entries must be in (0,1], with the rows
of the matrix summing to 1. The default draws rows from a dirichlet distribution
with concentrations determined by location in the matrix (diagonal values have
higher concentrations).}
}
\value{
A list containing \code{full_df}, a complete dataframe simulated under the
user's specified parameter settings. et_sim_datasets.R. The second list
element is \code{params}, the parameters used to simulate data in list form.
}
\description{
Simulate data from the count-detection model with counts per site-visit
}
\examples{
fake_data <- sim_dat(
   nsites = 30,
   nspecies = 2,
   nvisits = 3,
   seed = 101,
   psi = c(.3, .6),
   lambda = c(8, 3)
)

head (fake_data$full_df)
}
