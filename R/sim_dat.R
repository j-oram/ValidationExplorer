#' Simulate data from the count-detection model with counts per site-visit
#'
#' @param nsites the number of sites assumed in the design. Default value is 100.
#' @param nspecies the number of species in the assemblage. Default is 8.
#' @param nvisits the number of visits (detector nights) assumed for each site. Default is 4.
#' @param seed optional seed if you would like to reproduce the data simulation.
#' @param psi  a vector of length nspecies that contains the occurrence probabilities
#'   for each species in the assemblage. These values must be in \\[0,1\\]. Default
#'  is to draw a random vector from a U(.4, .9) distribution.
#' @param lambda vector of length nspecies that contains the relative activity
#'   parameters for each species. Note these values need to be positive. By default,
#'   lambda values are the absolute value of normal(0, 100) random variables.
#' @param theta n nspecies x nspecies matrix containing the (mis)classification
#'   probabilities for each species. All entries must be in (0,1], with the rows
#'   of the matrix summing to 1. The default draws rows from a dirichlet distribution
#'   with concentrations determined by location in the matrix (diagonal values have
#'   higher concentrations).
#'
#' @return A list containing `full_df`, a complete dataframe simulated under the
#'   user's specified parameter settings. et_sim_datasets.R. The second list
#'   element is `params`, the parameters used to simulate data in list form.
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' fake_data <- sim_dat(
#'    nsites = 30,
#'    nspecies = 2,
#'    nvisits = 3,
#'    seed = 101,
#'    psi = c(.3, .6),
#'    lambda = c(8, 3)
#' )
#'
#' head (fake_data$full_df)
sim_dat <- function(
  nsites = 100, nspecies = 8, nvisits = 4, seed = NULL,
  psi = stats::runif(nspecies, .4, .9),
  lambda = abs(stats::rnorm(nspecies, 0, 100)),
  theta = t(apply(18*diag(nspecies)+2, 1, function(x) nimble::rdirch(1, x)))
){

  # optional seed
  if(!is.null(seed)) set.seed(seed)

  # build empty df
  df <- dplyr::tibble(
    site = rep(1:nsites, each = nspecies * nspecies * nvisits),
    visit = rep(rep(1:nvisits, each = nspecies * nspecies), nsites),
    true_spp = rep(1:nspecies, nsites * nvisits * nspecies),
    id_spp = rep(rep(1:nspecies, each = nspecies), nsites * nvisits)
  )

  # add lambda, psi and theta values
  df2 <- dplyr::left_join(
    df,
    dplyr::tibble(
      true_spp = 1:nspecies,
      lambda = lambda,
      psi = psi
    ), by = "true_spp"
  ) %>%
    dplyr::left_join(
      .,
      dplyr::tibble(
        true_spp = rep(1:nspecies, nspecies),
        id_spp = rep(1:nspecies, each = nspecies),
        theta = c(theta)
      ), by = c("true_spp", "id_spp")
    )

  # add latent z state and counts for each true-autoID pair at each site-night
  df3 <- df2 %>%
    dplyr::select(dplyr::all_of(c('site', 'true_spp', 'psi'))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(z = stats::rbinom(dplyr::n(), size = 1, prob = .data$psi)) %>%
    dplyr::left_join(
      df2,
      .,
      by = c("site", "true_spp", "psi")
    ) %>%
    dplyr::mutate(
    # cijkk' = count of detections truly belonging to spp k that are autoID'd as k'
      # on visit j to site i
      count = stats::rpois(dplyr::n(), .data$z * .data$lambda * .data$theta)
    )
  full_df <- df3 # Output required for the data simulation contained in simulate_BySpeciesValidation.R

  out <- list(full_df = full_df, params = list(psi = psi, theta = theta, lambda = lambda))
  return(out)
}

