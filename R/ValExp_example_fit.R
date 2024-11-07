#' ValExp_example_fit
#'
#' Draws from an MCMC algorithm fit to simulated data. This object exists solely
#' for easing examples in documentation of \link{mcmc_sum}.
#'
#'
#'
#' \itemize{
#'   \item{Columns: Parameters (lambda, psi, and theta for a two species assemblage)}
#'   \item{Rows; Iterations of the MCMC. There are 1000 post-warmup draws for each.}
#' }
#' @docType data
#' @name ValExp_example_fit
#' @format A list of length 3 matrices with columns and rows as described here.
#' @source Simulated data using \link{simulate_validatedData}, then isolated one
#'   dataset and the corresponding zeros. Draws were obtained from model fitting
#'   using the code inside \link{tune_mcmc}.
#'
#' @examples
#' str(ValExp_example_fit)
NULL
