#' MCMC_sum: A custom function for summarizing MCMC posterior sampling
#'
#' @param out Draws from a model fit using a probabilistic programming language
#'   (e.g., Stan, NIMBLE or JAGS). The expected format of this input is a list,
#'   where each entry is a Markov chain.
#' @param thin An optional thinning interval.
#' @param truth A vector with the true parameter values organized alphanumerically
#'   by parameter value (e.g., lambda\\[1\\], lambda\\[2\\], psi\\[1\\], psi\\[2\\], theta\\[1,1\\],
#'   theta\\[1,2\\], theta\\[2,1\\], theta\\[2,2\\])
#'
#' @return A dataframe object summarizing the MCMC draws, including diagnostics,
#'   quantiles and posterior means.
#' @export
#'
#' @examples
#'
#' # An example fit of one dataset
#' draws <- ValExp_example_fit
#'
#' # The data generating values
#' truth <- c(11,2,0.3, 0.6, 0.9, 0.15, 0.10, 0.85)
#'
#' mcmc_sum(draws, truth = truth)

mcmc_sum <- function(out, thin = 1, truth){

# Convert to mcmc list
mcmc.list <- coda::as.mcmc.list(
  lapply(out,
         function(x) coda::as.mcmc(
           x,
           start = floor(nrow(out[[1]])/2),
           end = nrow(out[[1]]),
           thin = thin
         )
  )
)

# Compute summary of the mcmc list, define parameters and create a matrix with
# values from these
sum <- summary(mcmc.list)
params <- dimnames(sum$statistics)[[1]]
tmp_sum <- cbind(sum$statistics, sum$quantiles)

# Create a matrix to hold the MCMC diagnostic statistics
m <- matrix(nrow = nrow(tmp_sum), ncol = 3)
colnames(m) <- c("Rhat", "ess_bulk", "ess_tail")

# For each parameter, compute the Rhat, ess_bulk and ess_tail
for(i in 1:nrow(tmp_sum)){
  tmp <- sapply(out, function(x) x[,i])
  m[i,] <- c(rstan::Rhat(tmp), rstan::ess_bulk(tmp), rstan::ess_tail(tmp))
}

# Create the dataframe with statistics, quantiles and MCMC diagnostics
mcmc_summary <- cbind(tmp_sum, m) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "parameter")

# Add the true values that are specified by the user and summarize whether each 
# 95% interval captures the true value and whether the model coverged based on 
# Rhat criteria
mcmc_summary$truth <- truth
mcmc_summary$capture <- ifelse(mcmc_summary$`2.5%` <= mcmc_summary$truth &
                                 mcmc_summary$`97.5%` >= mcmc_summary$truth,
                               1, 0)
mcmc_summary$converge <- ifelse(mcmc_summary$Rhat <= 1.1, 1,0)


return(mcmc_summary)

}
