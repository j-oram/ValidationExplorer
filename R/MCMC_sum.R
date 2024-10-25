#' MCMC_sum
#'
#' @param out A list of outputs from each chain in the MCMC fit. Each list entry should be a matrix where each row corresponds to the iteration and each column corresponds to a parameter. This is the default output format for NIMBLE objects, as well as samples from other PPLs.
#' @param thin An integer value to thin draws by. Default value is 1.
#' @param truth a vector containing the true data-generating values for each model parameter
#'
#' @return a dataframe object with each row corresponding to parameters and columns corresponding to numeric summaries such as mean, sd, standard quantiles and MCMC diagnostics.

#' @export
#'
#' @examples
#'
#' library(rstan)
#' library(coda)
#' library(dplyr)
#'
#' x <- runif(30, -1, 1)
#' beta <- -1
#' y <- rnorm(x*beta, sd = 1)
#'
#' code <- nimbleCode({
#'   # priors
#'   beta ~ dnorm(0, 2)
#'   sigma ~ T(dnorm(0, 2), 0, Inf)
#'
#'   # likelihood
#'   for(i in 1:n) {
#'     y[i] ~ dnorm(x[i]*beta, sd = sigma)
#'   }
#' })
#'
#' fit <- nimbleMCMC(code,
#'   constants = list(n = length(y)),
#'   data = list(x = x, y = y),
#'   inits = list(beta = rnorm(1, 0, 10), sigma = abs(rnorm(0, 10))),
#'   nchains = 3
#' )
#'
#'mcmc_sum(fit, truth = c(beta, 1))
#'
mcmc_sum <- function(out, thin = 1, truth){

mcmc.list <- as.mcmc.list(
  lapply(out,
         function(x) as.mcmc(
           x,
           start = floor(nrow(out[[1]])/2),
           end = nrow(out[[1]]),
           thin = thin
         )
  )
)

sum <- summary(mcmc.list)
params <- dimnames(sum$statistics)[[1]]
tmp_sum <- cbind(sum$statistics, sum$quantiles)

m <- matrix(nrow = nrow(tmp_sum), ncol = length(out))
colnames(m) <- c("Rhat", "ess_bulk", "ess_tail")

for(i in 1:nrow(tmp_sum)){
  tmp <- sapply(out, function(x) x[,i])
  m[i,] <- c(Rhat(tmp), ess_bulk(tmp), ess_tail(tmp))
}

mcmc_summary <- cbind(tmp_sum, m) %>%
  as.data.frame() %>%
  rownames_to_column(var = "parameter")

mcmc_summary$truth <- truth
mcmc_summary$capture <- ifelse(mcmc_summary$`2.5%` <= mcmc_summary$truth &
                                 mcmc_summary$`97.5%` >= mcmc_summary$truth,
                               1, 0)
mcmc_summary$converge <- ifelse(mcmc_summary$Rhat <= 1.1, 1,0)


return(mcmc_summary)

}
