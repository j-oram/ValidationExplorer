## Credit goes to CS for this -- this is essentially just a copy-paste of nimble_summary() 
# from the CoupledUncoupled repo

library(coda)
library(rstan)

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
