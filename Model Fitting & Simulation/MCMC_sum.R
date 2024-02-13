## Function to summarize an MCMC fit from a probabilistic programming language 
# (PPL) such as NIMBLE, Stan, JAGS, etc.. The function leans on the summary 
#  function the rstan package for computing bulk and tail effective sample sizes 
# and for computing rhat values. 
# 
## ================= Inputs ================ ## 
# 
# out: a list of outputs from each chain in the MCMC fit. Each list entry should be a 
# matrix where each row corresponds to the iteration and each column corresponds to a 
# parameter. This is the default output format for NIMBLE objects, as well as samples 
# from other PPLs. 
# 
# thin: an integer value to thin draws by. Default value is 1. 
# 
# truth: a vector containing the true data-generating values for each model parameter
#
## ================ Outputs ================= ## 
#
# mcmc_summary: a dataframe object with each row corresponding to parameters and columns 
# corresponding to numeric summaries such as mean, sd, standard quantiles and MCMC 
# diagnostics. 

library(coda)
library(rstan)
library(tidyverse)

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
