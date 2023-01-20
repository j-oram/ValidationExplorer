# Function for running simulations. Required inputs are 
# data_list = nested list of masked datasets (datasets within scenarios -- see output of 
# get_sim_datasets.r)
#
# zeros_list = list of length 50 dataframes containing the site visits where no calls 
# were observed (under the corresponding theta classifier -- These are the noCounts from
# the get_sim_datasets.r and are needed for housekeeping).
#
# DGVs = data-generating values; needed to evaluate whether posterior intervals contain 
# the true value. This is expected to be a list with entries psi, lambda and theta containing 
# the respective parameters
#
# theta_scenario_id: keep track of which assumed classifier
#
# parallel: should the model be fit in parallel? 
# n_iter, nburn,thin: user-specified MCMC settings

library(nimble)
library(tidyverse)
source("runMCMC_fit.r")
source("MCMC_sum.r")

run_sims <- function(data_list, zeros_list, DGVs, theta_scenario_id, parallel = TRUE, 
                     niter = 20000, nburn = 12000, thin = 8) {
  
  # housekeeping
  ndatasets <- length(data_list[[1]])
  nscenarios <- length(data_list)
  
  # storage
  big_list <- list()
  
  # run scenarios
  for(scenario in 1:nscenarios){
    # progress
    message(paste0("Beginning scenario ", scenario, "."))
    message(Sys.time())
    
    # storage
    individual_summaries_list <- list()
    
    # progress
    pb <- txtProgressBar(min = 0, max = ndatasets, style = 3, width = 50, char = "=")
    for(dataset in 1:ndatasets){
      df7 <- data_list[[scenario]][[dataset]]
      zeros <- zeros_list[[dataset]]
      df8 <- bind_rows(df7, zeros) %>% arrange(site, visit, true_spp, id_spp)
      
      ## NIMBLE --------- 
      
      code <- nimbleCode({
        
        # priors
        for(species in 1:nspecies){
          
          psi[species] ~ dbeta(1,1)
          lambda[species] ~ T(dnorm(0, sd = 100), 0, Inf)
          theta[species, 1:nspecies] ~ ddirch(alpha = alpha0[species, 1:nspecies])
          
        }
        
        ##  --- Likelihood --- ##
        
        # state model
        for(i in 1:nsites){
          for(species in 1:nspecies){
            
            z[i, species] ~ dbern(psi[species])
            
          }
        }
        
        # encounter model: should be for all sites and visits
        for(i in 1:nsites){
          for(j in 1:nvisits){
            
            Y.[i,j] ~ dpois(sum(z[i,1:nspecies] * lambda[1:nspecies])) # Total number of calls from all spp
            
          }
        }
        
        # observation model: needs to be only for sites where we know that something happened (else sum(zlam) = 0 and
        # this likelihood specification doesn't make any sense)
        for(row in 1:total_calls){
          
          pi[row, 1:nspecies] <- z[site[row], 1:nspecies] * lambda[1:nspecies] /
            sum(z[site[row], 1:nspecies] * lambda[1:nspecies])
          k[row] ~ dcat(pi[row, 1:nspecies])
          y[row] ~ dcat(theta[k[row], 1:nspecies])
          
        }
        
      })
      
      # values passed to Nimble for indexing.
      constants <- list(
        site = df7$site, # Only sites where at least one call was made
        nspecies = n_distinct(df7$id_spp),
        nvisits = 4,
        nsites = n_distinct(df8$site), # we want to have a value for all possible site, even if that val is 0
        total_calls = nrow(df7)
      )
      
      # y = observed autoID, k = partially-observed true spp label
      # alpha0 = reference distance prior specification for classification pars
      # Y. = total number of calls observed (all spp) at each site-visit
      nimble_data <- list(
        y = df7$id_spp,
        k = df7$true_spp,
        alpha0 = matrix(0.2,
                        nrow = n_distinct(df7$id_spp),
                        ncol = n_distinct(df7$id_spp)),
        
        # Define Y. based on all site-visits, even if it had no calls
        Y. = bind_rows(df7, zeros) %>% 
          arrange(site, visit, true_spp, id_spp) %>% 
          group_by(site, visit) %>%
          summarize(total = unique(L.)) %>% # L. = leftover notation from Spiers et al., (2022)
          pivot_wider(
            names_from = visit,
            names_prefix = "visit",
            values_from = total,
            values_fill = 0 # if NA, turn into a 0, since the NA is due to no calls being detected at that site-visit
          ) %>%
          as.matrix()
      )
      
      # correct the dimensions: drop the extra column for site "grouping variable" 
      # Number of columns will need to change with number of visits. 
      nimble_data$Y. <- nimble_data$Y.[,2:5]
      
      alpha0 <- matrix(.2, length(unique(df8$id_spp)), length(unique(df8$id_spp)))
      df <- as.data.frame(df7)
      
      # randomly initialize lambda values near the observed autoID count for each spp
      # at each site-visit
      lambda_init <- rep(-1,5)
      naive_lambda <- as.data.frame(df) %>% 
        group_by(site, visit, id_spp, z) %>% 
        summarize(n = n()) %>% 
        filter(z ==1) %>% 
        ungroup() %>% 
        group_by(id_spp) %>% 
        summarise(naive_lam = mean(n)) %>% 
        select(naive_lam)
      
      while(any(lambda_init < 0)) lambda_init <- naive_lambda + rnorm(5)
      
      if(parallel){
        
        library(parallel)
        this_cluster <- makeCluster(3)
        fit <- parLapply(cl = this_cluster, 
                         X = 1:3, 
                         fun = runMCMC_fit, 
                         code = code,
                         data = nimble_data, 
                         lambda_init = lambda_init, 
                         constants = constants, 
                         inits = inits_fun,
                         alpha0 = alpha0, 
                         niter = niter,
                         nburn = nburn, 
                         thin = thin
        )
        stopCluster(this_cluster)
        
      } else {
        
        fit <- runMCMC_fit(code = code, df = df7, data = nimble_data, 
                           constants = constants, alpha0 = alpha0, 
                           inits = inits_fun, nchains = 3, 
                           niter = niter, nburn = nburn, thin = thin, seed = 1:3)
        
      }
      
      
      # save fit
      saveRDS(fit, paste0("Simulation/Theta", theta_scenario_id,"/fits/fit_", scenario, "_", dataset, ".rds"))
      
      # summarize fit
      scenario_tmp <- scenario
      dataset_tmp <- dataset
      fit_summary <- mcmc_sum(
        fit, truth = c(DGVs$lambda, DGVs$psi, DGVs$theta)
      ) %>%
        mutate(
          theta_scenario = theta_scenario_id,
          scenario = scenario_tmp,
          dataset = dataset_tmp
        )
      individual_summaries_list[[dataset]] <- fit_summary
      
      # output individual summary list for each dataset
      saveRDS(individual_summaries_list, paste0("Simulation/Theta", theta_scenario_id,"/individual_summaries/list_", scenario, ".rds"))
      
      # increment progress bar
      setTxtProgressBar(pb, dataset)
    }
    close(pb)
    
    # individual summary df after all datasets have been fit and summarized
    individual_summary_df <- do.call("bind_rows", individual_summaries_list)
    saveRDS(individual_summary_df, paste0("Simulation/Theta", theta_scenario_id,"/individual_summaries/df_", scenario, ".rds"))
    
    big_list[[scenario]] <- individual_summary_df
  }
  
  out <- do.call("bind_rows", big_list) # bind summary dfs for all scenarios into big df
  return(out)
}