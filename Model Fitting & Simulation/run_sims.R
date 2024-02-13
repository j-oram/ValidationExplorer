# Function for running simulations as described in the main text. 
#
## ================== Inputs ================ ## 
# data_list: nested list of masked datasets (datasets within scenarios -- see output of 
# get_sim_datasets.r)
#
# zeros_list: list of length 50 dataframes containing the site visits where no calls 
# were observed (under the corresponding theta classifier -- These are the noCounts from
# the get_sim_datasets.r and are needed for housekeeping).
#
# DGVs: data-generating values; needed to evaluate whether posterior intervals contain 
# the true value. This is expected to be a list with entries psi, lambda and theta containing 
# the respective parameters
#
# theta_scenario_id: keep track of which assumed classifier
#
# parallel: should the model be fit in parallel? 
#
# n_iter, nburn,thin: user-specified MCMC settings as in runMCMC_fit.R
#
## ================ Outputs ================== ## 
#
# out: a dataframe with the summaries (from MCMC_sum.R) for all scenarios and datasets.

library(nimble)
library(tidyverse)

source("Model Fitting & Simulation/runMCMC_fit.r")
source("Model Fitting & Simulation/MCMC_sum.r")

run_sims <- function(data_list, zeros_list, DGVs, theta_scenario_id, 
                     parallel = TRUE, initialize_lambda_near_naive_val = FALSE,
                     niter = 2000, nburn = floor(niter/2), thin = 1, 
                     save_fits = FALSE,
                     save_individual_summaries_list = FALSE, 
                     directory = here::here()) {
  
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
      observed_df <- data_list[[scenario]][[dataset]] # was df7
      zeros <- zeros_list[[dataset]]
      all_sites_and_visits <- bind_rows(observed_df, zeros) %>% arrange(site, visit, true_spp, id_spp) # was df8
      
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
            
            Y._mat[i,j] ~ dpois(sum(z[i,1:nspecies] * lambda[1:nspecies])) # Total number of calls from all spp
            
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
        site = observed_df$site, # Only sites where at least one call was made
        nspecies = n_distinct(observed_df$id_spp),
        nvisits = n_distinct(observed_df$visit),
        nsites = n_distinct(all_sites_and_visits$site), # we want to have a value for all possible site, even if that val is 0
        total_calls = nrow(observed_df) # each row is a distinct recording
      )
      
      # y = observed autoID, k = partially-observed true spp label
      # alpha0 = reference distance prior specification for classification pars
      # Y. = total number of calls observed (all spp) at each site-visit
      nimble_data <- list(
        y = observed_df$id_spp,
        k = observed_df$true_spp,
        alpha0 = matrix(1/constants$nspecies,
                        nrow = n_distinct(observed_df$id_spp),
                        ncol = n_distinct(observed_df$id_spp)),
        
        # Define Y._matrix based on all site-visits, even if it had no calls. The 
        # matrix is nsites x nvisits in dimension.
        Y._mat = bind_rows(observed_df, zeros) %>% 
          arrange(site, visit, true_spp, id_spp) %>% 
          group_by(site, visit) %>%
          summarize(total = unique(Y.)) %>%
          pivot_wider(
            names_from = visit,
            names_prefix = "visit",
            values_from = total,
            values_fill = 0 # if NA, turn into a 0, since the NA is due to no calls being detected at that site-visit
          ) %>%
          ungroup() %>% 
          select(-site) %>% 
          as.matrix()
      )
      
      
      if(initialize_lambda_near_naive_val == TRUE){
        
        # randomly initialize lambda values near the observed autoID count for each spp
        # at each site-visit
        lambda_init <- rep(-1,5)
        naive_lambda <- as.data.frame(df) %>% 
          group_by(site, visit, id_spp, z) %>% 
          summarize(n = n()) %>% 
          filter(z == 1) %>% 
          ungroup() %>% 
          group_by(id_spp) %>% 
          summarise(naive_lam = mean(n)) %>% 
          select(naive_lam)
        
        while(any(lambda_init < 0)) lambda_init <- naive_lambda + rnorm(constants$nspecies)
        
      } else {
        
        lambda_init <- NULL
        
      }
      
      
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
                         niter = niter,
                         nburn = nburn, 
                         thin = thin
        )
        stopCluster(this_cluster)
        
      } else {
        
        fit <- runMCMC_fit(code = code, 
                           data = nimble_data, 
                           constants = constants,
                           lambda_init = lambda_init,
                           nchains = 3, 
                           niter = niter, 
                           nburn = nburn, 
                           thin = thin, 
                           seed = 1:3)
        
      }
      
      if (save_fits == TRUE){
        
        saveRDS(fit, paste0(directory, "/Theta", theta_scenario_id,"/fits/fit_", scenario, "_", dataset, ".rds"))
        
      }
      
      
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
      
      if (save_individual_summaries_list == TRUE){
        
        saveRDS(individual_summaries_list, paste0(directory, "/Theta", theta_scenario_id,"/individual_summaries/list_", scenario, ".rds"))
        
      }
      
      # increment progress bar
      setTxtProgressBar(pb, dataset)
    }
    close(pb)
    
    # summary df for the entire scenario after all datasets have been fit and summarized
    individual_summary_df <- do.call("bind_rows", individual_summaries_list)
    individual_summary_df$scenario <- scenario
    individual_summary_df$theta_scenario <- theta_scenario_id
    
    saveRDS(individual_summary_df, paste0(directory, "/Theta", theta_scenario_id,"/summary_df_for_scenario_", scenario, ".rds"))
    
    big_list[[scenario]] <- individual_summary_df
  }
  
  out <- do.call("bind_rows", big_list) # bind summary dfs for all scenarios into big df (all scenarios, all datasets)
  return(out)
}
