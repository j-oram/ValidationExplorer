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

# KMB: commenting out to avoid rel working directory issues and promote good coding practice to load 
# all functions from one single file! 
# source("../Model Fitting & Simulation/runMCMC_fit.r")
# source("../Model Fitting & Simulation/MCMC_sum.r")

run_sims <- function(data_list, zeros_list, DGVs, theta_scenario_id, 
                     parallel = TRUE,
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
            lambda[species] ~ T(dnorm(0, sd = 10), 0, Inf)
            theta[species, 1:nspecies] ~ ddirch(alpha = alpha0[species, 1:nspecies])
            
          }
          
          ##  --- Likelihood --- ##
          
          ## Nothing changed here
          for(i in 1:nsites){
            for(species in 1:nspecies){
              
              z[i, species] ~ dbern(psi[species])
              
            }
          }
          
          
          for(i in 1:nsites){
            for(j in 1:nvisits){
              
              Y.[i,j] ~ dpois(sum(z[i,1:nspecies] * lambda[1:nspecies]))
              
            }
          }
          
          
          ## This code chunk is the likelihood contribution from the recordings where we observed 
          # both the autoID and the true species label
          for(row in 1:n_confirmed_calls){
            
            pi[row, 1:nspecies] <- z[site1[row], 1:nspecies] * lambda[1:nspecies] /
              sum(z[site1[row], 1:nspecies] * lambda[1:nspecies])
            
            k[row] ~ dcat(pi[row, 1:nspecies])
            y[row] ~ dcat(theta[k[row], 1:nspecies])
            
          }
          
          ## The next chunk is the product \prod_{l_{ij}: I_{l_{ij}} =0}, with the unobserved 
          # true species labels summed out. 
          for(row in 1:n_ambiguous_calls){
            
            pi2[row, 1:nspecies] <- z[site2[row], 1:nspecies] * lambda[1:nspecies] /
              sum(z[site2[row], 1:nspecies] * lambda[1:nspecies])
            y2[row] ~ dmarginal_autoID(theta_mat = theta[1:nspecies, 1:nspecies], 
                                       pi = pi2[row, 1:nspecies])
            
          }
          
        })
        
        
        # Define the ambiguous and unambiguous datasets
        amb <- observed_df[is.na(observed_df$true_spp), ]
        uamb <- observed_df[!is.na(observed_df$true_spp), ]
        
        
        # values passed to Nimble for indexing.
        constants <- list(
          site1 = uamb$site, # Only sites where at least one call was made
          site2 = amb$site,
          nspecies = n_distinct(observed_df$id_spp),
          nvisits = n_distinct(observed_df$visit),
          nsites = n_distinct(all_sites_and_visits$site), # we want to have a value for all possible site, even if that val is 0
          n_confirmed_calls = nrow(uamb), 
          n_ambiguous_calls = nrow(amb)
        )
        
        # y = observed autoID, k = observed true spp label
        # y2 = observed autoIDs from ambiguous data, k2 = unobserved true label
        # alpha0 = reference distance prior specification for classification pars
        # Y. = total number of calls observed (all spp) at each site-visit
        nimble_data <- list(
          y = uamb$id_spp,
          k = uamb$true_spp,
          y2 = amb$id_spp,
          alpha0 = matrix(1/(constants$nspecies),
                          nrow = n_distinct(all_sites_and_visits$id_spp),
                          ncol = n_distinct(all_sites_and_visits$id_spp)),
          
          # Define Y. based on all site-visits, even if it had no calls
          Y. = all_sites_and_visits %>% 
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
      
      
      if(parallel){
        
        library(parallel)
        this_cluster <- makeCluster(3)
        fit <- parLapply(cl = this_cluster, 
                         X = 1:3, 
                         fun = runMCMC_fit, 
                         code = code,
                         data = nimble_data, 
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
                           nchains = 3, 
                           niter = niter, 
                           nburn = nburn, 
                           thin = thin, 
                           seed = 1:3)
        
      }
      
      if (save_fits == TRUE){
        # if directory/ThetaScenarioID/fits does not exist, create it
        if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id), "fits"))) {
          dir.create(file.path(directory, paste0("Theta", theta_scenario_id), "fits"), recursive = TRUE)
        }
        
        saveRDS(
          fit, 
          file=file.path(directory, paste0("Theta", theta_scenario_id),"fits", paste0("fit_", scenario, "_", dataset, ".rds"))
        )
        
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
        # if directory/ThetaScenarioID/individual_summaries does not exist, create it
        if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id), "individual_summaries"))) {
          dir.create(file.path(directory, paste0("Theta", theta_scenario_id), "individual_summaries"), recursive = TRUE)
        }
        saveRDS(
          individual_summaries_list, 
          file=file.path(directory, paste0("Theta", theta_scenario_id),"individual_summaries", paste0("list_", scenario, ".rds"))
        )
        
      }
      
      # increment progress bar
      setTxtProgressBar(pb, dataset)
    }
    close(pb)
    
    # summary df for the entire scenario after all datasets have been fit and summarized
    individual_summary_df <- do.call("bind_rows", individual_summaries_list)
    individual_summary_df$scenario <- scenario
    individual_summary_df$theta_scenario <- theta_scenario_id
    
    # make sure that the ThetaSCENARIOID folder is available for saving
    if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id)))) {
      dir.create(file.path(directory, paste0("Theta", theta_scenario_id)), recursive = TRUE)
    }
    
    saveRDS(
      individual_summary_df, 
      file=file.path(directory, paste0("Theta", theta_scenario_id), paste0("summary_df_for_scenario_", scenario, ".rds"))
    )
    
    big_list[[scenario]] <- individual_summary_df
  }
  
  out <- do.call("bind_rows", big_list) # bind summary dfs for all scenarios into big df (all scenarios, all datasets)
  return(out)
}
