library(nimble)
library(tidyverse)

# rds objects
test_data_list <- readRDS("Testing/masked_test_dfs.rds") 
test_zeros_list <- readRDS("Testing/test_zeros_list.rds") 
test_DGVs <- readRDS("Testing/test_DGVs.rds") 

# functions
source("Model Fitting & Simulation/runMCMC_fit.R")
source("Model Fitting & Simulation/MCMC_sum.R")

inits_fun <- function(){
  
  out <- list(
    psi = runif(constants$nspecies, min = 0.1, max = .5),
    lambda = abs(rnorm(constants$nspecies, mean = 0, sd = 10)),
    theta = t(apply(alpha0, 1, function(x) rdirch(1,x))),
    z = matrix(1, nrow = constants$nsites, ncol = constants$nspecies)
  )
  
  return(out)
  
}

# convenience
options(dplyr.summarise.inform = FALSE)

# simulation function
run_sims <- function(data_list, zeros_list, DGVs, theta_scenario_id, parallel = TRUE) {
  
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
            
            L.[i,j] ~ dpois(sum(z[i,1:nspecies] * lambda[1:nspecies])) # Total number of calls from all spp
            
          }
        }
        
        # observation model: needs to be only for sites where we know that something happened (else sum(zlam) = 0 and
        # this likelihood specification doesn't make any sense)
        for(row in 1:Ltotal){
          
          pi[row, 1:nspecies] <- z[site[row], 1:nspecies] * lambda[1:nspecies] /
            sum(z[site[row], 1:nspecies] * lambda[1:nspecies])
          k[row] ~ dcat(pi[row, 1:nspecies])
          y[row] ~ dcat(theta[k[row], 1:nspecies])
          
        }
        
      })
      
      
      constants <- list(
        site = df7$site, # Only sites where at least one call was made
        nspecies = n_distinct(df7$id_spp),
        nvisits = 4,
        nsites = n_distinct(df8$site), # we want to have a value for all possible site, even if that val is 0
        Ltotal = nrow(df7)
      )
      
      
      nimble_data <- list(
        y = df7$id_spp,
        k = df7$true_spp,
        alpha0 = matrix(0.2,
                        nrow = n_distinct(df7$id_spp),
                        ncol = n_distinct(df7$id_spp)),
        
        # Define L. based on all site-visits, even if it had no calls
        L. = bind_rows(df7, zeros) %>% 
          arrange(site, visit, true_spp, id_spp) %>% 
          group_by(site, visit) %>%
          summarize(total = unique(L.)) %>%
          pivot_wider(
            names_from = visit,
            names_prefix = "visit",
            values_from = total,
            values_fill = 0 # if NA, turn into a 0, since the NA is due to no calls being detected at that site-visit
          ) %>%
          as.matrix()
      )
      
      # # correct the dimensions: drop the extra column for site "grouping variable" 
      # # Number of columns will need to change with number of visits. 
      nimble_data$L. <- nimble_data$L.[,2:5]
      
      alpha0 <- matrix(.5, length(unique(df8$id_spp)), length(unique(df8$id_spp)))
      
      lambda_init <- rep(-1,5)
      naive_lambda <- as.data.frame(df8) %>% 
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
                                  constants = constants, 
                                  inits = inits_fun,
                                  lambda_init = lambda_init,
                                  alpha0 = alpha0
        )
        stopCluster(this_cluster)
        
      } else {
        
        fit <- runMCMC_fit(code = code, data = nimble_data, 
                                    constants = constants, alpha0 = alpha0, 
                                    inits = inits_fun, nchains = 3, seed = 1:3)
        
      }
      
      
      # save fit
      saveRDS(fit, paste0("Testing/simulation/fits/fit_", scenario, "_", dataset, ".rds"))
      
      # summarize fit
      scenario_tmp <- scenario
      dataset_tmp <- dataset
      fit_summary <- mcmc_sum(
        fit, truth = c(lambda = DGVs$lambda, psi = DGVs$psi, theta = DGVs$theta)
      ) %>%
        mutate(
          theta_scenario = theta_scenario_id,
          scenario = scenario_tmp,
          dataset = dataset_tmp
        )
      individual_summaries_list[[dataset]] <- fit_summary
      
      # output individual summary list sometimes
      saveRDS(individual_summaries_list, paste0("Testing/simulation/individual_summaries/list_", scenario, ".rds"))
      
      # increment progress bar
      setTxtProgressBar(pb, dataset)
    }
    close(pb)
    
    # individual summary df
    individual_summary_df <- do.call("bind_rows", individual_summaries_list)
    saveRDS(individual_summary_df, paste0("Testing/simulation/individual_summaries/df_", scenario, ".rds"))
    
    big_list[[scenario]] <- individual_summary_df
  }
  
  out <- do.call("bind_rows", big_list)
  return(out)
}

# run sims
theta_scenario_1 <- run_sims(
  data_list = test_data_list,
  zeros_list = test_zeros_list,
  DGVs = test_DGVs,
  theta_scenario_id = "1"
)

saveRDS(theta_scenario_1, "Testing/simulation/theta_scenario_1.rds") # This is the 'results' object used for visualization