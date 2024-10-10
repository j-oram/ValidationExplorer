
# The below doesn't work if we don't have a list of datasets or zeros. 

# test <- run_sims(data_list = sim_dat$full_datasets[[1]][[4]], 
#          zeros_list = sim_dat$zeros[[4]], 
#          DGVs = list(
#            psi = c(0.6331, 0.6122, 0.8490, 0.6972),
#            lambda = c(5.9347, 4.1603, 14.2532, 6.1985),
#            theta = c(test_theta2)
#          ), 
#          theta_scenario_id = "1"
#         )

         
# A function that evaluates the $rhat after every thousand iterations
tune_mcmc <- function(dataset, zeros, ) {
    
  ## :::::::::::::::: fit MCMC with max_iters :::::::::::::::::: ##
  observed_df <- dataset
  all_sites_and_visits <- bind_rows(observed_df, zeros) %>% 
    arrange(site, visit, true_spp, id_spp) 
  
  ## NIMBLE --------- ##
  
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
    
    library(parallel)
    nburn = max_iters/2
    this_cluster <- makeCluster(3)
    start <- Sys.time()
    fit <- parLapply(
      cl = this_cluster, 
      X = 1:3, 
      fun = runMCMC_fit, 
      code = code,
      data = nimble_data, 
      constants = constants, 
      niter = max_iters,
      nburn = 0, 
      thin = 1
    )
    end <- Sys.time()
    stopCluster(this_cluster)
    
    ## :::::: Check whether all R-hat values are less than 1.1 ::::: ##
    
    warmups <- c(500, 1000, 2000, 5000)
    iters_to_check <- 1:5*1000
    
    i <- 1
    j <- 1
    
    while(i<=length(warmups) & j<=length(iters_to_check)){
      
      tmp1 <- lapply(fit, function(x) x[warmups[i]:warmups[i]+iters_to_check[j], ])
      Rhat <- vector(length = ncol(tmp1[[1]]))
      
      for(k in 1:ncol(tmp1[[1]])){
        tmp2 <- sapply(tmp1, function(x) x[,k])
        Rhat[k] <- Rhat(tmp2)
      }
      
      if(all(Rhat <= 1.1)) break 
      else j <- j+1
      
    }
    
    
    
    
    
    
    #max_check <- floor((max_iters - nburn)/1000)
    checkpoint <- 1
    iter <- NULL
    all_converged <- NULL
    
    while(checkpoint <= max_check) {
      max_rows <- checkpoint * 1000
      tmp1 <- lapply(fit, function(x) x[1:max_rows, ])
      Rhat <- vector(length = ncol(tmp1[[1]]))
      for(i in 1:ncol(tmp1[[1]])){
        tmp2 <- sapply(tmp1, function(x) x[,i])
        Rhat[i] <- Rhat(tmp2)
      }
      
      all_converged <- c(all_converged, ifelse(all(Rhat < 1.1), 1, 0))
      iter <- c(iter, max_rows)
      checkpoint <- checkpoint + 1
    }
      
      
    
    converged_tbl = tibble(
      iter = iter, 
      all_converged = all_converged
    )
    
    converged_tbl
  
    
    return(list(max_iter_time = end-start, 
                converged_tbl = converged_tbl, 
                warmup = nburn))
    
    
    
}

