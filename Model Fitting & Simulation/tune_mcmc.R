
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
tune_mcmc <- function(dataset, zeros) {
  
  max_iters <- 10000
  
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
    
  print("Fitting MCMC in parallel ... ")
    library(parallel)
    this_cluster <- makeCluster(3)
    start <- Sys.time()
    fit <- parLapply(
      cl = this_cluster, 
      X = 1:3, 
      fun = runMCMC_fit, 
      code = code,
      data = nimble_data, 
      constants = constants, 
      niter = 1500,
      nburn = 500, 
      thin = 1
    )
    end <- Sys.time()
    stopCluster(this_cluster)
    
    ## :::::: Check whether all R-hat values are less than 1.1 ::::: ##
    
    warmups <- c(500, 1000, 2000, 5000)
    iters_to_check <- 1:5*1000
    
    M <- apply(cbind(warmups), 1, function(x) { # for each warmup value,
      
      V <- apply(cbind(iters_to_check), 1, function(y) { # for each iteration, 
        tmp1 <- lapply(fit, function(z) z[x:(x+y), ]) # look at each element in the fit (a list of 3), and subset it from the warmup to the warmup+iters
        Rhat <- vector(length = ncol(tmp1[[1]])) # create a vector that is the same length as the number of columns (variables) in the first element in the list of subsets. 
        for(k in 1:ncol(tmp1[[1]])) { # fill in the entries of the vector one by one with the Rhat values for each variable 
          tmp2 <- sapply(tmp1, function(a) a[,k])
          Rhat[k] <- Rhat(tmp2)
        }
        
        return(ifelse(all(Rhat <= 1.1), 1, 0)) # if all parameters have Rhat  < 1.1 for this number of iterations given the warmup, return 1
      }) # output is a vector of length 5; for this warmup, how many additional iterations must be completed to 
      
      return(V)
    })
    
    colnames(M) <- paste0("warmup = ", as.character(warmups))
    rownames(M) <- paste0("iters = ", as.character(iters_to_check))
    
    iter <- min(which(M == 1, arr.ind = TRUE)[, "row"])
    warmup_out <- warmups[min(which(M[iter, ] == 1,))]
    iter_out <- iters_to_check[iter]
    
    if(all(M == 0)) stop(message("Convergence was not reached in under 10,000 iterations. You must run chains for longer!"))
    
    return(list(max_iter_time = end-start, 
                min_warmup = warmup_out, 
                min_iter = iter_out, 
                convergence_matrix = M))
    
    
    
}

