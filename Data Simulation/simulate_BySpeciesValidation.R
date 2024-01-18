simulate_BySpeciesValidation <- function(n_datasets, 
                                         scenarios_dataframe, 
                                         nsites, 
                                         nspecies, 
                                         nvisits, 
                                         psi, 
                                         lambda, 
                                         theta, 
                                         save_datasets = TRUE, 
                                         save_masked_datasets = TRUE,
                                         directory = here::here()){
  
  source("Data Simulation/count_detection_sim.R")
  source("Data Simulation/mask_by_spp.R")
    
  # Initialize storage lists
  datasets_list <- list()
  site_visits_without_calls <- list()
    
    for(m in 1:n_datasets){
      
      # sim_dat simulates from the (aggregated) count-detection model as specified in 
      # Stratton et al., (2022). To obtain the disaggregated dataset, we use 
      # tidyr::uncount 
      aggregate_CD <- sim_dat(nsites = nsites,
                      nspecies = nspecies,
                      nvisits = nvisits,
                      lambda = lambda,
                      psi = psi,
                      seed = m,
                      theta = theta)$full_df
      
      # Y. = total calls at each site-visit (L. in the notation of Spiers et el., 2022).
      agg_CD_with_total <- aggregate_CD %>% 
        group_by(site, visit) %>% 
        mutate(Y. = sum(count))
      
      # Disaggregated count detection data gets stored in mth entry of the 
      # datasets_list
      datasets_list[[m]] <- agg_CD_with_total %>% 
        uncount(., weights = count, .remove = FALSE)
      
      # Store the zeros in a list for houskeeping 
      site_visits_without_calls[[m]] <- agg_CD_with_total %>% filter(count == 0)
      
      # If user wants individual rds files for each dataframe, save them and the zeros
      # in the specified directory
      if(save_datasets == TRUE){
        saveRDS(datasets_list[[m]], file = paste0(directory, "/dataset_", m, ".rds"))
        saveRDS(site_visits_without_calls[[m]], 
                file = paste0(directory, "/site_visits_without_calls_in_dataset_", m, ".rds"))
      }  
      
    }
  
  ######## Mask datasets generated above #########
  
  # set up storage for the masked datasets
  masked_dataset_list <- list()
  
  # loop over scenarios and datasets, saving each masked dataset and 
  for(s in 1:nrow(scenarios_dataframe)){
    masked_dataset_list[[s]] <- list()
    for(d in 1:length(datasets_list)){
      
      masked_df <- mask_spp2(datasets_list[[d]], scenarios_dataframe[s,])$final_df
      masked_df$scenario <- s
      
      if(save_masked_datasets == TRUE){
        saveRDS(masked_df, 
                file = paste0(directory, "/dataset_", d, "_masked_under_scenario_", s, ".rds"))
      }
      
      masked_dataset_list[[s]][[d]] <- masked_df
      
    }
  }
    
  return(list(full_datasets = datasets_list, 
              zeros = site_visits_without_calls, 
              masked_dfs = masked_dataset_list))
  
}
