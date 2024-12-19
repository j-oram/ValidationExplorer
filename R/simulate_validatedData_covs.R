simulate_validatedData_covs <- function(n_datasets,
                                   design_type = c("BySpecies", "FixedPercent"),
                                   FE_type = NULL,
                                   scenarios,
                                   nsites,
                                   nspecies,
                                   nvisits,
                                   psi = runif(nspecies),
                                   alpha  = matrix(rnorm(nspecies*2), 2, nspecies),
                                   theta = t(apply(18*diag(nspecies)+2, 1, function(x) rdirch(1, x))),
                                   save_datasets = FALSE,
                                   save_masked_datasets = FALSE,
                                   directory = here::here()){
  
  # check the users's specified classifier
  if(any(round(rowSums(theta), 5) != 1)) {
    stop("The rows of theta do not sum to 1.")
  }
  
  # check the length of scenarios under BySpecies validation design
  if(design_type == "BySpecies" & length(scenarios) != nspecies) {
    stop("Scenarios must be a list with length = nspecies.")
  }
  
  # if the requisite directories don't exist, create them.
  if(!dir.exists(file.path(directory)) & (save_datasets | save_masked_datasets)){
    dir.create(file.path(directory))
  }
  if(save_datasets & !dir.exists(file.path(directory, "datasets"))) {
    dir.create(file.path(directory, "datasets"), recursive = TRUE)
    dir.create(file.path(directory, "zeros"), recursive = TRUE)
  }
  if(save_masked_datasets & !dir.exists(file.path(directory, "masked_datasets"))) {
    dir.create(file.path(directory, "masked_datasets"), recursive = TRUE)
  }
  
  # Quiet the chatter
  options(dplyr.summarize.inform = FALSE)
  
  # Initialize storage lists
  datasets_list <- list()
  zeros <- list()
  
  for(m in 1:n_datasets){
    
    # sim_dat simulates from the (aggregated) count-detection model as specified in
    # Stratton et al., (2022). To obtain the disaggregated dataset, we use
    # tidyr::uncount below
    
    
    
     sims <- sim_CD_with_covs(nsites = nsites,
                            nspecies = nspecies,
                            nvisits = nvisits,
                            alpha = alpha,
                            psi = psi,
                            seed = m,
                            mask = FALSE,
                            masking_function = FE_type,
                            theta = theta)
    
  
    # save the full dfs 
    datasets_list[[m]] <- sims$full_df
    
    # Store the zeros in a list for houskeeping
    zeros[[m]] <- sims$zeros
    
    # If user wants individual rds files for each dataframe, save them and the zeros
    # in the specified directory
    if(save_datasets == TRUE){
      
      saveRDS(datasets_list[[m]], file = file.path(directory, "datasets", paste0("dataset_", m, ".rds")))
      saveRDS(zeros[[m]],
              file = file.path(directory, "zeros", paste0("zeros_in_dataset_", m, ".rds")))
    }
    
  }
  
  ######## Mask datasets generated above #########
  
  # set up storage for the masked datasets
  masked_dataset_list <- list()
  
  if(design_type == "BySpecies"){
    
    scenarios <- expand.grid(scenarios)
    
    # loop over scenarios and datasets, saving each masked dataset
    # Note that for this design, `scenarios` is a dataframe object
    for(s in 1:nrow(scenarios)){
      masked_dataset_list[[s]] <- list()
      for(d in 1:length(datasets_list)){
        
        masked_df <- mask_by_spp(datasets_list[[d]], scenarios[s,])$final_df
        masked_df$scenario <- s
        
        if(save_masked_datasets == TRUE){
          
          saveRDS(masked_df,
                  file = file.path(directory, "masked_datasets", paste0("dataset_", d, "_masked_under_BSV_scenario_", s, ".rds")))
        }
        
        masked_dataset_list[[s]][[d]] <- masked_df
        
      }
    }
    
    # add a column for the scenario number to make it easy to see which
    # LOVE for each species goes with which number on x-axis of plots
    scenarios <- scenarios %>%
      tibble::rownames_to_column(var = "scenario")
    
  } else {
    
    # Loop over the specified vector of percentages
    # Note: here `scenarios` is a vector!
    for(s in 1:length(scenarios)){
      masked_dataset_list[[s]] <- list() # initiate interior storage for each scenario (i.e., for each distinct percentage)
      for(d in 1:length(datasets_list)) { # loop over the datasets list, masking each according to the scenario
        
        if (FE_type == "all_visits") {
          masked_df <- mask_FE_all_visits(
            df = datasets_list[[d]], 
            effort_prop = scenarios[s], 
            seed = d + 1
          ) %>% 
            dplyr::mutate(scenario = s)
        } else if (FE_type == "random_visit") {
          masked_df <- mask_sampled_visit(
            datasets_list[[d]], 
            effort_prop = scenarios[s], 
            seed = d + 1
          ) %>% 
            dplyr::mutate(scenario = s)
        } else {
          print("You must choose one of the following masking function options: all_visits or random_visit.")
        }
        
        if(save_masked_datasets == TRUE) {
          
          saveRDS(
            masked_df,
            file = file.path(directory, "masked_datasets", paste0("dataset_", d, "_masked_under_FE_scenario_", s, ".rds"))
          )
        }
        
        masked_dataset_list[[s]][[d]] <- masked_df
        
      }
    }
    
  }
  
  # If the validation design is by-species, also return the scenarios dataframe
  # created by the expand.grid call above. This is potentially useful for users
  # to see exactly which scenarios are being investigated
  if(design_type == "BySpecies") {
    output <- list(
      full_datasets = datasets_list,
      zeros = zeros,
      masked_dfs = masked_dataset_list,
      scenarios_df = scenarios
    )
  } else {
    output <- list(
      full_datasets = datasets_list,
      zeros = zeros,
      masked_dfs = masked_dataset_list
    )
  }
  
  return(output)
  
}
