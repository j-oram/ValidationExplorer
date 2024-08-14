## Function for simulating data using either a stratified-by-species validation design or a 
# fixed effort design. 
# For more details, see Vignette.pdf in this repo! 

# ====================== Inputs ========================== #
#
# n_datasets: the number of datasets you would like to have simulated. Each of these 
# simulated datasets will be subjected to all validation designs specified in the scenarios_dataframe
#
# validation_design: Character string, either "BySpecies" for a stratified-by-species design,
# or "FixedPercentage" for a fixed effort design (see Oram et al., 2024 for more details on each of these)
# 
# scenarios: if validation_design == "BySpecies", `scenarios` argument must be a dataframe with each row corresponding 
# to a specific validation scenario. The columns contain the level of effort for each autoID label. If 
# validation_design == "FixedPercent", then `scenarios` argument must be a vector with each entry corresponding to 
# a potential percent of calls to be sampled from the first visit at each site. See vignette for an example. 
# 
# nsites: number of sites in each dataset
#
# nspecies: size of the species assemblage
#
# nvisits: the number of visits to each site. Note that these simulations assume a balanced design.
#
# psi: a vector of length nspecies with the assumed occurrence probabilities for each species
# 
# lambda: a vector of length nspecies with the assumed relative activity levels for each species. 
# Make sure the order is correct and matches psi. 
#
# theta: a matrix containing the (mis)classification probabilities. The rows of this matrix must sum
# to 1. See vignette for an example.
# 
# save_datasets: logical. If TRUE (default), the datasets without any masking of true species labels (i.e., 
# corresponding to complete validation of all recordings) will be saved. 
# 
# save_masked_datasets: logical. If TRUE (default), the masked datasets (i.e., the simulated datasets with 
# partial validation according to the simulation scenario) will be saved. This means that there will be 
# n_datasets x nrow(scenarios_dataframe) datasets saved: one for each dataset under each validation scenario.
#
# directory: character. Required if save_datasets = TRUE or save_masked_datasets = TRUE. This is where the 
# datasets will be saved. By default, the current working directory (i.e., here::here()) will be used.

# ====================== Output ========================== # 

# The output of this function is a list containing the following items: 
# 
# full_datasets: A list of length n_datasets with unmasked datasets (i.e., full validation of all recordings).
# If `save_datasets = TRUE`, then these will be saved individually in `directory` as dataset_n.rds, where n 
# is the dataset number.
#
# zeros: A list of length n_datasets containing all of the site-visits where no recordings of a certain 
# classification were observed. For example, if, in dataset 10, there were no calls from species 1 that were 
# classified as  3 on visit 4 to site 156, then the 10th entry of this list would contain a dataset with 
# a row corresponding tosite = 156, visit = 4, true_spp = 1, id_spp = 3, with count = 0. These zeros are 
# necessary for housekeeping in the model-fitting process. If `save_datasets = TRUE`, the zeros for each 
# each dataset will be saved in `directory` individually as site_visits_without_calls_in_dataset_n.rds, where
# n is the dataset number.
#
# masked_dfs: A nested list containing each dataset masked under each scenario. masked_dfs[[9]][[27]] contains
# dataset 27, assuming validation scenario 9. If `save_masked_datasets = TRUE`, then each dataset/scenario 
# scenario combination is saved individually in `directory` as dataset_n_masked_under_scenario_s.rds, 
# where n is the dataset number and s is the scenario number. 

source("Data Simulation/count_detection_sim.R")
source("Data Simulation/mask_by_spp.R")
source("Data Simulation/mask_FE.R")

simulate_validatedData <- function(n_datasets,
                                   validation_design = c("BySpecies", "FixedPercent"),
                                   scenarios, 
                                   nsites, 
                                   nspecies, 
                                   nvisits, 
                                   psi, 
                                   lambda, 
                                   theta, 
                                   save_datasets = TRUE, 
                                   save_masked_datasets = TRUE,
                                   directory = here::here()){
    
  # Initialize storage lists
  datasets_list <- list()
  site_visits_without_calls <- list()
    
    for(m in 1:n_datasets){
      
      # sim_dat simulates from the (aggregated) count-detection model as specified in 
      # Stratton et al., (2022). To obtain the disaggregated dataset, we use 
      # tidyr::uncount below
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
  
  if(validation_design == "BySpecies"){
    
    # loop over scenarios and datasets, saving each masked dataset
    # Note that for this design, `scenarios` is a dataframe object
    for(s in 1:nrow(scenarios)){
      masked_dataset_list[[s]] <- list()
      for(d in 1:length(datasets_list)){
        
        masked_df <- mask_spp2(datasets_list[[d]], scenarios[s,])$final_df
        masked_df$scenario <- s
        
        if(save_masked_datasets == TRUE){
          saveRDS(masked_df, 
                  file = paste0(directory, "/dataset_", d, "_masked_under_BSV_scenario_", s, ".rds"))
        }
        
        masked_dataset_list[[s]][[d]] <- masked_df
        
      }
    }
    
  } else {
    
    # Loop over the specified vector of percentages
    # Note: here `scenarios` is a vector! 
    for(s in 1:length(scenarios)){
      masked_dataset_list[[s]] <- list() # initiate interior storage for each scenario (i.e., for each distinct percentage)
      for(d in 1:length(datasets_list)) { # loop over the datasets list, masking each according to the scenario 
        
        masked_df <- mask_FE(
          df = datasets_list[[d]], 
          effort_prop = scenarios[s]
        ) %>% mutate(scenario = s)
        
        if(save_masked_datasets == TRUE) {
          saveRDS(
            masked_df, 
            file = paste0(directory, "/dataset_", d, "_masked_under_FE_scenario_", s, ".rds")
          )
        }
        
        masked_dataset_list[[s]][[d]] <- masked_df
        
      }
    }
    
  }
    
  return(list(full_datasets = datasets_list, 
              zeros = site_visits_without_calls, 
              masked_dfs = masked_dataset_list))
  
}
