simulate_validatedData <- function(n_datasets,
                                   validation_design = c("BySpecies", "FixedPercent"),
                                   scenarios,
                                   nsites,
                                   nspecies,
                                   nvisits,
                                   psi,
                                   lambda,
                                   theta,
                                   save_datasets = FALSE,
                                   save_masked_datasets = FALSE,
                                   directory = here::here()){


  if(any(round(rowSums(theta), 5) != 1)) {
    stop("The rows of theta do not sum to 1.")
  }

  if(validation_design == "BySpecies" & length(scenarios) != nspecies) {
    stop("Scenarios must be a list with length = nspecies.")
  }

  # Initialize storage lists
  datasets_list <- list()
  zeros <- list()

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
      zeros[[m]] <- agg_CD_with_total %>% filter(count == 0)

      # If user wants individual rds files for each dataframe, save them and the zeros
      # in the specified directory
      if(save_datasets == TRUE){
        saveRDS(datasets_list[[m]], file = file.path(directory, paste0("dataset_", m, ".rds")))
        saveRDS(zeros[[m]],
                file = file.path(directory, paste0("zeros_in_dataset_", m, ".rds")))
      }

    }

  ######## Mask datasets generated above #########

  # set up storage for the masked datasets
  masked_dataset_list <- list()

  if(validation_design == "BySpecies"){

    scenarios <- expand.grid(scenarios)

    # loop over scenarios and datasets, saving each masked dataset
    # Note that for this design, `scenarios` is a dataframe object
    for(s in 1:nrow(scenarios)){
      masked_dataset_list[[s]] <- list()
      for(d in 1:length(datasets_list)){

        masked_df <- mask_spp2(datasets_list[[d]], scenarios[s,])$final_df
        masked_df$scenario <- s

        if(save_masked_datasets == TRUE){
          saveRDS(masked_df,
                  file = file.path(directory, paste0("dataset_", d, "_masked_under_BSV_scenario_", s, ".rds")))
        }

        masked_dataset_list[[s]][[d]] <- masked_df

      }
    }

    # add a column for the scenario number to make it easy to see which
    # LOVE for each species goes with which number on x-axis of plots
    scenarios <- scenarios %>%
      rownames_to_column(var = "Scenario_Number")

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
            file = file.path(directory, paste0("dataset_", d, "_masked_under_FE_scenario_", s, ".rds"))
          )
        }

        masked_dataset_list[[s]][[d]] <- masked_df

      }
    }

  }

  # If the validation design is by-species, also return the scenarios dataframe
  # created by the expand.grid call above. This is potentially useful for users
  # to see exactly which scenarios are being investigated
  if(validation_design == "BySpecies") {
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
