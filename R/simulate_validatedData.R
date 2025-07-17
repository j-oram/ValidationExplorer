#' Simulate many datasets under candidate validation designs
#'
#' @param n_datasets The number of datasets you would like to have simulated. Each of these
#'   simulated datasets will be subjected to all candidate validation designs.
#' @param design_type Character string, either "BySpecies" for a stratified-by-species design,
#'   or "FixedPercentage" for a fixed effort design (see Oram et al., in review for more details on each of these)
#' @param scenarios if `design_type = "BySpecies"`, the `scenarios` argument must be a list with each entry corresponding
#'   to the potential levels of effort for a particular autoID label. If
#'   `design_type == "FixedPercent"`, then the `scenarios` argument must be a vector with each entry corresponding to
#'   a potential percent of calls to be sampled from the first visit at each site. See vignette for an example.
#' @param nsites number of sites in each dataset
#' @param nspecies size of the species assemblage
#' @param nvisits the number of visits to each site. Note that these simulations assume a balanced design.
#' @param psi a vector of length nspecies with the assumed occurrence probabilities for each species
#' @param lambda a vector of length nspecies with the assumed relative activity levels for each species.
#'   Make sure the order is correct and matches psi.
#' @param theta a matrix containing the (mis)classification probabilities. The rows of this matrix must sum
#'   to 1. See vignette for an example.
#' @param scen_expand If `design_type = "BySpecies"`, should `simulate_validatedData` expand the list of `scenarios`?
#'   If TRUE (the default value), then `scenarios` must be a list; if FALSE, then `simulate_validatedData` expects a
#'   user-supplied dataframe object through the `scen_df` argument. 
#' @param scen_df If `scen_expand = FALSE`, a user-supplied dataframe object with each row corresponding to 
#'   the validation scenario and each column to the species. Default value is NULL. 
#' @param save_datasets logical. If TRUE, the datasets without any masking of true species labels (i.e.,
#'   corresponding to complete validation of all recordings) will be saved. Default value is FALSE.
#' @param save_masked_datasets logical. If TRUE, the masked datasets (i.e., the simulated datasets with
#'   partial validation according to the simulation scenario) will be saved. This means that there will be
#'   n_datasets x nrow(scenarios_dataframe) datasets saved: one for each dataset under each validation scenario.
#'   Default value is FALSE.
#' @param directory character. Required if save_datasets = TRUE or save_masked_datasets = TRUE. This is where the
#'   datasets will be saved. By default, the current working directory (i.e., here::here()) will be used.
#'
#' @return A list containing three elements:
#'   1) `full_datasets`: A list of length n_datasets with unmasked datasets (i.e., full validation of all recordings).
#'   If `save_datasets = TRUE`, then these will be saved individually in `directory` as dataset_n.rds, where n
#'   is the dataset number.
#'   2) `zeros`: A list of length n_datasets containing all of the site-visits where no recordings of a certain
#'   classification were observed. For example, if, in dataset 10, there were no calls from species 1 that were
#'   classified as  3 on visit 4 to site 156, then the 10th entry of this list would contain a dataset with
#'   a row corresponding tosite = 156, visit = 4, true_spp = 1, id_spp = 3, with count = 0. These zeros are
#'   necessary for housekeeping in the model-fitting process. If `save_datasets = TRUE`, the zeros for each
#'   each dataset will be saved in `directory` individually as zeros_in_dataset_n.rds, where
#'   n is the dataset number.
#'   3) `masked_dfs`: A nested list containing each dataset masked under each scenario. masked_dfs\\[\\[9\\]\\]\\[\\[27\\]\\] contains
#'   dataset 27, assuming validation scenario 9. If `save_masked_datasets = TRUE`, then each dataset/scenario
#'   scenario combination is saved individually in `directory` as dataset_n_masked_under_scenario_s.rds,
#'   where n is the dataset number and s is the scenario number.
#' @export
#'
#' @examples
#'
#' psi <- c(0.3, 0.6)
#' lambda <- c(11, 2)
#'
#' nspecies <- length(psi)
#' nsites <- 30
#' nvisits <- 5
#'
#' test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
#' val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)
#'
#' fake_data <- simulate_validatedData(
#'   n_datasets = 5,
#'   design_type = "BySpecies",
#'   scenarios = val_scenarios,
#'   nsites = nsites,
#'   nvisits = nvisits,
#'   nspecies = nspecies,
#'   psi = psi,
#'   lambda = lambda,
#'   theta = test_theta1,
#'   save_datasets = FALSE,
#'   save_masked_datasets = FALSE,
#'   directory = paste0(here::here("Testing"))
#' )
#'
#'
simulate_validatedData <- function(n_datasets,
                                   design_type = c("BySpecies", "FixedPercent"),
                                   scenarios = NULL,
                                   nsites = 100,
                                   nspecies = 8,
                                   nvisits = 3,
                                   confirmable_limits = NULL,
                                   psi = runif(nspecies, 0.1, 0.9),
                                   lambda = abs(rnorm(nspecies, 0, 5)),
                                   theta = t(apply(diag(18, nrow = nspecies)+2, 1, function(x) {nimble::rdirch(alpha = x)})),
                                   scen_expand = TRUE,
                                   scen_df = NULL,
                                   save_datasets = FALSE,
                                   save_masked_datasets = FALSE,
                                   directory = here::here()){

  # check the users's specified classifier
  if(any(round(rowSums(theta), 5) != 1)) {
    stop("The rows of theta do not sum to 1.")
  }
  
  # check the length of scenarios under BySpecies validation design
  if(design_type == "BySpecies" & !is.null(scenarios) & length(scenarios) != nspecies) {
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
      aggregate_CD <- sim_dat(nsites = nsites,
                      nspecies = nspecies,
                      nvisits = nvisits,
                      lambda = lambda,
                      psi = psi,
                      seed = m,
                      theta = theta)$full_df

      # Y. = total calls at each site-visit
      agg_CD_with_total <- aggregate_CD %>%
        dplyr::group_by(.data$site, .data$visit) %>%
        dplyr::mutate(Y. = sum(.data$count))

      # Disaggregated count detection data gets stored in mth entry of the
      # datasets_list
      datasets_list[[m]] <- agg_CD_with_total %>%
        tidyr::uncount(weights = .data$count, .remove = FALSE)

      # Store the zeros in a list for housekeeping
      zeros[[m]] <- agg_CD_with_total %>% dplyr::filter(.data$count == 0)

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
    
    if(scen_expand) {
      # if user specified BySpecies, create a dataframe with a unique combination
      # of validation efforts for each species
      scenarios <- expand.grid(scenarios)
      
      # loop over scenarios and datasets, saving each masked dataset
      # Note that for this design, `scenarios` is a dataframe object
      for(s in 1:nrow(scenarios)){
        masked_dataset_list[[s]] <- list()
        for(d in 1:length(datasets_list)){
          
          masked_df <- suppressMessages(mask_by_spp(datasets_list[[d]], scenarios[s,])$final_df)
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
      
    } else { # User specifies they don't want to use expand.grid
      
      if(is.null(scen_df)) {
        stop("`scen_df` must not be NULL if `scen_expand = FALSE`.\nPlease supply a scenario x species dataframe.")
      } else {
        
        # create the summary df to output
        scenarios <- scen_df
        
        # Do the masking in the same way as if expand.grid was called on the spp list
        for(s in 1:nrow(scenarios)){
          masked_dataset_list[[s]] <- list()
          for(d in 1:length(datasets_list)){
            
            masked_df <- suppressMessages(
              mask_by_spp(datasets_list[[d]], scenarios[s,])$final_df
            ) 
            masked_df$scenario <- s
            
            if(save_masked_datasets == TRUE){
              
              saveRDS(masked_df,
                      file = file.path(directory, "masked_datasets", paste0("dataset_", d, "_masked_under_BSV_scenario_", s, ".rds")))
            }
            
            masked_dataset_list[[s]][[d]] <- masked_df
            
          }
        }
        
        # create scenario column for joining later
        scenarios <- scenarios %>%
          mutate(scenario = 1:nrow(scenarios)) %>% 
          relocate(scenario)
        
      }
      
    }


  } else if (design_type == "FixedPercent") { # user specified a fixed effort design

    # Loop over the specified vector of percentages
    # Note: here `scenarios` is a vector!
    for(s in 1:length(scenarios)){
      masked_dataset_list[[s]] <- list() # initiate interior storage for each scenario (i.e., for each distinct percentage)
      for(d in 1:length(datasets_list)) { # loop over the datasets list, masking each according to the scenario

        masked_df <- suppressMessages(mask_FE_all_visits(
          df = datasets_list[[d]],
          effort_prop = scenarios[s]
        )) %>% dplyr::mutate(scenario = s)

        if (!is.null(confirmable_limits)) {
          masked_df$selected <- ifelse(!is.na(masked_df$true_spp), "Y", "N")
          masked_df <- masked_df %>% 
            group_by(site, visit) %>% 
            mutate(
              prop_confirmable = runif(
                1, 
                min = confirmable_limits[1], 
                max = confirmable_limits[2]
              )
            ) %>% 
            ungroup()
          
          # split df into the selected and not selected components
          selected <- masked_df %>% filter(selected == "Y")
          not_selected <- setdiff(masked_df, selected)
          
          # If selected: Pull off the calls that were selected and can be confirmed
          confirmable <- selected %>% 
            group_by(site, visit) %>% 
            group_split() %>% 
            lapply(., function(x) {
              p <- unique(x$prop_confirmable)
              return(slice_sample(x, prop = p))
            }) %>% 
            do.call('bind_rows', .)
          
          # If not confirmable, then the true spp label remains ambiguous (has value NA)
          not_confirmable <- setdiff(selected, confirmable)
          not_confirmable$true_spp <- NA
          
          # Bind together a copy of selected subset that has confirmed and non-
          # confirmable recordings in it
          selected_out <- bind_rows(confirmable, not_confirmable) %>% 
            arrange(unique_call_id)
          
          # Bind copy of selected subset with the not selected recordings
          masked_df_new <- bind_rows(selected_out, not_selected)
        }
        
        if (save_masked_datasets == TRUE) {
    
          saveRDS(
            masked_df,
            file = file.path(directory, "masked_datasets", paste0("dataset_", d, "_masked_under_FE_scenario_", s, ".rds"))
          )
        }

        masked_dataset_list[[s]][[d]] <- masked_df

      }
    }

  } else {
    stop("design_type must be one of c('BySpecies', 'FixedPercent')")
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
