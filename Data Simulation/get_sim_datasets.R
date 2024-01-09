library(nimble)
library(tidyverse)

source("count_detection_sim.R")
source("mask_by_spp.R")
source("simulation_scenarios.R")



n_datasets <- 50

## Simulate 50 datasets from assemblage with theta1 classifier
theta1_datasets <- list()
theta1_noCounts <- list() # Need to keep track of the rows where count == 0 to fit the model later. 

for(i in 1:n_datasets){
  
  # sim_dat simulates from the (aggregated) count-detection model as specified in 
  # Stratton et al., (2022). To obtain the disaggregated dataset, we use 
  # tidyr::uncount 
  
  data <- sim_dat(nsites = 100,
                  nspecies = 5,
                  nvisits = 4,
                  lambda =psi_lambda$lambda,
                  psi = psi_lambda$psi,
                  seed = i,
                  theta = theta1)$full_df
  
  # L. = total calls at each site-visit (notation from Spiers et el., 2022).
  df6 <- data %>% group_by(site, visit) %>% mutate(Y. = sum(count)) 
  
  # Disaggregated count detection data gets stored in ith entry of the 
  # theta1_datasets list
  theta1_datasets[[i]] <- df6 %>% uncount(., weights = count, .remove = FALSE)
  
  # store the site-visits where no calls were observed so that total number of
  # site-visits is consistent in model fitting
  theta1_noCounts[[i]] <- df6 %>% filter(count == 0)
}


## Simulate 50 datasets under theta2 scenario -- repeat the process above
theta2_datasets <- list()
theta2_noCounts <- list()

for(i in 1:n_datasets){
  
  data <- sim_dat(nsites = 100,
                  nspecies = 5,
                  nvisits = 4,
                  lambda =psi_lambda$lambda,
                  psi = psi_lambda$psi,
                  seed = i,
                  theta = theta2)$full_df
  
  df6 <- data %>% group_by(site, visit) %>% mutate(Y. = sum(count))
  theta2_datasets[[i]] <- df6 %>% uncount(., weights = count, .remove = FALSE)
  theta2_noCounts[[i]] <- df6 %>% filter(count == 0)
  
}

## Simulate 50 datasets under theta3 scenario
theta3_datasets <- list()
theta3_noCounts <- list()

for(i in 1:n_datasets){
  
  data <- sim_dat(nsites = 100,
                  nspecies = 5,
                  nvisits = 4,
                  lambda =psi_lambda$lambda,
                  psi = psi_lambda$psi,
                  seed = i,
                  theta = theta3)$full_df
  
  df6 <- data %>% group_by(site, visit) %>% mutate(Y. = sum(count))
  theta3_datasets[[i]] <- df6 %>% uncount(., weights = count, .remove = FALSE)
  theta3_noCounts[[i]] <- df6 %>% filter(count == 0)
}

# saveRDS(theta1_datasets, file = "Simulation data/theta1_datasets.rds")
# saveRDS(theta1_noCounts, file = "Simulation data/theta1_noCounts.rds")
# 
# saveRDS(theta2_datasets, file = "Simulation data/theta2_datasets.rds")
# saveRDS(theta2_noCounts, file = "Simulation data/theta2_noCounts.rds")
# 
# saveRDS(theta3_datasets, file = "Simulation data/theta3_datasets.rds")
# saveRDS(theta3_noCounts, file = "Simulation data/theta3_noCounts.rds")


## Next, mask each dataset under first 9 scenarios: 

# Need a function that takes in proportions to validate based on whether 
# the expected number of calls for a spp is considered to be low/medium/high
get_props_to_val <- function(x){
  
  # Spp 1 is "high", 2,3,5 are "medium" and spp4 is "rare"
  props_to_val <- c(x[3], x[2], x[2], x[1], x[2])  
  return(props_to_val)
  
}

# Get distinct validation effort scenarios. LOVE = level of validation effort
LOVEs <- val_efforts %>% select(-theta_cat) %>% distinct()

# Get the matrix with proportions to validate for each species under each 
# vetting scenario
props_matrix <- apply(LOVEs, 1, get_props_to_val) %>% t()


# function that, given a vector of proportions and a list of dataframes, 
# will apply masking to 1 - proportion of true spp labels. 
# See the details of mask_by_spp script. 
get_mDF <- function(props, list_of_dfs){
  
  out <- lapply(list_of_dfs, mask_spp2, props_to_val = props)
  out_full <- lapply(out, function(x) x[[1]])
  return(out_full)
  
}


# For each level of validation effort (LOVE) contained in props_matrix, apply the 
# masking function to all datasets. The result is a nested list: nine elements
# (one for each vetting scenario), each of which is a list of length 50
# containing the masked datasets under that vetting scenario.

byLOVE <- apply(props_matrix, 1, get_mDF, list_of_dfs = theta1_datasets)
#saveRDS(byLOVE, file = "Simulation data/theta1_masked_datasets.rds")

byLOVE2 <- apply(props_matrix, 1, get_mDF, list_of_dfs = theta2_datasets)
#saveRDS(byLOVE2, file = "Simulation data/theta2_masked_datasets.rds")

byLOVE3 <- apply(props_matrix, 1, get_mDF, list_of_dfs = theta3_datasets)
#saveRDS(byLOVE3, file = "Simulation data/theta3_masked_datasets.rds")


# To get the masking/validation summary, once masked datasets have been obtained

get_mDF_summary <- function(list_of_dfs){
  out <- lapply(list_of_dfs, function(x) {
    x %>% 
      group_by(id_spp) %>% 
      summarize(n = n(), n_NA = sum(is.na(true_spp)), prop_NA = n_NA/n, 
                n_val = n - n_NA)
    }
  )
  return(out)
  
}

theta1_masked_dfs_sum <- lapply(byLOVE, get_mDF_summary)
theta2_masked_dfs_sum <- lapply(byLOVE2, get_mDF_summary)
theta3_masked_dfs_sum <- lapply(byLOVE3, get_mDF_summary)

# saveRDS(theta1_masked_dfs_sum, file = "theta1_masked_dfs_sums.rds")
# saveRDS(theta2_masked_dfs_sum, file = "theta2_masked_dfs_sums.rds")
# saveRDS(theta3_masked_dfs_sum, file = "theta3_masked_dfs_sums.rds")


# ------------ Additional scenarios after running first round of simulations ----------# 

# load the datasets (created above)
t1_datasets <- readRDS("./Simulation data/theta1_datasets.rds")
t2_datasets <- readRDS("./Simulation data/theta2_datasets.rds")
t3_datasets <- readRDS("./Simulation data/theta3_datasets.rds")

## Add scenarios: 

## Build analog to LOVEs object above with additional sim scenarios
ad_sims <- data.frame(rare = rep(1, 3), medium = c(.25, 1, .25), high = c(.1, .1, .5))
# ad_sims

## Modify the masking function for these additional scenarios: species 3 is now
## classified as a 'rare' species in the sense of 'across the landscape'.  

get_props_to_val2 <- function(x){
  
  # spp 1 = high, spp3,4 = rare, spp 2,5 = medium
  props_to_val <- c(x[3], x[2], x[1], x[1], x[2])
  return(props_to_val)
  
}

## Obtain matrix of proportions, analogous to props_matrix above
val_matrix <- apply(ad_sims, 1, get_props_to_val2) %>% t()
# val_matrix




# ------ Theta 1 ------- #

# Obtain masked datasets
theta1_adl_masked_datasets <- apply(val_matrix, 1, get_mDF, list_of_dfs = t1_datasets)
#saveRDS(theta1_adl_masked_datasets, file = "Simulation data/theta1_adl_masked_datasets.rds")

# Grab summaries of the datasets 
theta1_adl_mDFs_sums <- lapply(theta1_adl_masked_datasets, get_mDF_summary)
#saveRDS(theta1_adl_mDFs_sums, file = "Simulation data/theta1_adl_mDFs_sums.rds") 





# ------ Theta 2 ------- #

# obtain masked dfs
theta2_adl_masked_datasets <- apply(val_matrix, 1, get_mDF, list_of_dfs = t2_datasets)
# saveRDS(theta2_adl_masked_datasets, file = "Simulation data/theta2_adl_masked_datasets.rds")

theta2_adl_mDFs_sums <- lapply(theta2_adl_masked_datasets, get_mDF_summary)
# saveRDS(theta2_adl_mDFs_sums, file = "Simulation data/theta2_adl_mDFs_sums.rds") 





# ------ Theta 3 ------- #

# obtain masked dfs 
theta3_adl_masked_datasets <- apply(val_matrix, 1, get_mDF, list_of_dfs = t3_datasets)
# saveRDS(theta3_adl_masked_datasets, file = "Simulation data/theta3_adl_masked_datasets.rds")

theta3_adl_mDFs_sums <- lapply(theta3_adl_masked_datasets, get_mDF_summary)
# saveRDS(theta3_adl_mDFs_sums, file = "Simulation data/theta3_adl_mDFs_sums.rds") 
