library(nimble)
library(tidyverse)

source("Data Simulation/count_detection_sim.R")
source("Data Simulation/mask_by_spp.R")


n_datasets <- 3
testing_datasets <- list()
testing_noCounts <- list()
testing_dgvs <- list()

# Simulate n_datasets datasets from the model with all true spp labels visible

for(i in 1:n_datasets){
  
  data <- sim_dat(nsites = 100,
                  nspecies = 2,
                  nvisits = 4,
                  lambda = c(2, 4),
                  psi = c(.37, .65),
                  seed = i,
                  )
  testing_dgvs[[i]] <- data$params
  df6 <- data$full_df %>% group_by(site, visit) %>% mutate(L. = sum(count))
  testing_datasets[[i]] <- df6 %>% uncount(., weights = count, .remove = FALSE)
  testing_noCounts[[i]] <- df6 %>% filter(count == 0)
}

# Save full datasets, the data-generating values, and the sites where there were 
# no counts, which are needed for housekeeping. Note the "Testing" folder must 
# exist in the directory to avoid errors. 

saveRDS(testing_datasets, "Testing/test_data_list.rds")
saveRDS(testing_noCounts, "Testing/test_zeros_list.rds")
saveRDS(testing_dgvs[[1]], "Testing/test_DGVs.rds")

# Get the level of effort for each species. This is less critical in this small 
# example, but this step is used in the five-species assemblage to assign the 
# same level of effort to all species that have a high/medium/low expected number 
# of calls. 

get_props_to_val <- function(x){
  
  props_to_val <- c(x[2], x[1]) # assumes two species and two expected-call categories (high/low)
  return(props_to_val)
  
}

# Assign levels of validation effort to expected call categories. 
# In the first scenario, less-prevalent/active species get .75 of autoIDs validated, 
# while more-prevalent and active ones (which create greater # of calls) get .25. 
# In the second scenario, both species types get half of their autoIDs validated.
# `validation_levels` is a matrix. Row corresponds with scenario, column corresponds 
# with expected call category, where the first column is "High expected number of calls" 
# and second column is "low expected number of calls". 

validation_levels <- matrix(c(.25, .75,
                              .5, .5), 
                            byrow = TRUE, 
                            nrow = 2)

# Again, less critical in the 2-spp case, but the following assigns the 
# validation level (based on the expected number of calls) to each species. 
# Resulting output is a matrix (props_matrix) that describes the level of effort for each spp; 
# The row is the validation scenario and the column is the species. 
# So, spp 1 gets 75% effort, spp 2 gets 25% effort. 

props_matrix <- apply(validation_levels, 1, get_props_to_val) %>% t()

# Function to obtain the masked dataset. 
# Relies on the mask_spp2 function contained in mask_by_spp.R
# To the list of dataframes simulated above, apply the mask_by_spp function. 

get_mDF <- function(props, list_of_dfs){
  
  out <- lapply(list_of_dfs, mask_spp2, props_to_val = props)
  out_full <- lapply(out, function(x) x[[1]])
  return(out_full)
  
}

# Level of Validation Effort = LOVE. 
# Apply the masking function to all datasets in the list, according to the 
# prescribed level of effort (i.e., vetting scenario). 
# The resulting output is a list of length n_vetting_scenarios; each element in 
# this list is itself a list of the n_datasets simulated datasets that have true spp
# labels retained according to the vetting scenario. 

byLOVE <- apply(props_matrix, 1, get_mDF, list_of_dfs = testing_datasets)

# Save simulated data
saveRDS(byLOVE, file = "Testing/masked_test_dfs.rds")

