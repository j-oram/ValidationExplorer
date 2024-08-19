## This function evaluates a nested list of datasets and outputs the average number of recordings 
## validated for each scenario. 
# 
# Input: 
# 
# - data_list: a nested list in the format put out by simulate_validatedData$masked_dfs
# 
# Output: 
# 
# - a list containing the average number of recordings validated in a dataset under each scenario. 

library(dplyr)

summarize_n_validated <- function(data_list) {
  
  lapply(data_list, function(x) { # x is the scenario
    lapply(x, function(y) { # y is an individual dataset
      sum(!is.na(y$true_spp)) # find the number of validated calls in y
    }) %>% # output is a list. 
      unlist() %>% # Take this, make it a vector and 
      mean() # take the average -- this is the expected number of validated recordings under scenario x
  }) %>% 
    unlist() %>% 
    return() # return the list of averages (one for each scenario)
  
}
