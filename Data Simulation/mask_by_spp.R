# Masking true species labels based on the assigned autoIDs
# Function allows user to specify the proportions to sample from each ID'd spp
#
## ================== Inputs ================= ## 
#
# data: A dataframe containing the columns: 
#   - site
#   - visit
#   - true_spp
#   - id_spp
#   - count
#
# props_to_val: a vector containing the proportion of recordings to validate for each species
# 
## ================ Outputs ================== ## 
#
# final_df: A dataframe with the same number of rows as the input data. The true species labels 
# are shown for each autoID category according to props_to_val; all other true species labels 
# masked as NA. 
#
# data_sum: A small tibble containing summaries for each autoID containing 
#   - n: the number of recordings with that autoID label
#   - n_NA: the proportion of that autoID with unknown true species labels
#   - prop_NA: the proportion of recordings with that autoID label that are masked. This should be 
#     (1 - props_to_val[autoID]). 

mask_spp2 <- function(data, props_to_val){
  
  n <- nrow(data)
  data$call <- 1:n # For housekeeping to make sure that the anti_join call works properly
  
    # Otherwise, specify that the proportion to mask is 1 - proportion to validate
    # and do the masking using slice_sample  
    
    eps <- sqrt(.Machine$double.eps)
    props_to_mask <- 1 - props_to_val + eps
    
    
    masked_df <- data %>% 
      filter(count != 0) %>% # Don't want to mask rows that exist just for housekeeping
      group_by(id_spp) %>% 
      group_split() %>% # split into separate dfs for each group
      map2_dfr(props_to_mask, ~ slice_sample(.x, prop = .y)) %>% # apply slice_sample with 
      mutate(true_spp = NA)                                      # designated proportion to each 
                                                                 # separate df, then mask 
  
  # Once masking is completed, bind the rows that weren't masked (anti_join)
  # with those that *were* (inner_join) and arrange to make it look good 
  
  final_df <- bind_rows(
    anti_join( # returns rows of data that do not have a match in masked_df, 
      data,    # including rows where count == 0
      masked_df, 
      by = "call" # this is necessary to ensure the number of rows after masking is the same as OG df
    ),  
    inner_join( # returns rows of data[,-true_spp] that have a match in masked_df
      data %>% select(-true_spp), 
      masked_df
    ) 
  ) %>% arrange(call)
  
  # Compute a summary of the masked dataset (good check to make sure masking is 
  # working as expected) 
  data_summary <- final_df %>% 
    group_by(id_spp) %>% 
    summarize(
      n = n(), 
      n_NA = sum(is.na(true_spp)), 
      prop_NA = n_NA/n
    )
  
  out.list <- list(final_df = final_df, data_sum = data_summary)  
  return(out.list)
  
}


