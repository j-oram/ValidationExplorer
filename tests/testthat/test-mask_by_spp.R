# Testing for mask_by_spp.R
library(dplyr)

test_df <- sim_dat()$full_df
props <- runif(dplyr::n_distinct(test_df$true_spp), min = 0.2, max = 1)

test_that("mask_by_spp returns df with the same number of rows as input", {
  expect_equal(nrow(mask_by_spp(test_df, props_to_val = props)$final_df), nrow(test_df))
})

test_that("mask_by_spp proportions for each spp are approximately equal", {
  expect_true(all(mask_by_spp(test_df, props_to_val = props)$final_df %>% 
                 filter(count !=0) %>% 
                 ungroup() %>% 
                 group_by(id_spp) %>% 
                 summarize(props_vald = sum(!is.na(true_spp))/n()) %>%
                 ungroup() %>% 
                 select(props_vald) %>% 
                 unlist() %>% 
                 round(digits = 2) -  
               (props %>% round(digits = 2)) < 0.15)
  )
})
