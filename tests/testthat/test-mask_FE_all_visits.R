# Testing for fixed effort (all visits) masking function
library(dplyr)
test_df <- sim_dat()$full_df
p <- 0.2

test_that("mask_FE_all_visits returns df with the same number of rows as input", {
  expect_equal(nrow(mask_FE_all_visits(test_df, effort_prop = p, seed = 12)), nrow(test_df))
})

test_that("mask_by_spp proportions for each spp are approximately equal", {
  expect_true(all(mask_FE_all_visits(test_df, effort_prop=p) %>% 
                    filter(count !=0) %>% 
                    ungroup() %>% 
                    group_by(visit) %>% 
                    summarize(props_vald = sum(!is.na(true_spp))/n()) %>%
                    ungroup() %>% 
                    select(props_vald) %>% 
                    unlist()- p <= 0.15)
  )
})