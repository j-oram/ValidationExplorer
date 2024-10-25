#' mask_by_spp
#'
#' @param data A dataframe containing the columns `site`, `visit`, `true_spp`, `id_spp`, `count`
#' @param props_to_val a vector containing the proportion of recordings to validate for each species
#'
#' @return A dataframe object that is a copy of the input `data` masked according to the validation design supplied by `props_to_val`.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' dat <- sim_dat()$full_df
#'
#' head(dat)
#'
#' dat <- dat %>% tidyr::uncount(weights = count, .remove = FALSE)
#' val_dat <- mask_by_spp(dat, props_to_val = c(rep(.1, 4), rep(.4, 4)))
#'
#' val_dat$final_df %>% group_by(id_spp) %>% summarize(prop_vald = sum(!is.na(true_spp))/n())
mask_by_spp <- function(data, props_to_val){

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

  return(final_df)

}


