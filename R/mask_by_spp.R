#' mask_by_spp: simulate a validation design
#'
#' @param data A dataframe containing the columns `site`, `visit`, `true_spp`, `id_spp`, `count`
#' @param props_to_val a vector containing the proportion of recordings to validate for each species
#'
#' @return A list containing two elements: `final_df` and `data_sum`. `final_df` is a copy of the input `data` masked according to the validation design supplied by `props_to_val`. The second output, `data_sum` is a dataframe containing a summary of the number and proportion of ambiguous (i.e., not validated) recordings. It provides a check that the masking function is working correctly.
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
      dplyr::filter(count != 0) %>% # Don't want to mask rows that exist just for housekeeping
      dplyr::group_by(id_spp) %>%
      dplyr::group_split() %>% # split into separate dfs for each group
      purrr::map2_dfr(props_to_mask, ~ dplyr::slice_sample(.x, prop = .y)) %>% # apply slice_sample with
      dplyr::mutate(true_spp = NA)                                      # designated proportion to each
                                                                 # separate df, then mask

  # Once masking is completed, bind the rows that weren't masked (anti_join)
  # with those that *were* (inner_join) and arrange to make it look good

  final_df <- dplyr::bind_rows(
    dplyr::anti_join( # returns rows of data that do not have a match in masked_df,
      data,    # including rows where count == 0
      masked_df,
      by = "call" # this is necessary to ensure the number of rows after masking is the same as OG df
    ),
    dplyr::inner_join( # returns rows of data[,-true_spp] that have a match in masked_df
      data %>% dplyr::select(-true_spp),
      masked_df
    )
  ) %>%
    dplyr::arrange(call)

  # Compute a summary of the masked dataset (good check to make sure masking is
  # working as expected)
  data_summary <- final_df %>%
    dplyr::group_by(id_spp) %>%
    dplyr::summarize(
      n = dplyr::n(),
      n_NA = sum(is.na(true_spp)),
      prop_NA = n_NA/n
    )

  out.list <- list(final_df = final_df, data_sum = data_summary)
  return(out.list)

}


