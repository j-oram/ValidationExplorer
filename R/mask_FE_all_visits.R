#' Mask a proportion of all visits: A function for simulating a fixed effort validation design.
#'
#' @param df A dataframe object in the format  of `full_dfs` output from
#'   \link{simulate_validatedData}
#' @param effort_prop The proportion of recordings to be randomly sampled from
#'   the first visit for validation
#' @param seed An optional random seed to make masking reproducible
#'
#' @return A dataframe object that is a copy of the input `df`, but with the
#'   appropriate level of effort according to a fixed effort validation design.
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' cd_data <- sim_dat()$full_df %>% tidyr::uncount(weights = count, .remove = FALSE)
#' FE_data <- mask_FE_all_visits(cd_data, effort_prop = 0.1)
#'
#' head(cd_data)
#' head(FE_data)
#'
#' FE_data %>% 
#'   group_by(site, visit) %>% 
#'   summarize(
#'     prop_validated = sum(!is.na(true_spp))/n()
#'   )
#'   
#'

mask_FE_all_visits <- function (df, effort_prop, seed = NULL) {
  
  # set the seed if specified by the user
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  # for housekeeping -- make sure that we aren't double counting any obs
  # (or missing any)
  df$call <- 1:nrow(df)
  
  # mask a fixed percent from each visit to each site
  masked <- df %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(.data$site, .data$visit) %>% 
    dplyr::slice_sample(prop = 1 - effort_prop) %>% 
    dplyr::mutate(true_spp = NA)
  
  # grab the calls that were validated
  unmasked <- df %>% dplyr::filter(call %notin% masked$call)
  
  # return a copy of the original dataframe but with the mask applied
  out_df <- dplyr::bind_rows(masked, unmasked) %>% 
    dplyr::arrange(call) %>% 
    dplyr::select(-call)
  
  return(out_df)
    
}
