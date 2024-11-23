#' mask_FE_h: A function for simulating a fixed effort validation design.
#' 
#' This function implements a fixed effort design to be used when heterogenous 
#'   relative activity is expected. Rather than validating a random x% from the 
#'   first visit, which assumes that the first visit is representative of all 
#'   others in terms of the number of recordings that are expected, this design 
#'   randomly selects y% from each visit. This is the design used in the Oregon-
#'   Washington dataset that was previously analyzed by Oram et al. (in 
#'   submission). 
#'
#' @param df A dataframe object in the format  of `full_dfs` output from
#'   \link{simulate_validatedData}
#' @param effort_prop The proportion of recordings to be randomly sampled from
#'   the each site visit for validation. 
#' @param seed An optional random seed to make masking reproducible
#'
#' @return A dataframe object that is a copy of the input `df`, but with the
#'   appropriate level of effort according to a fixed effort validation design.
#' @export
#'
#' @examples
#' cd_data <- sim_dat()$full_df %>% tidyr::uncount(weights = count, .remove = FALSE)
#'
#' FE_data <- mask_FE(cd_data, effort_prop = 0.2)
#'
#' head(cd_data)
#' head(FE_data)
#'
mask_FE <- function(df, effort_prop, seed = NULL) {
  
  # set the seed if specified by the user
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  # for housekeeping -- make sure that we aren't double counting any obs
  # (or missing any)
  df$call <- 1:nrow(df)
  
  # slice off (1 - effort_prop) of the observations
  # from each visit to each site and mask the true species labels
  masked <- df %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$site, .data$visit) %>%
    dplyr::slice_sample(prop = 1-effort_prop) %>%
    dplyr::mutate(true_spp = NA)
  
  # None of the observations from visits 2,3, ... ,J get validated (all masked)
  not_masked <- df %>%
    filter(call %notin% masked$call)
  
  # output is a copy of the original
  out_df <- bind_rows(masked, not_masked) %>% arrange(call)
  
  # check that the call columns match for both, and if they do, return a
  # masked copy of the OG df
  if(any(out_df$call != df$call)) {
    stop(message("dfs were not bound together correctly")) # this error should never be seen... 
  } else {
    return(out_df %>% dplyr::select(-call))
  }
  
}
