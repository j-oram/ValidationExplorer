#' mask_sampled_visit: A function for simulating a fixed effort validation design.
#' 
#' This function implements a fixed effort design, but rather than validating a 
#'   random x% from the first visit, which assumes that the first visit is 
#'   representative of all others in terms of the number of recordings that are 
#'   expected, this design randomly selects y% from each visit. 
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
#' library(dplyr)
#' cd_data <- sim_dat()$full_df %>% tidyr::uncount(weights = count, .remove = FALSE)
#'
#' FE_data <- mask_sampled_visit(cd_data, effort_prop = 0.2)
#'
#' head(cd_data)
#' head(FE_data)
#' 
#' cd_data %>% 
#'   group_by(site, visit) %>% 
#'   summarize(prop_vald = sum(is.na(true_spp))/n())


mask_sampled_visit <- function (df, effort_prop, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  df$call <- 1:nrow(df)
  nvisits <- max(df$visit)
  
  out <- df %>% 
    dplyr::ungroup() %>% 
    dplyr::group_split(.data$site) %>% # split into list of dataframes one for each site
    lapply(., function (x) {
      visit_to_mask <- sample(1:nvisits, 1) # grab an integer between 1 and nvisits
      
      masked_calls <- x %>% 
        dplyr::filter(.data$visit == visit_to_mask) %>% # filter to just that visit
        dplyr::slice_sample(prop = 1 - effort_prop) %>% # slice off the correct proportion
        dplyr::mutate(true_spp = NA) # mask the true species
      
      # no other visits are validated, so mask all true species labels
      other_visits <- x %>% 
        dplyr::filter(.data$visit != visit_to_mask) %>% 
        mutate(true_spp = NA)
      
      unmasked <- x[which(x$call %notin% masked_calls$call & x$call %notin% other_visits$call), ]
      
      masked_version <- dplyr::bind_rows(masked_calls, unmasked, other_visits) %>% 
        dplyr::arrange(site, visit, true_spp, id_spp)
      
      return(masked_version)
    }) %>% 
    do.call(eval(parse(text = "dplyr::bind_rows")), .)
  
  return(out)
}
