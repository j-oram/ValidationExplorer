#' mask_FE
#'
#' @param df A dataframe object
#' @param effort_prop The proportion of observations from the first visit to be validated.
#' @param seed An optional see to make masking/validation simulation reproducible
#'
#' @return A dataframe object with `effort_prop` of the observations validated from the first visit to each site.
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
#' FE_validated_data <- mask_FE(dat, effort_prop = .2, seed = 17)
#'
#' head(dat)
#' head(FE_validated_data)
mask_FE <- function(df, effort_prop, seed = NULL) {

  # set the seed if specified by the user
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # for housekeeping -- make sure that we aren't double counting any obs
  # (or missing any)
  df$call <- 1:nrow(df)

  # slice off (1 - effort_prop) of the observations
  # from the first visit to each site and mask the true species labels
  visit1 <- df %>%
    ungroup() %>%
    filter(visit == 1) %>%
    group_by(site) %>%
    slice_sample(prop = 1-effort_prop) %>%
    mutate(true_spp = NA)

  # None of the observations from visits 2,3, ... ,J get validated (all masked)
  visitnot1 <- df %>%
    ungroup() %>%
    filter(visit != 1) %>%
    mutate(true_spp = NA)

  # bind all the masked rows together
  masked <- bind_rows(visit1, visitnot1)

  # grab the "validated recordings"
  unmasked <- df[which(df$call %notin% masked$call), ]

  # bind the two back together and sort by recording
  masked_copy <- bind_rows(masked, unmasked) %>% arrange(call)

  # check that the call columns match for both, and if they do, return a
  # masked copy of the OG df
  if(any(masked_copy$call != df$call)) {
    stop(message("dfs were not bound together correctly"))
  } else {
    return(masked_copy %>% select(-call))
  }

}
