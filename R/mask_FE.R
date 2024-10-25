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
    dplyr::ungroup() %>%
    dplyr::filter(visit == 1) %>%
    dplyr::group_by(site) %>%
    dplyr::slice_sample(prop = 1-effort_prop) %>%
    dplyr::mutate(true_spp = NA)

  # None of the observations from visits 2,3, ... ,J get validated (all masked)
  visitnot1 <- df %>%
    dplyr::ungroup() %>%
    dplyr::filter(visit != 1) %>%
    dplyr::mutate(true_spp = NA)

  # bind all the masked rows together
  masked <- dplyr::bind_rows(visit1, visitnot1)

  # grab the "validated recordings"
  unmasked <- df[which(df$call %notin% masked$call), ]

  # bind the two back together and sort by recording
  masked_copy <- dplyr::bind_rows(masked, unmasked) %>%
    dplyr::arrange(call)

  # check that the call columns match for both, and if they do, return a
  # masked copy of the OG df
  if(any(masked_copy$call != df$call)) {
    stop(message("dfs were not bound together correctly"))
  } else {
    return(masked_copy %>% dplyr::select(-call))
  }

}
