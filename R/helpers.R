#' Negation of %in%
#'
#' `%notin%` checks if elements are *not* in a vector, acting as the opposite of `%in%`.
#'
#' @param x Vector of values to check
#' @param table Vector to check against
#' @return A logical vector indicating if there are values not in `table`.
#' @keywords internal
#' @export
#' @examples
#' 1 %notin% c(2, 3, 4)   # TRUE
#' 2 %notin% c(2, 3, 4)   # FALSE
`%notin%` <- function(x, table) {
  !(x %in% table)
}

#' Further masking of true species labels to mimic 
#' whether a recording is confirmable
#' 
#' @export
#' @return A copy of the original dataframe, with some selected observations masked
#' because they could not be confirmed. 
#' @keywords internal
make_not_confirmable <- function(masked_df, confirmable_limits) {
  
  if (any(confirmable_limits < 0) | any(confirmable_limits > 1)) {
    stop(message("`confirmable_limits` must be in the range [0,1]."))
  }
  
  # Get the proportion confirmable. We assume it is a random 
  # value for each site visit. 
    masked_df <- masked_df %>% 
      dplyr::group_by(.data$site, .data$visit) %>% 
      dplyr::mutate(
        prop_confirmable = stats::runif(
          1, 
          min = confirmable_limits[1], 
          max = confirmable_limits[2]
        )
      ) %>% 
      dplyr::ungroup()
  
  # split df into the selected and not selected components
  selected_df <- masked_df %>% 
    dplyr::filter(.data$selected == 1)
  not_selected <- dplyr::setdiff(masked_df, selected_df)
  
  confirmable_df <- selected_df %>% 
    dplyr::group_split(.data$site, .data$visit, .keep = TRUE) %>% 
    purrr::map(\(x) dplyr::slice_sample(x, prop = unique(x$prop_confirmable))) %>% 
    purrr::list_rbind()
  
  # If not confirmable, then the true spp label remains ambiguous (has value NA)
  not_confirmable <- dplyr::setdiff(selected_df, confirmable_df)
  not_confirmable$true_spp <- NA
  
  # Bind together a copy of selected subset that has confirmed and non-
  # confirmable recordings in it
  selected_out <- dplyr::bind_rows(confirmable_df, not_confirmable) %>% 
    dplyr::arrange(.data$unique_call_id)
  
  # Bind copy of selected subset with the not selected recordings
  masked_df_new <- dplyr::bind_rows(selected_out, not_selected)
  
  return(masked_df_new)
}
