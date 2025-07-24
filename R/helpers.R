#' L: A labeller function from Ben Bolker on Stack Overflow
#'
#' @description
#' A function that allows symbols (like parameters with indices) to be parsed
#'   in ggplot facet labels
#'
#' @keywords internal
#' @export
L <- function(labels,multi_line=TRUE) {
  r <- if (all(grepl("\n",labels[[1]]))) {
    list(as.character(labels[[1]]))
  } else {
    ggplot2::label_parsed(labels,multi_line=multi_line)
  }
  ## browser()
  return(r)
}
class(L) <- "labeller"

#' Negation of %in%
#'
#' `%notin%` checks if elements are *not* in a vector, acting as the opposite of `%in%`.
#'
#' @usage x <- c(1,2,3); 1 %notin% x
#'
#' @export
#' @return A logical vector indicating if there are values not in `table`.
#' @keywords internal
#' @examples
#' 1 %notin% c(2, 3, 4)   # TRUE
#' 2 %notin% c(2, 3, 4)   # FALSE
`%notin%` <- Negate(`%in%`)


#' Further masking of true species labels to mimic 
#' whether a recording is confirmable
#' 
#' @export
#' @return A copy of the original dataframe, with some selected observations masked
#' because they could not be confirmed. 
#' @keywords internal
make_not_confirmable <- function(masked_df, confirmable_limits, phi_vec) {
  masked_df$selected <- ifelse(!is.na(masked_df$true_spp), "Y", "N")
  
  if (!is.null(phi_vec) & !is.null(confirmable_limits)) {
    stop(message("`confirmable_limits` and `phi_vec` cannot both be specified.\nPlease make one NULL."))
  }
  
  # Get the proportion confirmable depending on whether we assume
  # each species has a different probability of being confirmed, 
  # or it is a random proportion for each site visit. 
  if (!is.null(phi_vec)) { # the 'confirmable by species case'

    spp_conf_probs_df <- dplyr::tibble(
      true_spp = unique(masked_df$true_spp[!is.na(masked_df$true_spp)]), 
      phi = phi_vec
    )
    
    masked_df <- dplyr::left_join(masked_df, spp_conf_probs_df, by = "true_spp")
  } else { # the 'random proportion of each site-visit' case
    masked_df <- masked_df %>% 
      dplyr::group_by(site, visit) %>% 
      dplyr::mutate(
        prop_confirmable = stats::runif(
          1, 
          min = confirmable_limits[1], 
          max = confirmable_limits[2]
        )
      ) %>% 
      dplyr::ungroup()
  }
  
  # split df into the selected and not selected components
  selected <- masked_df %>% 
    dplyr::filter(selected == "Y")
  not_selected <- dplyr::setdiff(masked_df, selected)
  
  # If selected: Pull off the calls that were selected and can be confirmed
  if (!is.null(phi_vec)) {
    confirmable <- selected %>% 
      dplyr::group_by(true_spp) %>% 
      dplyr::mutate(conf_indicator = stats::rbinom(dplyr::n(), size = 1, prob = .data$phi)) %>% 
      dplyr::filter(conf_indicator == 1) %>% 
      dplyr::select(-conf_indicator)
      
  } else {
    confirmable <- selected %>% 
      dplyr::group_split(site, visit, .keep = TRUE) %>% 
      purrr::map(\(x) dplyr::slice_sample(x, prop = unique(x$prop_confirmable))) %>% 
      purrr::list_rbind()
  }
  
  # If not confirmable, then the true spp label remains ambiguous (has value NA)
  not_confirmable <- dplyr::setdiff(selected, confirmable)
  not_confirmable$true_spp <- NA
  
  # Bind together a copy of selected subset that has confirmed and non-
  # confirmable recordings in it
  selected_out <- dplyr::bind_rows(confirmable, not_confirmable) %>% 
    dplyr::arrange(unique_call_id)
  
  # Bind copy of selected subset with the not selected recordings
  masked_df_new <- dplyr::bind_rows(selected_out, not_selected)
  
  return(masked_df_new)
}