#' Suppress R CMD check notes for global variables
#'
#' The following variables are used in NIMBLE model specification blocks and
#' non-standard evaluation contexts within  tidyverse/dplyr
#' functions. This declaration satisfies R CMD check requirements.
#'
#' @keywords internal
#' @noRd

utils::globalVariables(
  c(
    # NIMBLE model constants and data structures
    "nspecies", "nsites", "nvisits", "n_confirmed_calls", "z", "site1",
    "lambda", "n_ambiguous_calls", "site2", "theta",
    
    # Variables used in masking and simulation functions
    ".", "scenario", "site_visit_idspp_number"
  )
)