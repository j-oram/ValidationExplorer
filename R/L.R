#' L: A labeller function borrowed from Ben XX on stack exchange.
#'
#' @param labels The labels to be parsed.
#' @param multi_line Whether the to print facet labels on
#'
#' @return facet labels
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' tib <- tibble(
#'   pars = paste0("lambda[", rep(1:2, each = 5), "]"),
#'   vals = rnorm(10)
#' )
#'
#' tib %>%
#'   ggplot(aes(x = vals)) +
#'   geom_histogram() +
#'   facet_wrap(~pars, labeller = L)
#'
L <- function(labels,multi_line=TRUE) {
  r <- if (all(grepl("\n",labels[[1]]))) {
    list(as.character(labels[[1]]))
  } else {
    label_parsed(labels,multi_line=multi_line)
  }
  ## browser()
  return(r)
}
class(L) <- "labeller"
