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
