% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{\%notin\%}
\alias{\%notin\%}
\title{Negation of \%in\%}
\usage{
x <- c(1,2,3); 1 %notin% x
}
\value{
A logical vector indicating if there are values not in \code{table}.
}
\description{
\verb{\%notin\%} checks if elements are \emph{not} in a vector, acting as the opposite of \code{\%in\%}.
}
\examples{
1 \%notin\% c(2, 3, 4)   # TRUE
2 \%notin\% c(2, 3, 4)   # FALSE
}
\keyword{internal}
