#' example_val_sum: Example summaries of validated data
#'
#' Example output from \link{summarize_n_validated}.
#'
#' \itemize{
#'   \item{theta_scenario. The ID of the classifier scenario.}
#'   \item{scenario. The validation scenario index.}
#'   \item{n_validated. The expected number of validated calls in an average dataset simulated under each scenario.}
#' }
#'
#' @docType data
#' @name example_val_sum
#' @format A dataframe with 2 rows and 3 columns
#' @source Data was simulated using \link{simulate_validatedData} and summarized
#'   using \link{summarize_n_validated}.
#'
#' @examples
#' example_val_sum
NULL
