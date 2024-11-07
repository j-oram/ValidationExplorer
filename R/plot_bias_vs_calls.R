#' plot_bias_vs_calls.R
#'
#' @param sim_summary Simulation summaries in the format output from `run_sims`. This means a dataframe object with columns for `parameter`, (posterior) `Mean`, `truth`, `theta_scenario`, (validation) `scenario`, and `dataset` are all available.
#' @param calls_summary A summary of the validated data as output from `summarize_n_validated`. Must contain the columns `theta_scenario` and `scenario`
#' @param pars A character vector containing the parameters to be visualized. Use one of the following: "psi", "lambda", or "theta".
#' @param regex_pars A string containing the name of a parameter group (e.g., "lambda") to be visualized.
#' @param theta_scenario The classifier ID to be visualized.
#' @param scenarios The ID's corresponding to the validation scenarios to be visualized. Typically a numeric vector (e.g. 1:5)
#' @param convergence_threshold The threshold  value for the Gelman-Rubin statistic. Below this value, a model is considered 'converged' for the purpose of including it in visualizations.
#'
#' @return A ggplot object  of average estimation error vs the number of validated calls under each scenario.
#' @export
#'
#' @examples
#'
#' s1 <- readRDS("Testing/FixedEffortExample/ThetaFE/summary_df_for_scenario_1.rds")
#' s2 <- readRDS("Testing/FixedEffortExample/ThetaFE/summary_df_for_scenario_2.rds")
#'
#' s <- bind_rows(s1, s2)
#' plot_bias_vs_calls(s, calls_summary)
plot_bias_vs_calls <- function(sim_summary,
                               calls_summary, # output from summarize_n_validated
                               pars = NULL,
                               regex_pars = NULL,
                               theta_scenario,
                               scenarios,
                               convergence_threshold = 1.1) {

  plt_df <- sim_summary %>%
    mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    group_by(theta_scenario, scenario, dataset) %>%
    mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    ungroup %>%
    filter(all_converge == 1) %>%
    mutate(est_error = Mean - truth) %>%
    group_by(theta_scenario, scenario, parameter) %>%
    mutate(
      av_est_err = mean(est_error),
      low50 = quantile(est_error, 0.25),
      up50 = quantile(est_error, 0.75)
    ) %>%
    select(theta_scenario, scenario, parameter, av_est_err, low50, up50) %>%
    ungroup() %>%
    mutate(theta_scenario = as.character(theta_scenario),
           scenario = as.character(scenario)) %>%
    left_join(., calls_summary, by = c("scenario", "theta_scenario"))

  if(!is.null(pars)) {
    plt_df <- plt_df %>% filter(parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% filter(str_detect(parameter, regex_pars))
  }


  plt <- plt_df %>%
    ggplot(
      aes(x = n_validated, y = av_est_err, group = parameter, color = parameter)
    ) +
    geom_point() +
    geom_linerange(aes(ymin=low50, ymax=up50))+
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dotted")+
    labs(
      x = "Number of validated calls",
      y = "Average Estimation Error (50% intervals)",
      color = "Parameter"
    )

  return(plt)

}
