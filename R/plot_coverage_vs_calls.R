plot_coverage_vs_calls <- function(sim_summary,
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
    group_by(theta_scenario, scenario, parameter) %>%
    mutate(
      coverage = mean(capture),
    ) %>%
    select(theta_scenario, scenario, parameter, coverage) %>%
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
      aes(x = n_validated, y = coverage, group = parameter, color = parameter)
    ) +
    geom_point() +
    geom_line() +
    ylim(0,1) +
    labs(
      x = "Number of validated calls",
      y = "Coverage",
      color = "Parameter"
    )

  return(plt)
}
