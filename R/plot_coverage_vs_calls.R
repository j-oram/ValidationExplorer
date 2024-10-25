plot_coverage_vs_calls <- function(sim_summary,
                                   calls_summary, # output from summarize_n_validated
                                   pars = NULL,
                                   regex_pars = NULL,
                                   theta_scenario,
                                   scenarios,
                                   convergence_threshold = 1.1) {

  plt_df <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(theta_scenario, scenario, dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup %>%
    dplyr::filter(all_converge == 1) %>%
    dplyr::group_by(theta_scenario, scenario, parameter) %>%
    dplyr::mutate(
      coverage = mean(capture),
    ) %>%
    dplyr::select(theta_scenario, scenario, parameter, coverage) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(theta_scenario = as.character(theta_scenario),
           scenario = as.character(scenario)) %>%
    dplyr::left_join(., calls_summary, by = c("scenario", "theta_scenario"))

  if(!is.null(pars)) {
    plt_df <- plt_df %>% dplyr::filter(parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% dplyr::filter(str_detect(parameter, regex_pars))
  }


  plt <- plt_df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = n_validated, y = coverage, group = parameter, color = parameter)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::ylim(0,1) +
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Coverage",
      color = "Parameter"
    )

  return(plt)
}
