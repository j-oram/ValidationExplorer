plot_bias_vs_calls <- function(sim_summary,
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
    dplyr::mutate(est_error = Mean - truth) %>%
    dplyr::group_by(theta_scenario, scenario, parameter) %>%
    dplyr::mutate(
      av_est_err = mean(est_error),
      low50 = quantile(est_error, 0.25),
      up50 = quantile(est_error, 0.75)
    ) %>%
    dplyr::select(theta_scenario, scenario, parameter, av_est_err, low50, up50) %>%
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
      ggplot2::aes(x = n_validated, y = av_est_err, group = parameter, color = parameter)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(aes(ymin=low50, ymax=up50))+
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted")+
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Average Estimation Error (50% intervals)",
      color = "Parameter"
    )

  return(plt)

}
