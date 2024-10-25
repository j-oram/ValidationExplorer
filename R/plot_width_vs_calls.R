plot_width_vs_calls <- function(sim_summary,
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
    mutate(width = `97.5%` - `2.5%`) %>%
    group_by(theta_scenario, scenario, parameter) %>%
    mutate(
      mean_width = mean(width),
      low50_width = quantile(width, 0.25),
      up50_width = quantile(width, .75)
    ) %>%
    select(theta_scenario, scenario, parameter, mean_width, low50_width, up50_width) %>%
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
      aes(x = n_validated, y = mean_width, group = parameter, color = parameter)
    ) +
    geom_point() +
    geom_linerange(aes(ymin=low50_width, ymax=up50_width))+
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dotted")+
    labs(
      x = "Number of validated calls",
      y = "Average 95% credible interval width",
      color = "Parameter"
    )

  return(plt)

}
