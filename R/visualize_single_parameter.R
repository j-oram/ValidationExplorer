visualize_single_parameter <- function(sim_summary,
                                       par, # use a specific parameter (e.g., "psi[1]" or "theta[3, 2]")
                                       theta_scenario,
                                       scenarios,
                                       convergence_threshold = 1.1){
  results <- sim_summary %>%
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
      av_low95 = mean(`2.5%`),
      av_up95 = mean(`97.5%`),
      coverage = mean(capture),
      av_post_mean = mean(Mean)
    )

  plt <- results %>%
    mutate(scenario = factor(scenario)) %>%
    filter(
      all_converge == 1,
      theta_scenario == theta_scenario,
      parameter == par,
      scenario %in% scenarios,
    ) %>%
    ggplot(
      aes(
        x = scenario,
        y = av_post_mean
      )
    ) +
    geom_errorbar(
      aes(
        ymin = `2.5%`,
        ymax = `97.5%`
      ),
      position = position_dodge2(width = 0, padding = 0.1),
      alpha = 0.2
    ) +
    geom_errorbar(
      aes(
        ymin = av_low95,
        ymax = av_up95,
        color = coverage
      )
    ) +
    viridis::scale_color_viridis(limits = c(0,1)) +
    geom_point(color = "red") +
    geom_point(
      inherit.aes = FALSE,
      aes(x = scenario, y = truth)
    ) +
    labs(
      x = "Manual Verification Scenario",
      y = "",
      color = "Coverage"
    )

  return(plt)
}
