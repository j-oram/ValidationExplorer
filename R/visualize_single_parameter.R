visualize_single_parameter <- function(sim_summary,
                                       par, # use a specific parameter (e.g., "psi[1]" or "theta[3, 2]")
                                       theta_scenario,
                                       scenarios,
                                       convergence_threshold = 1.1){
  results <- sim_summary %>%
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
      av_low95 = mean(`2.5%`),
      av_up95 = mean(`97.5%`),
      coverage = mean(capture),
      av_post_mean = mean(Mean)
    )

  plt <- results %>%
    dplyr::mutate(scenario = factor(scenario)) %>%
    dplyr::filter(
      all_converge == 1,
      theta_scenario == theta_scenario,
      parameter == par,
      scenario %in% scenarios,
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = scenario,
        y = av_post_mean
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = `2.5%`,
        ymax = `97.5%`
      ),
      position = ggplot2::position_dodge2(width = 0, padding = 0.1),
      alpha = 0.2
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = av_low95,
        ymax = av_up95,
        color = coverage
      )
    ) +
    viridis::scale_color_viridis(limits = c(0,1)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::eom_point(
      inherit.aes = FALSE,
      ggplot2::aes(x = scenario, y = truth)
    ) +
    ggplot2::labs(
      x = "Manual Verification Scenario",
      y = "",
      color = "Coverage"
    )

  return(plt)
}
