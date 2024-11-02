#' visualize_single_parameter
#'
#' See detailed simulation study results for a parameter of interest
#'
#' @param sim_summary Summary output in the format of \link{run_sims}.
#' @param par The parameter to be visualized.
#' @param theta_scenario The theta scenario IDs.
#' @param scenarios Scenarios to be visualized.
#' @param convergence_threshold If the Gelman-Rubin statistic is below this value,
#'   consider an MCMC to have converged. Default value is 1.1, but we recommend 1.05.
#'
#' @return A ggplot object summarizing simulation results.
#' @export
#'
#' @examples
#'
#' visualize_single_parameter(
#'   example_output,
#'   par = "lambda[1]",
#'   theta_scenario = "1",
#'   scenarios = 1:2,
#'   convergence_threshold = 1.05
#' )
#'
visualize_single_parameter <- function(sim_summary,
                                       par, # use a specific parameter (e.g., "psi[1]" or "theta[3, 2]")
                                       theta_scenario,
                                       scenarios,
                                       convergence_threshold = 1.1){
  ggplot2::theme_set(ggplot2::theme_bw())
  results <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(theta_scenario, scenario, dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
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
    ggplot2::geom_point(
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

#' visualize_parameter_group
#'
#' Visualize simulation results for entire groups of parameters
#'
#'
#' @param sim_summary Summary output in the format of `run_sims()`.
#' @param pars The parameters to be visualized.
#' @param theta_scenario The theta scenario IDs.
#' @param scenarios Scenarios to be visualized.
#' @param convergence_threshold If the Gelman-Rubin statistic is below this value,
#'   consider an MCMC to have converged. Default value is 1.1, but we recommend 1.05.
#'
#' @return A ggplot object summarizing simulation results.
#' @export
#'
#' @examples
#'
#' visualize_parameter_group(
#'   example_output,
#'   pars = "lambda",
#'   theta_scenario = "1",
#'   scenarios = 1:2
#' )
#'
visualize_parameter_group <- function(sim_summary,
                                      pars,
                                      theta_scenario,
                                      scenarios,
                                      convergence_threshold = 1.1){
  ggplot2::theme_set(ggplot2::theme_bw())

  results <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(theta_scenario, scenario, dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
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
      stringr::str_detect(parameter, pattern = pars),
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
    ggplot2::geom_point(
      inherit.aes = FALSE,
      ggplot2::aes(x = scenario, y = truth)
    ) +
    ggplot2::facet_wrap(
      ~parameter,
      #scales = "free_y"
      labeller = L
    ) +
    ggplot2::labs(
      x = "Manual Verification Scenario",
      y = "",
      color = "Coverage"
    )

  return(plt)
}
