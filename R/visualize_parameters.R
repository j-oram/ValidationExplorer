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
  # Set theme to black and white
  ggplot2::theme_set(ggplot2::theme_bw())
  
  # create the dataframe for plotting by filtering to model fits where all 
  # parameters had Rhat <= convergence threshold, then summarizing the 
  # average posterior means and bounds of average 95% posterior intervals, as well
  # as the frequentist coverage.
  results <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(.data$Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(.data$below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$all_converge == 1) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$parameter) %>%
    dplyr::mutate(
      av_low95 = mean(.data$`2.5%`),
      av_up95 = mean(.data$`97.5%`),
      coverage = mean(.data$capture),
      av_post_mean = mean(.data$Mean)
    )
  
  # create plot to be returned
  plt <- results %>%
    dplyr::mutate(scenario = factor(.data$scenario)) %>%
    dplyr::filter(
      .data$all_converge == 1,
      .data$theta_scenario == theta_scenario,
      .data$parameter == par,
      .data$scenario %in% scenarios,
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$scenario,
        y = .data$av_post_mean
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$`2.5%`,
        ymax = .data$`97.5%`
      ),
      position = ggplot2::position_dodge2(width = 0, padding = 0.1),
      alpha = 0.2
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$av_low95,
        ymax = .data$av_up95,
        color = .data$coverage
      )
    ) +
    viridis::scale_color_viridis(limits = c(0,1)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_point(
      inherit.aes = FALSE,
      ggplot2::aes(x = .data$scenario, y = .data$truth)
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
  # set the ggplot theme
  ggplot2::theme_set(ggplot2::theme_bw())
  
  # create the dataframe for plotting by filtering to model fits where all 
  # parameters had Rhat <= convergence threshold, then summarizing the 
  # average posterior means and bounds of average 95% posterior intervals, as well
  # as the frequentist coverage.
  results <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(.data$Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(.data$below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$all_converge == 1) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$parameter) %>%
    dplyr::mutate(
      av_low95 = mean(.data$`2.5%`),
      av_up95 = mean(.data$`97.5%`),
      coverage = mean(.data$capture),
      av_post_mean = mean(.data$Mean)
    )
  
  # create the ggplot object to be returned
  plt <- results %>%
    dplyr::mutate(scenario = factor(.data$scenario)) %>%
    dplyr::filter(
      .data$all_converge == 1,
      .data$theta_scenario == theta_scenario,
      stringr::str_detect(.data$parameter, pattern = pars),
      .data$scenario %in% scenarios,
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$scenario,
        y = .data$av_post_mean
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$`2.5%`,
        ymax = .data$`97.5%`
      ),
      position = ggplot2::position_dodge2(width = 0, padding = 0.1),
      alpha = 0.2
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$av_low95,
        ymax = .data$av_up95,
        color = .data$coverage
      )
    ) +
    viridis::scale_color_viridis(limits = c(0,1)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_point(
      inherit.aes = FALSE,
      ggplot2::aes(x = .data$scenario, y = .data$truth)
    ) +
    ggplot2::facet_wrap(
      ~.data$parameter,
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
