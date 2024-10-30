#' plot_X_vs_calls: Compare validation designs based on simulated inference and expected level of effort
#'
#' @param sim_summary Simulation summary from many datasets under many validation scenarios in the format output by `mcmc_sum()`.
#' @param calls_summary Summary of the validation design in the format as output from `summarize_n_validated.R`
#' @param pars A character vector of parameters to visualize.
#' @param regex_pars String containing the name of a group of parameters to visualize. Must be one of "lambda", "psi", or "theta".
#' @param theta_scenario String or integer containing the theta scenario ID that was used to simulate data in `simulate_validatedData`
#' @param scenarios A vector of integers corresponding to the validation designs you would like to visualize.
#' @param convergence_threshold A threshold for the Gelman-Rubin statistic; values below this threshold indicate that a parameter has converged.
#'
#' @importFrom stats quantile
#'
#' @examples
#' plt_example_data <- readRDS('') # need to make sure there is data available with the package.
#'
#' sim_summary <- plt_example_data$summary
#' calls_summary <- plt_example_data$calls
#'
#' plot_bias_vs_calls(sim_summary, calls_summary, regex_pars = "lambda",
#'                    theta_scenario = "1", scenarios = 1:2,
#'                    convergence_threshold = 1.05)
#'
plot_bias_vs_calls <- function(sim_summary,
                               calls_summary, # output from summarize_n_validated
                               pars = NULL,
                               regex_pars = NULL,
                               theta_scenario,
                               scenarios,
                               convergence_threshold = 1.1) {
  ggplot2::theme_set(theme_bw())

  plt_df <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(theta_scenario, scenario, dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
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
    ggplot2::geom_linerange(ggplot2::aes(ymin=low50, ymax=up50))+
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted")+
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Average Estimation Error (50% intervals)",
      color = "Parameter"
    )

  return(plt)

}

plot_width_vs_calls <- function(sim_summary,
                                calls_summary, # output from summarize_n_validated
                                pars = NULL,
                                regex_pars = NULL,
                                theta_scenario,
                                scenarios,
                                convergence_threshold = 1.1) {
  ggplot2::theme_set(theme_bw())

  plt_df <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(theta_scenario, scenario, dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(all_converge == 1) %>%
    dplyr::mutate(width = `97.5%` - `2.5%`) %>%
    dplyr::group_by(theta_scenario, scenario, parameter) %>%
    dplyr::mutate(
      mean_width = mean(width),
      low50_width = quantile(width, 0.25),
      up50_width = quantile(width, .75)
    ) %>%
    dplyr::select(theta_scenario, scenario, parameter, mean_width, low50_width, up50_width) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(theta_scenario = as.character(theta_scenario),
                  scenario = as.character(scenario)) %>%
    dplyr::left_join(., calls_summary, by = c("scenario", "theta_scenario"))

  if(!is.null(pars)) {
    plt_df <- plt_df %>% dplyr::filter(parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% dplyr::filter(stringr::str_detect(parameter, regex_pars))
  }


  plt <- plt_df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = n_validated, y = mean_width, group = parameter, color = parameter)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(ymin=low50_width, ymax=up50_width))+
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted")+
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Average 95% credible interval width",
      color = "Parameter"
    )

  return(plt)

}

plot_coverage_vs_calls <- function(sim_summary,
                                   calls_summary, # output from summarize_n_validated
                                   pars = NULL,
                                   regex_pars = NULL,
                                   theta_scenario,
                                   scenarios,
                                   convergence_threshold = 1.1) {
  ggplot2::theme_set(theme_bw())

  plt_df <- sim_summary %>%
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
    plt_df <- plt_df %>% dplyr::filter(stringr::str_detect(parameter, regex_pars))
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
