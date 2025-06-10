#' plot_bias_vs_calls: Compare validation designs based on estimation error and
#'   expected level of effort
#'
#' @param sim_summary Simulation summary from many datasets under many validation
#'   scenarios in the format output by \link{mcmc_sum}.
#' @param calls_summary Summary of the validation design in the format as output
#'   from \link{summarize_n_validated}
#' @param pars A character vector of parameters to visualize.
#' @param regex_pars String containing the name of a group of parameters to visualize.
#'   Must be one of "lambda", "psi", or "theta".
#' @param theta_scenario String containing the theta scenario ID that
#'   was used to run simulations in \link{run_sims}. This string must match 
#'   between `calls_summary` and `sim_summary`.
#' @param scenarios A vector of integers corresponding to the validation designs
#'   you would like to visualize.
#' @param convergence_threshold A threshold for the Gelman-Rubin statistic; values
#'   below this threshold indicate that a parameter has converged.
#'
#' @importFrom stats quantile
#'
#' @examples
#'
#' sim_summary <- example_output
#' calls_summary <- example_val_sum
#'
#' plot_bias_vs_calls(sim_summary, calls_summary, regex_pars = "lambda",
#'                    theta_scenario = "1", scenarios = 1:2,
#'                    convergence_threshold = 1.05)
#'
#' @export
plot_bias_vs_calls <- function(sim_summary,
                               calls_summary, # output from summarize_n_validated
                               pars = NULL,
                               regex_pars = NULL,
                               theta_scenario,
                               scenarios,
                               convergence_threshold = 1.1) {
  # set themes
  ggplot2::theme_set(ggplot2::theme_grey())
  
  # create the dataframe for plotting that filters to models where all parameters
  # met the convergence threshold, and that contains information about estimation
  # error and 50% intervals for estimation error for each parameter.
  # This is joined with the user-supplied summary of validation effort into the 
  # final df for plotting.
  
  plt_df <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(.data$Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(.data$below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$all_converge == 1) %>%
    dplyr::mutate(est_error = .data$Mean - .data$truth) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$parameter) %>%
    dplyr::mutate(
      av_est_err = mean(.data$est_error),
      low50 = quantile(.data$est_error, 0.25),
      up50 = quantile(.data$est_error, 0.75)
    ) %>%
    dplyr::select(
      .data$theta_scenario,
      .data$scenario,
      .data$parameter,
      .data$av_est_err,
      .data$low50,
      .data$up50
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(theta_scenario = as.character(.data$theta_scenario),
           scenario = as.character(.data$scenario)) %>%
    dplyr::left_join(calls_summary, by = c("scenario", "theta_scenario"))
  
  # if pars is not null, plot only those parameters specified by the user; 
  # otherwise, use check regex pars and use those if specified. If everything
  # is null, go ahead and plot all parameters.
  if(!is.null(pars)) {
    plt_df <- plt_df %>% dplyr::filter(.data$parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% dplyr::filter(stringr::str_detect(.data$parameter, regex_pars))
  }

  # create the ggplot object to be returned
  plt <- plt_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$n_validated,
        y = .data$av_est_err,
        group = .data$parameter,
        color = .data$parameter
      )
    ) +
    ggplot2::geom_linerange(ggplot2::aes(ymin=.data$low50, ymax=.data$up50))+
    ggplot2::geom_line() + 
    ggplot2::geom_label(ggplot2::aes(label = .data$scenario)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted")+
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Average Estimation Error (50% intervals)",
      color = "Parameter"
    )

  return(plt)

}

#' plot_width_vs_calls: Compare validation designs based on 95% posterior interval
#'   width and expected level of effort
#'
#' @param sim_summary Simulation summary from many datasets under many validation
#'   scenarios in the format output by \link{mcmc_sum}.
#' @param calls_summary Summary of the validation design in the format as output
#'   from \link{summarize_n_validated}
#' @param pars A character vector of parameters to visualize.
#' @param regex_pars String containing the name of a group of parameters to
#'   visualize. Must be one of "lambda", "psi", or "theta".
#' @param theta_scenario String containing the theta scenario ID that
#'   was used to run simulations in \link{run_sims}. This string must match 
#'   between `calls_summary` and `sim_summary`.
#' @param scenarios A vector of integers corresponding to the validation designs
#'   you would like to visualize.
#' @param convergence_threshold A threshold for the Gelman-Rubin statistic; values
#'   below this threshold indicate that a parameter has converged.
#'
#' @importFrom stats quantile
#'
#' @examples
#'
#' sim_summary <- example_output
#' calls_summary <- example_val_sum
#'
#' plot_width_vs_calls(sim_summary, calls_summary, regex_pars = "lambda",
#'                    theta_scenario = "1", scenarios = 1:2,
#'                    convergence_threshold = 1.05)
#'
#' @export
plot_width_vs_calls <- function(sim_summary,
                                calls_summary, # output from summarize_n_validated
                                pars = NULL,
                                regex_pars = NULL,
                                theta_scenario,
                                scenarios,
                                convergence_threshold = 1.1) {
  # set theme
  ggplot2::theme_set(ggplot2::theme_grey())
  
  # create the dataframe for plotting that filters to models where all parameters
  # met the convergence threshold, and that contains information about means and
  # 50% intervals for the width of 95% posterior intervals for each parameter.
  # This is joined with the user-supplied summary of validation effort into the 
  # final df for plotting.
  plt_df <- sim_summary %>%
    dplyr::mutate(
      below_threshold = ifelse(round(.data$Rhat, 4) <= convergence_threshold, 1, 0)
    ) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$dataset) %>%
    dplyr::mutate(
      all_converge = ifelse(any(.data$below_threshold == 0), 0, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$all_converge == 1) %>%
    dplyr::mutate(width = .data$`97.5%` - .data$`2.5%`) %>%
    dplyr::group_by(.data$theta_scenario, .data$scenario, .data$parameter) %>%
    dplyr::mutate(
      mean_width = mean(.data$width),
      low50_width = quantile(.data$width, 0.25),
      up50_width = quantile(.data$width, .75)
    ) %>%
    dplyr::select(
      .data$theta_scenario,
      .data$scenario,
      .data$parameter,
      .data$mean_width,
      .data$low50_width,
      .data$up50_width
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(theta_scenario = as.character(.data$theta_scenario),
                  scenario = as.character(.data$scenario)) %>%
    dplyr::left_join(calls_summary, by = c("scenario", "theta_scenario"))

  # if pars is not null, plot only those parameters specified by the user; 
  # otherwise, use check regex pars and use those if specified. If everything
  # is null, go ahead and plot all parameters.
  if(!is.null(pars)) {
    plt_df <- plt_df %>% dplyr::filter(.data$parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% dplyr::filter(stringr::str_detect(.data$parameter, regex_pars))
  } else {
    plt_df <- plt_df
  }

  # create ggplot object
  plt <- plt_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$n_validated,
        y = .data$mean_width,
        group = .data$parameter,
        color = .data$parameter
      )
    ) +
    ggplot2::geom_linerange(ggplot2::aes(ymin=.data$low50_width, ymax=.data$up50_width))+
    ggplot2::geom_line() +
    ggplot2::geom_label(ggplot2::aes(label = .data$scenario)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Average 95% credible interval width",
      color = "Parameter"
    )

  return(plt)

}

#' plot_coverage_vs_calls: Compare validation designs based on coverage of 95%
#'   posterior intervals and expected level of effort
#'
#' @param sim_summary Simulation summary from many datasets under many validation
#'   scenarios in the format output by \link{mcmc_sum}.
#' @param calls_summary Summary of the validation design in the format as output
#'   from \link{summarize_n_validated}
#' @param pars A character vector of parameters to visualize.
#' @param regex_pars String containing the name of a group of parameters to
#'   visualize. Must be one of "lambda", "psi", or "theta".
#' @param theta_scenario String containing the theta scenario ID that
#'   was used to run simulations in \link{run_sims}. This string must match 
#'   between `calls_summary` and `sim_summary`.
#' @param scenarios A vector of integers corresponding to the validation designs
#'   you would like to visualize.
#' @param convergence_threshold A threshold for the Gelman-Rubin statistic; values
#'   below this threshold (and near 1) indicate that a parameter has converged.
#'
#' @importFrom stats quantile
#'
#' @examples
#'
#' sim_summary <- example_output
#' calls_summary <- example_val_sum
#'
#' plot_bias_vs_calls(sim_summary, calls_summary, regex_pars = "lambda",
#'                    theta_scenario = "1", scenarios = 1:2,
#'                    convergence_threshold = 1.05)
#'

#' @export
plot_coverage_vs_calls <- function(sim_summary,
                                   calls_summary, # output from summarize_n_validated
                                   pars = NULL,
                                   regex_pars = NULL,
                                   theta_scenario,
                                   scenarios,
                                   convergence_threshold = 1.1) {
  # set the ggplot theme
  ggplot2::theme_set(ggplot2::theme_grey())
  
  # create the dataframe for plotting that filters to models where all parameters
  # met the convergence threshold, and that contains information about coverage. 
  # this is joined with the user-supplied summary of validation effort into the 
  # final df for plotting.
  plt_df <- sim_summary %>%
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
      coverage = mean(.data$capture),
    ) %>%
    dplyr::select(
      .data$theta_scenario,
      .data$scenario,
      .data$parameter,
      .data$coverage
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(theta_scenario = as.character(.data$theta_scenario),
                  scenario = as.character(.data$scenario)) %>%
    dplyr::left_join(calls_summary, by = c("scenario", "theta_scenario"))
  
  # if pars is not null, plot only those parameters specified by the user; 
  # otherwise, use check regex pars and use those if specified.
  if(!is.null(pars)) {
    plt_df <- plt_df %>% dplyr::filter(.data$parameter %in% pars)
  } else if(!is.null(regex_pars)) {
    plt_df <- plt_df %>% dplyr::filter(stringr::str_detect(.data$parameter, regex_pars))
  }

  # create plots
  plt <- plt_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$n_validated,
        y = .data$coverage,
        group = .data$parameter,
        color = .data$parameter
      )
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_label(ggplot2::aes(label = .data$scenario)) +
    ggplot2::ylim(0,1) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::geom_hline(yintercept = 0.95, linetype = "dotted") +
    ggplot2::labs(
      x = "Number of validated calls",
      y = "Coverage",
      Shape = "Parameter"
    )

  return(plt)
}
