# Code for visualizing the simulations.
# First, define some helper functions: 

# For convenience
`%notin%` <- Negate(`%in%`)

# Labeller function -- borrowed from Ben Bolker on Stack Overflow; 
# useful for getting parameter values parsed in ggplot facets

L <- function(labels,multi_line=TRUE) {
  r <- if (all(grepl("\n",labels[[1]]))) {
    list(as.character(labels[[1]]))
  } else {
    label_parsed(labels,multi_line=multi_line)
  }
  ## browser()
  return(r)
}
class(L) <- "labeller"


############ functions for visualizing simulation results #############

# visualize_parameter_group creates figures
visualize_parameter_group <- function(sim_summary, 
                                      pars, 
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
      str_detect(parameter, pattern = pars), 
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
    scale_color_gradient(low = "red", high = "blue", limits = c(0,1)) +
    geom_point(color = "red") +
    geom_point(
      inherit.aes = FALSE,
      aes(x = scenario, y = truth)
    ) + 
    facet_wrap(
      ~parameter, 
      #scales = "free_y"
      labeller = L
    ) +
    labs(
      x = "Manual Verification Scenario", 
      y = "", 
      color = "Coverage"
    )
  
  return(plt)
}

visualize_single_parameter <- function(sim_summary, 
                                       par, 
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
    scale_color_gradient(low = "red", high = "blue", limits = c(0,1)) +
    geom_point(color = "red") +
    geom_point(
      inherit.aes = FALSE,
      aes(x = scenario, y = truth)
    ) + 
    # facet_wrap(
    #   ~scenario, 
    #   #scales = "free_y"
    #   labeller = L
    # ) +
    labs(
      x = "Manual Verification Scenario", 
      y = "", 
      color = "Coverage"
    )
  
  return(plt)
}
