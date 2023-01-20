## Code for generating the summary figures shown in Appendix 1. This is also the base 
# code that can be used to generate the figures shown in the Results section of the 
# main text with some minor filtering before calling ggplot( ) 

# This script assumes that an object called "results" already exists, which is a large 
# dataframe that summarizes all results from simulations under all classifiers and all vetting scenarios. 
# Assuming that simulations ran correctly, this is the output of run_sims.r

library(tidyverse)

results$theta_scenario <- factor(results$theta_scenario)
levels(results$theta_scenario) <- c("well-behaved", "common~to~rare", "rare~to~common")

c2 <- results  %>% 
  filter(all_converge1.2 == 1) %>% 
  group_by(theta_scenario, parameter, scenario) %>% 
  summarize(all_converge1.2_coverage = mean(capture))

results <- left_join(results, c2)

# labeller function -- borrowed from Ben Bolker on Stack Overflow; 
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

# For convenience
`%notin%` <- Negate(`%in%`)

# assign expressions to be parsed for psi lambda -- used in ggplot facets 
results$true_spp_pars <- factor(results$parameter)
levels(results$theta_scenario) <- c("well-behaved", "common~to~rare", "rare~to~common")
levels(results$true_spp_pars)[1:5] <- c(expression(paste(psi, "= .9, ", lambda, "= 28")),
                                         expression(paste(psi, "= .7, ", lambda, "= 2.4")),
                                         expression(paste(psi, "= .24, ", lambda, "= 11.9")),
                                         expression(paste(psi, "= .1, ", lambda, "= 1")),
                                         expression(paste(psi, "= .61,", lambda, "= 4")))

levels(results$true_spp_pars)[6:10] <- c(expression(paste(psi, "= .9, ", lambda, "= 28")),
                                         expression(paste(psi, "= .7, ", lambda, "= 2.4")),
                                         expression(paste(psi, "= .24, ", lambda, "= 11.9")),
                                         expression(paste(psi, "= .1, ", lambda, "= 1")),
                                         expression(paste(psi, "= .61,", lambda, "= 4")))



results %>% 
  filter(
    str_detect(
      string = parameter, 
      pattern = "theta"
    ), 
    theta_scenario == "well-behaved"
  ) %>% 
  ggplot(
    aes(
      x = factor(scenario), 
      y = mean_converge
    )
  ) + 
  geom_point() + 
  ylim(0,1) + 
  labs(
    x = "Validation Scenario", 
    y = "Convergence rate"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8)) +
  facet_wrap(~parameter)




results %>% 
  filter(
    str_detect(
      string = parameter, 
      pattern = "theta"
    ), 
    theta_scenario == "common~to~rare"
  ) %>% 
  ggplot(
    aes(
      x = factor(scenario), 
      y = mean_converge
    )
  ) + 
  geom_point() + 
  ylim(0,1) + 
  labs(
    x = "Validation Scenario", 
    y = "Proportion of Datasets that Converged", 
    title = expression(
      paste("Convergence by Validation Effort: ", Theta)
    )
  ) +
  facet_wrap(~parameter)



results %>% 
  filter(
    str_detect(
      string = parameter, 
      pattern = "theta"
    ), 
    theta_scenario == "rare~to~common"
  ) %>% 
  ggplot(
    aes(
      x = factor(scenario), 
      y = mean_converge
    )
  ) + 
  geom_point() + 
  ylim(0,1) + 
  labs(
    x = "Validation Scenario", 
    y = "Proportion of Datasets that Converged", 
    title = expression(
      paste("Convergence by Validation Effort: ", Theta[3])
    )
  ) +
  facet_wrap(~parameter)



results %>% 
  mutate(scenario = factor(scenario)) %>% 
  filter(
    all_converge1.2 == 1,
    theta_scenario == "well-behaved",
    str_detect(parameter, pattern = "theta") 
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
      color = all_converge1.2_coverage
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
  ) +
  labs(
    x = "Manual Verification Scenario", 
    y = "", 
    color = "Coverage"
  )


results %>% 
  mutate(scenario = factor(scenario)) %>% 
  filter(
    all_converge1.2 == 1,
    theta_scenario == "common~to~rare",
    str_detect(parameter, pattern = "theta") 
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
      color = all_converge1.2_coverage
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
  ) +
  labs(
    x = "Manual Verification Scenario", 
    y = "", 
    color = "Coverage"
  )
ggsave("Drafts/Appendix_figures/theta2.png", width = 8, height = 8)



results %>% 
  mutate(scenario = factor(scenario)) %>% 
  filter(
    all_converge1.2 == 1,
    theta_scenario == "rare~to~common",
    str_detect(parameter, pattern = "theta") 
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
      color = all_converge1.2_coverage
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
  ) +
  labs(
    x = "Manual Verification Scenario", 
    y = "", 
    color = "Coverage"
  )
ggsave("Drafts/Appendix_figures/theta3.png", width = 8, height = 8)



# Convergence figure for lambda
 results %>%
  group_by(theta_scenario, scenario, parameter) %>% 
  mutate(mean_converge1.2 = mean(converge1.2)) %>% 
  # group_by(theta_scenario, scenario, dataset) %>% 
  # summarize(converged = unique(all_converge1.2)) %>% 
  # group_by(theta_scenario, scenario) %>% 
  # summarize(n_converged = sum(converged), 
  #           prop_converged = n_converged/50) %>% 
  # left_join(results, .) %>% 
  filter(str_detect(parameter, "lam")) %>% 
  ggplot(
    aes(
      x = factor(scenario),
      y = mean_converge1.2,
      color = parameter
    )
  ) +
  geom_point() +
  ylim(0,1) +
  labs(
    x = "Validation Scenario",
    y = "Convergence rate",
    # title = expression(
    #   paste("Convergence by Validation Effort: ", lambda[k])
    # )
  ) +
  facet_grid(theta_scenario~parameter , labeller = L) +
  theme(
    legend.position = "none", 
    text = element_text(size = 18), 
    axis.text.x = element_text(size = 10)
  )

results %>%
  filter(
    str_detect(parameter, "psi"), 
    all_converge1.2 == 1, 
    
  ) %>% 
  ggplot(
    aes(
      x = factor(scenario), 
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
      color = all_converge1.2_coverage
    )
  ) +
  scale_color_gradient(low = "red", high = "blue", limits = c(0,1)) +
  geom_point(color = "red") +
  geom_point(
    inherit.aes = FALSE,
    aes(x = scenario, y = truth)
  ) + 
  facet_grid(theta_scenario~true_spp_pars, labeller = L) +
  labs(
    x = "Manual Verification Scenario", 
    y = "", 
    color = "Coverage"
   ) +
  theme(text = element_text(size = 18), 
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size = 10))


results %>%
  filter(
    str_detect(parameter, "lam"),
    all_converge1.2 == 1, 
  ) %>% 
  ggplot(
    aes(
      x = factor(scenario), 
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
      color = all_converge1.2_coverage
    )
  ) +
  scale_color_gradient(low = "red", high = "blue", limits = c(0,1)) +
  geom_point(color = "red") +
  geom_point(
    inherit.aes = FALSE,
    aes(x = scenario, y = truth)
  ) + 
  facet_grid(
    theta_scenario~true_spp_pars, 
    labeller = L
  ) +
  labs(
    x = "Manual Verification Scenario", 
    y = "", 
    color = "Coverage"
   ) +
  theme( text = element_text(size = 18), 
         strip.text.x = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         )

