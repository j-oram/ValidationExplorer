# This script determines the parameter values for the simulations described in 
# the main text. It is provided for the purposes of reproducibility, 
# but to conduct your own simulation study, you should use simulate_BySpeciesValidation.R


# MYLU, MYEV, MYCI, TABR, LACI
psi_lambda <- rbind(c(.90, 28),
                    c(.70, 2.4),
                    c(.24, 11.9),
                    c(.1, 1),
                    c(.61, 4)) %>% 
  as.data.frame() 

colnames(psi_lambda) <- c("psi", "lambda")   
  

  (theta1 <- rbind(
    c(.95, .02, .01, .01, .01),
    c(.05, .90, .02, .02, .01),
    c(.01, .02, .95, .01, .01),
    c(.02, .03, .05, .85, .05),
    c(.04, .01, .01, .01, .93)
  ))

(theta2 <- rbind(
  c(.55, .10, .10, .15, .10),
  c(.05, .90, .02, .02, .01),
  c(.03, .02, .87, .04, .04),
  c(.02, .03, .05, .85, .05),
  c(.04, .01, .01, .01, .93)
))


(theta3 <- rbind(
  c(.95, .02, .01, .01, .01),
  c(.05, .90, .02, .02, .01),
  c(.03, .02, .87, .04, .04),
  c(.31, .01, .01, .65, .02),
  c(.04, .01, .01, .01, .93)
))

# 2 and 3?

# diffuse diagonals?

## threshold stuff

val_efforts <- expand.grid(
  rare = c(1),
  medium = c(.75, .5, .25),
  high = c(.5, .25, .1)
) %>%
  as_tibble() %>%
  arrange(rare, desc(medium), desc(high)) %>%
  mutate(count = 4) %>%
  uncount(count) %>%
  mutate(theta_cat = rep(1:4, 9))

val_efforts
