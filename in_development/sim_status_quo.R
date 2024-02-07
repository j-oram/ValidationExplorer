## Simulate the status quo vetting strategy -- 
# filter the high-quality calls then vet up to 4 per site-night (all site-nights)

library(tidyverse)
library(nimble)

################ Helper function ####################

## A function required for simulating the classification process where high-quality 
# (i.e., high-confidence) recordings are more likely to be correctly classified. 

build_call_specific_theta_row <- function(true_spp, p_correct_id, nspecies){
  
  # define an empty vector
  p <- numeric(length = nspecies)
  # the probability of correct classification is from the logit link above
  p[true_spp] <- p_correct_id
  
  # define x to be the "leftover" probabilities to be defined. This is needed 
  # for normalizing the random dirch draw below.
  x <- (1-p_correct_id) 
  
  # Draw a random dirichlet (reference distance specification, inspired by 
  # Stratton et al., (2022)). The motivation for this is so that they will sum to
  # one; we can then normalize them to add up to the "leftover probability" that 
  # needs to be alloted among the remaining species. 
  y <- rdirch(1, alpha = rep(1/(nspecies - 1), nspecies-1))
  
  # normalize the dirichlet draw so that the values sum to the leftover probability mass x
  y2 <- x * y / sum(y)
  
  # assign the normalized values to the empty slots in the p vector
  p[p==0] <- y2
  
  # return the vector of probabilities. These can now be used to generate autoIDs for each recording.
  return(p)
  
}


################# Data Simulation ##################

nsites <- 10
nvisits <- 2
nspecies <- 3

lambda <- c(3,5,2)
set.seed(1)
psi <- runif(3, .2, .9)
psi

# build empty df
df <- tibble(
  site = rep(1:nsites, each = nspecies  * nvisits),
  visit = rep(rep(1:nvisits, each = nspecies ), nsites),
  true_spp = rep(1:nspecies, nsites * nvisits ),
  #id_spp = rep(rep(1:nspecies, each = nspecies), nsites * nvisits)
)

df2 <- left_join(
  df, 
  tibble(
    true_spp = 1:nspecies,
    lambda = lambda,
    psi = psi
  ), by = "true_spp"
) 

# sim latent z state, generate counts, disaggregate and simulate confidence scores
df3 <- df2 %>%
  select(site, true_spp, psi) %>%
  distinct() %>%
  mutate(z = rbinom(n(), size = 1, prob = .data$psi)) %>%
  left_join(
    df2, 
    ., 
    by = c("site", "true_spp", "psi")
  ) %>% 
  mutate(count = rpois(n(), z*lambda)) %>% 
  uncount(., count, .remove = FALSE) %>% 
  mutate(conf_score = runif(n()))

# b1 will be a user-specified setting when this gets turned into a function. b1
# controls the strength of the relationship between the confidence score and the 
# probability of correct classification. 
b0 <- 0
b1 <- 3

## logit(p(correct classification)) = b0 + b1 * X_confidence
df3$p_correct_id = arm::invlogit(b0 + b1*df3$conf_score)

# autoID ~ categorical(p), where p is a nspecies x 1 vector that is generated 
# using the build_call_specific_theta_row function at the top of this script
autoID <- apply(cbind(df3$true_spp, df3$p_correct_id), 1, 
                FUN=function(x){
                  p <- build_call_specific_theta_row(true_spp = x[1], p_correct_id = x[2], nspecies = 3)
                  out <- rcat(prob = p) 
                  return(out)
                })

full_df <- df3 %>% mutate(autoID = autoID, call = 1:n())

# grab the four highest rated calls for each species-site-night
unambiguous_data <- full_df %>% 
  group_by(site, visit, autoID) %>% 
  arrange(1-conf_score) %>% # arrange highest to lowest 
  slice(1:4) # take the top four 

# the ambiguous data is the rest, with the true_spp masked
ambiguous_data <- setdiff(full_df, unambiguous_data) %>% mutate(true_spp = NA)

# combine these two datasets again to make a masked copy of the original -- 
# this will be the output from the data simulation function
masked_df <- bind_rows(unambiguous_data, ambiguous_data) %>% arrange(call)
