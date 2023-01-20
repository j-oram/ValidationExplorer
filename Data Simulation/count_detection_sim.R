library(tidyverse)
library(nimble)

# Simulate data that comes from (aggregated) count detection model

sim_dat <- function(
  nsites = 100, nspecies = 8, nvisits = 4, seed = NULL, 
  psi = runif(nspecies, .4, .9), 
  lambda = abs(rnorm(nspecies, 0, 100)),
  theta = t(apply(18*diag(nspecies)+2, 1, function(x) rdirch(1, x)))
){
  
  # optional seed
  if(!is.null(seed)) set.seed(seed)
  
  # build empty df
  df <- tibble(
    site = rep(1:nsites, each = nspecies * nspecies * nvisits),
    visit = rep(rep(1:nvisits, each = nspecies * nspecies), nsites),
    true_spp = rep(1:nspecies, nsites * nvisits * nspecies),
    id_spp = rep(rep(1:nspecies, each = nspecies), nsites * nvisits)
  )
  
  df2 <- left_join(
    df, 
    tibble(
      true_spp = 1:nspecies,
      lambda = lambda,
      psi = psi
    ), by = "true_spp"
  ) %>%
    left_join(
      ., 
      tibble(
        true_spp = rep(1:nspecies, nspecies),
        id_spp = rep(1:nspecies, each = nspecies),
        theta = c(theta)
      ), by = c("true_spp", "id_spp")
    )
  
  # latent z state
  df3 <- df2 %>%
    select(site, true_spp, psi) %>%
    distinct() %>%
    mutate(z = rbinom(n(), size = 1, prob = .data$psi)) %>%
    left_join(
      df2, 
      ., 
      by = c("site", "true_spp", "psi")
    ) %>%
    mutate(
    # cijkk' = count of detections truly belonging to spp k that are autoID'd as k'
      # on visit j to site i
      count = rpois(n(), z * lambda * theta)  
    )
  full_df <- df3 # Output required for the data simulation contained in get_sim_datasets.r
  
  # The remaining code is for selecting all calls within a site-night. (Alt. vetting design)
  # (See Stratton et al., (2022) for details 
  # To begin, allocate some unambiguous calls:
  df4 <- df3 %>%
    select(site, visit) %>%
    distinct %>%
    group_by(site) %>%
    sample_frac(.5) %>% 
    mutate(true_spp = NA) %>%
    ungroup()
  
  df5 <- bind_rows(                               # Replace the rows that are "masked" with the ones in df4.
    anti_join(df3, df4, by = c("site", "visit")), # <- Site-visit combos of df3 that are not in df4 ("leftovers"- these become the unambiguous calls)
    inner_join(                                   # <- Retains only rows with matches in (df3 and df4)  (i.e. ambiguous calls)              
      df3 %>% select(-true_spp),
      df4,
      by = c("site", "visit")                     # Result is a copy of OG full_df, but with some of the 
    )                                             # rows (those with sites and visit combos in df4) true spp masked 
  ) %>%
    arrange(site, visit) %>%
    group_by(site, visit, true_spp, id_spp) %>%
    # count now = Cij.k' = number of calls on visit j to site i that are ID'd (either ambiguously (autoID'd), or unambiguously (manual vetting)) as species k'. 
    summarize(count = sum(count)) %>%
    mutate(type = ifelse(is.na(true_spp), "ambiguous", "unambiguous"))
  
  out <- list(df = df5, full_df = full_df, params = list(psi = psi, theta = theta, lambda = lambda))
  return(out)
}

