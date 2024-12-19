sim_CD_with_covs <- function(
    nsites = 100, nspecies = 8, nvisits = 4, seed = NULL,
    psi = runif(nspecies),
    beta = matrix(rnorm(nspecies*2), 2, nspecies), 
    alpha  = matrix(rnorm(nspecies*2), 2, nspecies),
    theta = t(apply(18*diag(nspecies)+2, 1, function(x) rdirch(1, x))), 
    mask = TRUE,
    masking_function = c("random_visit", "all_visits"),
    prop = 0.2
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
  
  # add covariates
  df_sitecovs <- df %>%
    select(site) %>%
    distinct %>%
    mutate(x = rnorm(n()))
  
  df_actcovs <- df %>%
    select(site, visit) %>%
    distinct() %>%
    mutate(w = rnorm(n()))
  
  df1point5 <- left_join(
    df, df_sitecovs, by = "site"
  ) %>%
    left_join(
      ., df_actcovs, by = c("site", "visit")
    )
  
  psi_df <- tibble(
    true_spp = 1:nspecies, 
    psi = psi
  )
  
  df2 <- df1point5 %>%
    rowwise() %>%
    mutate(
      # psi = (exp(beta[, true_spp] %*% c(1, x)) / (1 + exp(beta[, true_spp] %*% c(1, x))))[1,1],
      lambda = (exp(alpha[, true_spp] %*% c(1, w)))[1,1],
    ) %>%
    left_join(
      ., 
      tibble(
        true_spp = rep(1:nspecies, nspecies),
        id_spp = rep(1:nspecies, each = nspecies),
        theta = c(theta)
      ), by = c("true_spp", "id_spp")
    ) %>% 
    # to simulate with covs on occupancy, remove/comment out the next remaining lines,
    # the previous pipe, and the definition of psi_df,
    # then uncomment the line defining psi above.
    left_join(
      ., psi_df, by = "true_spp"
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
      count = rpois(n(), z * lambda * theta)
    ) %>% 
    ungroup() %>% 
    group_by(site, visit) %>% 
    mutate(Y. = sum(count)) %>% 
    ungroup()
  
  
  df_final <- df3 %>% 
    uncount(weights = count, .remove = FALSE) %>% 
    arrange(site, visit, id_spp, true_spp)
  
  if (mask){
    if (masking_function == "all_visits") {
      df_masked <- mask_FE_all_visits(df_final, effort_prop = FE_prop, seed = seed + 1)
    } else if (masking_function == "random_visit") {
      df_masked <- mask_sampled_visit(df_final, effort_prop = prop, seed = seed + 1)
    } else {
      print("You must choose one of the following masking function options: all_visits or random_visit.")
    }
  } else {
    df_masked <- NULL
  }
  
  zeros <- df3 %>% filter(count == 0)
  
  out <- list(
    df = df_masked, 
    zeros = zeros,
    full_df = df_final, 
    params = list(
      beta = beta, 
      theta = theta, 
      psi = psi, 
      alpha = alpha
    )
  )
  return(out)
}
