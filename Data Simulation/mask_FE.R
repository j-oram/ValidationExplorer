## Masking function for the fixed effort scenario, which assumes that a fixed
## proportion of recordings from the first visit to each site are validated and
## all other recordings are left as ambiguous. 

## Inputs: 
## - df: An unmasked dataset with an observation in each row
## - effort_prop: the proportion of observations from each site's first night to
## be left unmasked by the function. Must not be 1, or this function will break.
## - seed: an optional seed to reproduce a particular masking of observations. 

## Outputs: 
## A copy of the original df input, but with (1-effort_prop) * 100 % of the 
## observations' true species labels masked by NA

`%notin%` <- Negate(`%in%`)

mask_FE <- function(df, effort_prop, seed = NULL) {
  
  # set the seed if specified by the user
  if(!is.null(seed)) {
    set.seed(seed)
  }
  
  # for housekeeping -- make sure that we aren't double counting any obs 
  # (or missing any)
  df$call <- 1:nrow(df)
  
  # slice off (1 - effort_prop) of the observations 
  # from the first visit to each site and mask the true species labels
  visit1 <- df %>% 
    ungroup() %>%
    filter(visit == 1) %>% 
    group_by(site) %>% 
    slice_sample(prop = 1-effort_prop) %>% 
    mutate(true_spp = NA)
  
  # None of the observations from visits 2,3, ... ,J get validated (all masked)
  visitnot1 <- df %>% 
    ungroup() %>% 
    filter(visit != 1) %>% 
    mutate(true_spp = NA)
  
  # bind all the masked rows together
  masked <- bind_rows(visit1, visitnot1)
  
  # grab the "validated recordings"
  unmasked <- df[which(df$call %notin% masked$call), ]
  
  # bind the two back together and sort by recording
  masked_copy <- bind_rows(masked, unmasked) %>% arrange(call)
  
  # check that the call columns match for both, and if they do, return a 
  # masked copy of the OG df
  if(any(masked_copy$call != df$call)) {
    stop(message("dfs were not bound together correctly"))
  } else {
    return(masked_copy %>% select(-call))
  }
  
}
