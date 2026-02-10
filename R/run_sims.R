#' run_sims: conduct simulations easily
#'
#'
#' @param data_list nested list of masked dataframes (datasets nested within scenarios --
#'   this is the format of `masked_dfs` output from \link{simulate_validatedData})
#' @param zeros_list list of dataframes containing the site/visit/true_spp/id_spp combinations
#'   where no calls were observed.
#' @param DGVs A named list with entries psi, lambda and theta containing
#'   the true values of the respective parameters.
#' @param theta_scenario_id A character string ID for the simulations being run.
#' @param parallel Should models be fit in parallel? Default value is TRUE.
#' @param niter Number of iterations per MCMC chain.
#' @param nburn Number of warmup iterations.
#' @param thin Thinning interval for the MCMC chains.
#' @param nchains The number of chains.
#' @param save_fits Should individual model fits be saved? This could require large
#'   amounts of disk space if you are fitting many large models to big datasets.
#'   Default value is FALSE.
#' @param save_individual_summaries_list Should summaries for individual model fits
#'   be saved? While this requires much less space than `save_fits`, we still
#'   recommend keeping this at the default value of FALSE. Only use it if you
#'   anticipate that simulations may be interrupted.
#' @param directory The directory to save objects. Defaults to the current working directory.
#'
#' @export
#'
#' @return a dataframe with the summaries (from \link{mcmc_sum}) for all scenarios and datasets.
#'   A copy of this output is also saved to the current working directory.
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples
#' # :::::::::::: Simulate data ::::::::::::: #
#' psi <- c(0.3, 0.6)
#' lambda <- c(11, 2)
#'
#' nspecies <- length(psi)
#' nsites <- 30
#' nvisits <- 5
#'
#' test_theta1 <- matrix(c(0.9, 0.1, 0.15, 0.85), byrow = TRUE, nrow = 2)
#' val_scenarios <- list(spp1 = c(.75, .5), spp2 = .5)
#'
#' fake_data <- simulate_validatedData(
#'   n_datasets = 5,
#'   design_type = "BySpecies",
#'   scenarios = val_scenarios,
#'   nsites = nsites,
#'   nvisits = nvisits,
#'   nspecies = nspecies,
#'   psi = psi,
#'   lambda = lambda,
#'   theta = test_theta1,
#'   save_datasets = FALSE,
#'   save_masked_datasets = FALSE,
#'   directory = paste0(here::here("Testing"))
#' )
#'
#' # ::::::::::::: run simulations on sim'd data ::::::::::: #
#'
#' \dontrun{
#' out <- run_sims(
#'   data_list = fake_data$masked_dfs,
#'   zeros_list = fake_data$zeros,
#'   DGVs = list(lambda = lambda, psi = psi, theta = test_theta1),
#'   theta_scenario_id = 'StratBySpecies_1',
#'   parallel = TRUE,
#'   niter = 1000,
#'   nburn = 500,
#'   thin = 1,
#'   save_fits = FALSE,
#'   save_individual_summaries_list = FALSE,
#'   directory = here::here()
#' )
#' }

run_sims <- function(data_list, zeros_list, DGVs, theta_scenario_id,
                     parallel = TRUE,
                     niter = 2000, nburn = floor(niter/2), thin = 1, nchains = 3,
                     save_fits = FALSE,
                     save_individual_summaries_list = FALSE,
                     directory = here::here()) {

  # housekeeping
  ndatasets <- length(data_list[[1]])
  nscenarios <- length(data_list)

  # storage
  big_list <- list()

  # run scenarios
  for(scenario in 1:nscenarios){
    # progress
    message(paste0("Beginning scenario ", scenario, "."))
    message(Sys.time())

    # storage
    individual_summaries_list <- list()

    # progress
    pb <- txtProgressBar(min = 0, max = ndatasets, style = 3, width = 50, char = "=")
    for(dataset in 1:ndatasets){
      # select the dataset and zeros
      observed_df <- data_list[[scenario]][[dataset]]
      zeros <- zeros_list[[dataset]]
      # bind these together
      all_sites_and_visits <- dplyr::bind_rows(observed_df, zeros) %>%
        dplyr::arrange(.data$site, .data$visit, .data$true_spp, .data$id_spp) # was df8

      ## NIMBLE ---------

        code <- nimble::nimbleCode({

          # priors
          for(species in 1:nspecies){

            psi[species] ~ dbeta(1,1)
            lambda[species] ~ T(dnorm(0, sd = 10), 0, Inf)
            theta[species, 1:nspecies] ~ ddirch(alpha = alpha0[species, 1:nspecies])

          }

          ##  --- Likelihood --- ##

          for(i in 1:nsites){
            for(species in 1:nspecies){

              z[i, species] ~ dbern(psi[species])

            }
          }


          for(i in 1:nsites){
            for(j in 1:nvisits){

              Y.[i,j] ~ dpois(sum(z[i,1:nspecies] * lambda[1:nspecies]))

            }
          }


          ## This code chunk is the likelihood contribution from the recordings where we observed
          # both the autoID and the true species label
          for(row in 1:n_confirmed_calls){

            pi[row, 1:nspecies] <- z[site1[row], 1:nspecies] * lambda[1:nspecies] /
              sum(z[site1[row], 1:nspecies] * lambda[1:nspecies])

            k[row] ~ dcat(pi[row, 1:nspecies])
            y[row] ~ dcat(theta[k[row], 1:nspecies])

          }

          ## The next chunk is the product \prod_{l_{ij}: I_{l_{ij}} =0}, with the unobserved
          # true species labels summed out.
          for(row in 1:n_ambiguous_calls){

            pi2[row, 1:nspecies] <- z[site2[row], 1:nspecies] * lambda[1:nspecies] /
              sum(z[site2[row], 1:nspecies] * lambda[1:nspecies])
            y2[row] ~ dmarginal_autoID(theta_mat = theta[1:nspecies, 1:nspecies], 
                                       pi = pi2[row, 1:nspecies])

          }

        })


        # Define the ambiguous and unambiguous datasets
        amb <- observed_df[is.na(observed_df$true_spp), ]
        uamb <- observed_df[!is.na(observed_df$true_spp), ]


        # values passed to Nimble for indexing.
        constants <- list(
          site1 = uamb$site, # site indices for each recording that has both labels
          site2 = amb$site, # site indices for each recording with only autoID
          nspecies = dplyr::n_distinct(observed_df$id_spp),
          nvisits = dplyr::n_distinct(observed_df$visit),
          nsites = dplyr::n_distinct(all_sites_and_visits$site),
          n_confirmed_calls = nrow(uamb),
          n_ambiguous_calls = nrow(amb)
        )

        # y = observed autoID, k = observed true spp label
        # y2 = observed autoIDs from ambiguous data, k2 = unobserved true label
        # alpha0 = reference distance prior specification for classification pars
        # Y. = total number of calls observed (all spp) at each site-visit
        nimble_data <- list(
          y = uamb$id_spp,
          k = uamb$true_spp,
          y2 = amb$id_spp,
          alpha0 = matrix(1/(constants$nspecies),
                          nrow = dplyr::n_distinct(all_sites_and_visits$id_spp),
                          ncol = dplyr::n_distinct(all_sites_and_visits$id_spp)),

          # Define Y. based on all site-visits, even if it had no calls
          Y. = all_sites_and_visits %>%
            dplyr::group_by(.data$site, .data$visit) %>%
            dplyr::summarize(total = unique(.data$Y.)) %>%
            tidyr::pivot_wider(
              names_from = .data$visit,
              names_prefix = "visit",
              values_from = .data$total,
              values_fill = 0 # if NA, turn into a 0, since the NA is due to no calls being detected at that site-visit
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-'site') %>%
            as.matrix()
        )


      if(parallel){

        this_cluster <- parallel::makeCluster(nchains)
        parallel::clusterEvalQ(cl = this_cluster, library(nimble))
        fit <- parallel::parLapply(cl = this_cluster,
                         X = 1:nchains,
                         fun = runMCMC_fit,
                         code = code,
                         data = nimble_data,
                         constants = constants,
                         niter = niter,
                         nburn = nburn,
                         thin = thin
        )
        parallel::stopCluster(this_cluster)

      } else {

        fit <- runMCMC_fit(code = code,
                           data = nimble_data,
                           constants = constants,
                           nchains = nchains,
                           niter = niter,
                           nburn = nburn,
                           thin = thin,
                           seed = 1:nchains)

      }

      if (save_fits == TRUE){
        # if directory/ThetaScenarioID/fits does not exist, create it
        if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id), "fits"))) {
          dir.create(file.path(directory, paste0("Theta", theta_scenario_id), "fits"), recursive = TRUE)
        }
        
        # save the fit in standardized format
        saveRDS(
          fit,
          file=file.path(directory, paste0("Theta", theta_scenario_id),"fits", paste0("fit_", scenario, "_", dataset, ".rds"))
        )

      }


      # summarize fit
      scenario_tmp <- scenario
      dataset_tmp <- dataset
      fit_summary <- mcmc_sum(
        fit, truth = c(DGVs$lambda, DGVs$psi, DGVs$theta)
      ) %>%
        dplyr::mutate(
          theta_scenario = theta_scenario_id,
          scenario = scenario_tmp,
          dataset = dataset_tmp
        )
      individual_summaries_list[[dataset]] <- fit_summary

      if (save_individual_summaries_list == TRUE){
        
        # if directory/ThetaScenarioID/individual_summaries does not exist, create it and save individual summary there
        if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id), "individual_summaries"))) {
          dir.create(file.path(directory, paste0("Theta", theta_scenario_id), "individual_summaries"), recursive = TRUE)
        }
        
        saveRDS(
          individual_summaries_list,
          file=file.path(directory, paste0("Theta", theta_scenario_id),"individual_summaries", paste0("list_", scenario, ".rds"))
        )

      }

      # increment progress bar
      setTxtProgressBar(pb, dataset)
    } # closes the loop over datasets within a scenario
    close(pb)

    # summary df for the entire scenario after all datasets have been fit and summarized
    individual_summary_df <- do.call(eval(parse(text="dplyr::bind_rows")), individual_summaries_list)
    individual_summary_df$scenario <- scenario
    individual_summary_df$theta_scenario <- theta_scenario_id

    # make sure that the ThetaSCENARIOID folder is available for saving
    if(!dir.exists(file.path(directory, paste0("Theta", theta_scenario_id)))) {
      dir.create(file.path(directory, paste0("Theta", theta_scenario_id)), recursive = TRUE)
    }

    saveRDS(
      individual_summary_df,
      file=file.path(directory, paste0("Theta", theta_scenario_id), paste0("summary_df_for_scenario_", scenario, ".rds"))
    )

    big_list[[scenario]] <- individual_summary_df
  }

  # bind summary dfs for all scenarios into big df (all scenarios, all datasets) and return this
  out <- do.call(eval(parse(text="dplyr::bind_rows")), big_list) 
  return(out)
}
