#' Get suggested MCMC settings prior to starting your simulations
#' @param dataset A dataframe containing the validated and ambiguous data to be fit.
#'   Expected format is that of a single masked dataframe contained in the output from
#'   \link{simulate_validatedData}. We recommend using a dataset from your
#'   lowest-effort validation scenario.
#'
#' @param zeros A dataframe containing the site/visit/true_spp/id_spp combinations
#'   that were never observed (count = 0). This will be one of the elements of the
#'   zeros object ouput from \link{simulate_validatedData}.
#'
#' @export
#'
#' @returns A list containing the expected time to fit a single dataset, the
#'   minimum number of iterations, minimum warmup and a matrix with value of 1 in
#'   an entry if all model parameters had Rhat <= 1.1 after the corresponding
#'   warmup and number of iterations. See the examples for more.
#'
#' @examples
#'
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
#' # scenario 1 has the lowest effort, so use the 5th dataset from that scenario
#' # Not run during checks
#' \dontrun{
#' # note the index of the zeros matches the index of the dataset
#' tune_mcmc(dataset = fake_data$masked_dfs[[1]][[5]], zeros = fake_data$zeros[[5]])
#' }
#' @importFrom nimble getNimbleOption
tune_mcmc <- function(dataset, zeros, return_fit = FALSE) {

  max_iters <- 10000

  ## :::::::::::::::: fit MCMC with max_iters :::::::::::::::::: ##
  observed_df <- dataset
  all_sites_and_visits <- dplyr::bind_rows(observed_df, zeros) %>%
    dplyr::arrange(.data$site, .data$visit, .data$true_spp, .data$id_spp)

  ## NIMBLE --------- ##

  code <- nimble::nimbleCode({

    # priors
    for(species in 1:nspecies){

      psi[species] ~ dbeta(1,1)
      lambda[species] ~ T(dnorm(0, sd = 10), 0, Inf)
      theta[species, 1:nspecies] ~ ddirch(alpha = alpha0[species, 1:nspecies])

    }

    ##  --- Likelihood --- ##

    ## Nothing changed here
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
    site1 = uamb$site, # Only sites where at least one call was made
    site2 = amb$site,
    nspecies = dplyr::n_distinct(observed_df$id_spp),
    nvisits = dplyr::n_distinct(observed_df$visit),
    nsites = dplyr::n_distinct(all_sites_and_visits$site), # we want to have a value for all possible site, even if that val is 0
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
      dplyr::select(-.data$site) %>%
      as.matrix()
  )

  print("Fitting MCMC in parallel ... this may take a few minutes")
    this_cluster <- parallel::makeCluster(3)
    parallel::clusterEvalQ(cl = this_cluster, library(nimble))
    start <- Sys.time()
    fit <- parallel::parLapply(
      cl = this_cluster,
      X = 1:3,
      fun = runMCMC_fit,
      code = code,
      data = nimble_data,
      constants = constants,
      niter = max_iters,
      nburn = 0,
      thin = 1
    )
    end <- Sys.time()
    parallel::stopCluster(this_cluster)

    ## :::::: Check whether all R-hat values are less than 1.1 ::::: ##

    warmups <- c(500, 1000, 2000, 5000)
    iters_to_check <- 1:5*1000

    M <- apply(cbind(warmups), 1, function(x) { # for each warmup value,

      V <- apply(cbind(iters_to_check), 1, function(y) { # for each number of iterations,
        tmp1 <- lapply(fit, function(z) z[x:(x+y), ]) # look at each element in the fit (a list of 3 chains), and subset it from the warmup to the warmup+iters.
        Rhat <- vector(length = ncol(tmp1[[1]])) # Then, create a vector that is the same length as the number of columns (variables) in the first element in the list of subsets.
        for(k in 1:ncol(tmp1[[1]])) { # Then, fill in the entries of the vector one by one with the Rhat values for each variable.
          tmp2 <- sapply(tmp1, function(a) a[,k])
          Rhat[k] <- rstan::Rhat(tmp2)
        }

        return(ifelse(all(Rhat <= 1.1), 1, 0)) # If all parameters have Rhat  < 1.1 for this number of iterations given the warmup, return 1. Otherwise, return a 0.
      }) # The resulting output is a vector of length 5; for this warmup, how many additional iterations must be completed to get convergence.

      return(V)
    })

    colnames(M) <- paste0("warmup = ", as.character(warmups))
    rownames(M) <- paste0("post-warmup iters = ", as.character(iters_to_check))

    iter <- min(which(M == 1, arr.ind = TRUE)[, "row"])
    warmup_out <- warmups[min(which(M[iter, ] == 1,))]
    iter_out <- iters_to_check[iter]
    
    n_eff_df <- data.frame(
      parameter = colnames(fit[[1]]),
      ess_bulk = ess_bulk(fit),
      ess_tail = ess_tail(fit)
    )
    
    if (all(M == 0)) {
      stop(message("Convergence was not reached in under 10,000 iterations. You must run chains for longer!"))
    }
    
    if (return_fit) {
      out <- list(
        max_iter_time = end-start,
        min_warmup = warmup_out,
        min_iter = iter_out + warmup_out,
        convergence_matrix = M,
        fit = fit,
        n_eff = n_eff_df
      )
    } else {
      out <- list(max_iter_time = end-start,
                  min_warmup = warmup_out,
                  min_iter = iter_out + warmup_out,
                  convergence_matrix = M,
                  n_eff = n_eff_df)
    }
    
    return (out)

}

