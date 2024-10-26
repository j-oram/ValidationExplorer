#' @importFrom nimble getNimbleOption
tune_mcmc <- function(dataset, zeros) {

  max_iters <- 10000

  ## :::::::::::::::: fit MCMC with max_iters :::::::::::::::::: ##
  observed_df <- dataset
  all_sites_and_visits <- dplyr::bind_rows(observed_df, zeros) %>%
    dplyr::arrange(site, visit, true_spp, id_spp)

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
      dplyr::group_by(site, visit) %>%
      dplyr::summarize(total = unique(Y.)) %>%
      tidyr::pivot_wider(
        names_from = visit,
        names_prefix = "visit",
        values_from = total,
        values_fill = 0 # if NA, turn into a 0, since the NA is due to no calls being detected at that site-visit
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-site) %>%
      as.matrix()
  )

  print("Fitting MCMC in parallel ... this may take a few minutes")
    this_cluster <- parallel::makeCluster(3)
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
    rownames(M) <- paste0("iters = ", as.character(iters_to_check))

    iter <- min(which(M == 1, arr.ind = TRUE)[, "row"])
    warmup_out <- warmups[min(which(M[iter, ] == 1,))]
    iter_out <- iters_to_check[iter]

    if(all(M == 0)) stop(message("Convergence was not reached in under 10,000 iterations. You must run chains for longer!"))

    return(list(max_iter_time = end-start,
                min_warmup = warmup_out,
                min_iter = iter_out + warmup_out,
                convergence_matrix = M))



}

