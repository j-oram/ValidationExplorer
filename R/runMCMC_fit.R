#' @importFrom nimble getNimbleOption
runMCMC_fit <- function(seed = 1, code, data, constants,
                        nchains = 1, niter = 2000, nburn = 1200, thin = 1){
  
  # create initialization function that draws starting values for each chain
  inits_fun <- function(){

    out <- list(
      psi = stats::runif(constants$nspecies),
      lambda = abs(stats::rnorm(constants$nspecies, sd = 10)),
      theta = t(apply(data$alpha0, 1, function(x) nimble::rdirch(1,x))),
      z = matrix(1, nrow = constants$nsites, ncol = constants$nspecies)
    )
    return(out)
  }
  
  # specify the custom NIMBLE function that is used to compute the probabilities
  # for each ambiguous recording
  dmarginal_autoID <- nimble::nimbleFunction(
    run = function(x = integer(0), theta_mat = double(2),
                   pi = double(1), log= integer(0, default = 0)){

      returnType(double(0))

      # select the appropriate column of the confusion matrix
      theta_kprime <- theta_mat[ , x]

      # prob= sum_k(\theta_{kk'} * (z_{ik}\lambda_k) /sum_k (z_{ik}\lambda_k))
      prob <- sum(theta_kprime * pi)

      if(log) return(log(prob))
      else return(prob)

    },
    check = getNimbleOption("checkNimbleFunction")
  )

  # placeholder function to avoid error during compiling. NIMBLE will never use
  # this function, but it is required to be specified for fitting in parallel.
  # This placeholder is taken exactly from the NIMBLE manual.
  rmarginal_autoID <- nimble::nimbleFunction(
    run = function(n = integer(0, default = 1), theta_mat = double(2), pi = double(1)) {
      returnType(double(0))
      x <- 0
      return(x)
    },
    check = nimble::getNimbleOption("checkNimbleFunction")
  )

  # Register the distribution, which will yield an informational warning because we are
  # "overwriting" the user-specified distribution that NIMBLE detects when it
  # compiles the code. 
  assign('dmarginal_autoID', dmarginal_autoID, envir = .GlobalEnv)
  assign('rmarginal_autoID', rmarginal_autoID, envir = .GlobalEnv)
  nimble::registerDistributions("dmarginal_autoID")

  # Steps to compile the NIMBLE model and MCMC
  disag_model <- nimble::nimbleModel(code = code, constants = constants, data = data, inits = inits_fun())
  model_c <- nimble::compileNimble(disag_model)
  model_conf <- nimble::configureMCMC(disag_model)
  mcmc <- nimble::buildMCMC(model_conf)
  mcmc_c <- nimble::compileNimble(mcmc, project = model_c)

  # obtain samples from the posterior 
  out <- nimble::runMCMC(
    mcmc_c,
    niter = niter,
    nburnin = nburn,
    nchains = nchains, # Just 1 chain by default: parallelization gives multiple chains
    thin = thin,
    init = inits_fun(),
    setSeed = seed
  )
  
  # return samples from the posterior
  return(out)
}
