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
