

# Fit NIMBLE model (automates workflow of defining inits function, 
# configuring/compiling models and MCMC, and running chains). 
# Note: this is designed to be fit in parallel, so by default, the number of 
# chains is set to 1. 

# code = NimbleCode object
# data = nimble_data (list)
# inits = initial values for lambda 
# constants = nimble_constants 
# alpha0 = vector of length n_spp that controls priors on classification pars
# inits = initial values for all other pars

runMCMC_fit <- function(seed = 1, code, data, lambda_init, constants,
                        nchains = 1, niter = 20000, nburn = 12000, thin = 8){
  
  library(nimble)
  library(dplyr)
  
  inits_fun <- function(){
    
    if (!is.null(lambda_init)){
      lambda <- unlist(lambda_init)
    } else {
      lambda <- abs(rnorm(constants$nspecies, sd = 10))
    }
    
    out <- list(
      psi = runif(constants$nspecies),
      lambda = lambda,
      theta = t(apply(data$alpha0, 1, function(x) rdirch(1,x))),
      z = matrix(1, nrow = constants$nsites, ncol = constants$nspecies), 
      k = data$y
    )
    
    return(out)
    
  }
  
  
  disag_model <- nimbleModel(code = code, constants = constants, data = data, inits = inits_fun())
  model_c <- compileNimble(disag_model)
  model_conf <- configureMCMC(disag_model)
  mcmc <- buildMCMC(model_conf)
  mcmc_c <- compileNimble(mcmc, project = model_c)

  
  out <- runMCMC(
    mcmc_c, 
    niter = niter, 
    nburnin = nburn,
    nchains = nchains, # Just 1 chain by default: parallelization gives multiple chains 
    thin = thin, 
    init = inits_fun(),
    setSeed = seed
  )
  
  return(out)
}
