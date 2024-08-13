# Fit NIMBLE model (automates workflow of defining inits function, 
# configuring/compiling models and MCMC, and running chains). 
# Note: this is designed to be fit in parallel, so by default, the number of 
# chains is set to 1. 
# 
## ============== Inputs ============= ## 
# seed: the seed to reproduce the starting values of the chain. This is required for 
# running this function with parLapply.
#
# code: NimbleCode object
#
# data: a list containing the data for the NIMBLE model.
#
# lambda_inits: initial values for lambda 
#
# constants: a list object containing the constants required for indexing NIMBLE code. 
#
# nchains: how many chains to run. By default, 1, so that each chain can be fit 
# on a separate core. 
# 
# niter: integer number of iterations per chain. 
# 
# nburn: integer value for the number of warmup draws per chain. 
# 
# thin: integer-valued thinning interval for each chain.
#
## ============ Outputs ============== ## 
# 
# out: Samples from the posterior distribution specified in the code input. This
# will be a list of length nchains; each entry in the list will be a matrix with 
# (niter - nburn)/thin rows and columns for each model parameter. 

runMCMC_fit <- function(seed = 1, code, data, constants,
                        nchains = 1, niter = 2000, nburn = 1200, thin = 1){
  
  library(nimble)
  library(dplyr)
  
  inits_fun <- function(){
    
    out <- list(
      psi = runif(constants$nspecies),
      lambda = abs(rnorm(constants$nspecies, sd = 10)),
      theta = t(apply(data$alpha0, 1, function(x) rdirch(1,x))),
      z = matrix(1, nrow = constants$nsites, ncol = constants$nspecies)
    )
    return(out)
  }
  
  dmarginal_autoID <- nimbleFunction(
    run = function(x = integer(0), theta_mat = double(2), 
                   pi = double(1), log= integer(0, default = 0)){
      
      returnType(double(0))
      
      # select the appropriate column of the confusion matrix 
      theta_kprime <- theta_mat[ , x]
      
      # prob= sum_k(\theta_{kk'} * (z_{ik}\lambda_k) /sum_k (z_{ik}\lambda_k))
      prob <- sum(theta_kprime * pi)
      
      if(log) return(log(prob))
      else return(prob)
      
    }
  )
  
  # placeholder function to avoid error during compiling. NIMBLE will never use 
  # this function. 
  rmarginal_autoID <- nimbleFunction(
    run = function(n = integer(0, default = 1), theta_mat = double(2), pi = double(1)) {
      returnType(double(0))
      x <- 0 
      return(x)
    }
  )
  
  assign('dmarginal_autoID', dmarginal_autoID, envir = .GlobalEnv)
  assign('rmarginal_autoID', rmarginal_autoID, envir = .GlobalEnv)
  registerDistributions("dmarginal_autoID")

  
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
