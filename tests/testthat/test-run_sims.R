# testing for run_sims 
K <- 2
Theta <- t(apply(diag(18, K) + 2, 1, function(x){nimble::rdirch(alpha = x)}))
sim_data <- simulate_validatedData(
  n_datasets = 2,
  design_type = "FixedPercent",
  scenarios = c(0.2, 0.4), 
  nsites = 20,
  nvisits = 3, 
  nspecies = K,
  psi = c(0.6, 0.7),
  lambda = c(1,2),
  theta = Theta 
)

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

test_that("can run a simulation study fitting in parallel", {
  expect_no_error(
    run_sims(
      data_list = sim_data$masked_dfs, 
      zeros_list = sim_data$zeros, 
      DGVs = list(
        psi = c(0.6, 0.7),
        lambda = c(1,2),
        theta = Theta
      ), 
      theta_scenario_id = "automated_test",
      niter = 1000, 
      nburn = 500, 
      thin = 1, 
      nchains = 2,
      directory = here::here("Testing")
    )
  )
})
