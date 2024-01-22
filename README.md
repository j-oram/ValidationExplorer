# BySpeciesValidation

This repository contains supplementary files to accompany the manuscript "Investigation into stratified-by-species validation of species labels for acoustic surveys". 

**Disclaimer:**  This information is preliminary and is subject to revision. It is being provided to meet the need for timely best science.
The information is provided on the condition that neither the U.S. Geological Survey nor the U.S. Government shall be
held liable for any damages resulting from the authorized or unauthorized use of the information.

The files in this repo are organized into three folders: Data simulation, model fitting and simulation, and summary figures. To replicate our simulation study, begin with files in the Data Simulation folder. To "test drive" the simulation study with a smaller number of species and datasets, use the testing folder, described below. 

- Data Simulation:
  - `simulate_BySpeciesValidation.R`: This script defines a user-friendly wrapper function that can be used to simulate data assuming various stratified-by-species validation scenarios. Use this function to conduct your own simulations. For more information about the inputs/outputs of the function and an example of its usage, see the pdf vignette in this repo.
  - `count_detection_sim.R`: This file simulates from the count detection model as described by Stratton et al., (2022). This is used in the `simulate_BySpeciesValidation` function to to generate a user-specified number of (unmasked) datasets which are then disaggregated from counts to individual recordings. In `get_sim_datasets.R`, this function is used to generate the 50 datasets used for our example simulations in the main text. 
  - `mask_by_spp.R`: Function to carry out the masking of true species labels according to the level of effort assigned by each vetting scenario. Masking of a true species label corresponds with that file not being validated. Rows of the dataset that retain their true species label are the 'validated data' in our simulation study.
  - `simulation_scenarios.R`: A file containing the simulation scenarios that are fed to `mask_by_spp.R` to obtain the masked data used in the main text.
  - `get_sim_datasets.R`: This file replicates all datasets simulated under all validation and classifier scenarios that are described in the main text. Because specific seeds were used in the data simulation, the resulting datasets will be exactly the ones we used in our simulation study. In order to avoid errors, the working directory must have a folder named "Simulation data" within it. Data simulation requires the other files in this folder, described below. Notice that the output is a nested list. The outer layer corresponds with the vetting scenario. Within each of the 'vetting scenario' list elements, there is a list of the 50  dataframes that were output from `mask_by_spp.R` according to that vetting scenario. 
  
- Model Fitting: 
  - `run_sims.R`: This is the main file in this folder, and it is used for fitting the simulated data obtained by running `get_sim_datasets.R`. It is designed to work with the nested list output from `simulate_BySpeciesValidation`. Note: to save simulation results, the folders /ThetaX/fits and /ThetaX/individual_summaries are expected (X is the classifier scenario ID). This function depends on functions contained in `MCMC_sum.R` and `runMCMC_fit.R`. For an example of how to use this function, see the vignette.
  - `MCMC_sum.R`: Functions for summarizing MCMC output. 
  - `runMCMC_fit`: This file automates the NIMBLE workflow of defining inits functions, configuring and compiling models and MCMC and then running chains. Notice that by default, the number of chains is set to 1 because this is used to parallelize model fitting in `run_sims.R`. 
  
- Summary Figures:
  - `visualize_sims.R`: This script contains two wrapper functions that can visualize the simulation output from `run_sims.R`. See the pdf vignette for an example of how to use these.  
  - `Appendix_figures.R`: This file contains code to generate the figures contained in the supplementary material (all validation scenarios, all classifiers). It assumes that an object called `results` has already been read into the global environment. Assuming simulations have run correctly, this can be obtained from the output of `run_sims.r`. 
  
- Testing: This is a lightweight version of this repo. All files are configured to be run in their entirety in the 2 spp case. The testing folder contains the following: 
  - `get_testing_datasets.R`: Analogous to `get_sim_datasets.R` in the Data Simulation folder, but for the two-species case. To test drive this repo, run this file first. 
  - `run_sims_test.R`: This is the "testing" version of `run_sims.R`. After generating test data, run this file. Fitting all models takes 5-10 minutes.
  - `simulation` folder: empty directory required to run simulations and save fits. It contains two empty subdirectories for fits and individual summaries. 
