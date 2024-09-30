# BySpeciesValidation

This repository contains supplementary files to accompany the manuscript "Investigation into stratified-by-species validation of species labels for acoustic surveys". 

**Disclaimer:**  This information is preliminary and is subject to revision. It is being provided to meet the need for timely best science.
The information is provided on the condition that neither the U.S. Geological Survey nor the U.S. Government shall be
held liable for any damages resulting from the authorized or unauthorized use of the information.

## Getting started 

To get started with this repo, follow these steps: 

1. Clone the repo from GitHub. You can do this by following these substeps:
  - On GitHub, click on the green "Code" button and copy the repo URL.
  ![repo URL](pngs/URL.png)
  - Open RStudio on your computer then in the top right corner clicking on "Project: (None)"
  !["Project: (None)""](pngs/project.png)
  - Selecting New Project > Version Control > Git. 
  - Paste the repo URL.
  - Name the directory.
  - Set the location where you'd like to set the project as a subdirectory. 
  - Click "Create Project".

2. When cloning is complete, run `runall.R`, which conducts an entire small-scale simulation study. You can expect this to take approximately 2-3 minutes.
3. Knit `vignette.Rmd` and read this document to see how the code in this repo holds together.

To familiarize yourself with the contents of the repo, see the description of contents below.


## Contents 

- I THOUGHT WE DISCUSSED PACKAGING THIS UP AS AN R PACKAGE. ARE YOU PLANNING TO DO THAT? IF SETTING THIS UP AS AN R PACKAGE, REFER TO "R PACKAGES"" BOOK [https://r-pkgs.org/](https://r-pkgs.org/) TO RESTRUCTURE CODE AND DESCRIBE STRUCTURE ACCORDINGLY. YOU'LL NEED TO ADD DOCUMENTATION TO ALL FUNCTIONS IF MAKING AN R PACKAGE, SO SOME OF THE TEXT BELOW MAY BE ABLE TO GO IN THE DOCUMENTATION FOR THE FUNCTIONS. WE CAN DISCUSS THIS ON THURSDAY...SEE ALSO COMMENTS ON CURRENT STRUCTURE.


The files in this repo are organized into three [5?] folders: __Data simulation__, __model fitting and simulation__, and __summary figures__, TESTING?, VIGNETTE --- see some notes on these below?. To replicate our simulation study [WHICH SIM STUDY IS THIS REFERRING TO? AOAS PAPER OR THE IN-PROGRESS MEE PAPER? IF THIS IS THE REPO FOR THE MEE PAPER THIS VIGNETTE SHOULD FOCUS ON THE ASSOCAIATED SIM STUDY], begin with files in the Data Simulation folder. To "test drive" the simulation study with a smaller number of species and datasets, use the testing folder, described below. 

- Data Simulation: [IS THERE ONE OF THESE FUNCTIONS THAT JUST DOES EVERYTHING? PERHAPS PRESENT IN ORDER OF USE AND NEST DEPENDENT FUNCTIONS UNDER PARENT FUNCTIONS WITH SUB-BULLETS?  (E.G., SIMULATION_SCENARIOS.R FIRST, THEN SIMULATE_BYSPPVALIDATION.R WITH DEPENDENDT FUNCTIONS NESTED BENEATH IT)]
  - `simulate_BySpeciesValidation.R`: This script defines a user-friendly wrapper function that can be used to simulate data assuming various stratified-by-species validation scenarios. Use this function to conduct your own simulations. For more information about the inputs/outputs of the function and an example of its usage, see the pdf vignette in this repo.
  - `count_detection_sim.R`: This file simulates from the count detection model as described by Stratton et al., (2022). This is used in the `simulate_BySpeciesValidation` function to to generate a user-specified number of (unmasked) datasets which are then disaggregated from counts to individual recordings. In `get_sim_datasets.R`, this function is used to generate the 50 datasets used for our example simulations in the main text. 
  - `mask_by_spp.R`: Function to carry out the masking of true species labels according to the level of effort assigned by each vetting scenario. Masking of a true species label corresponds with that file not being validated. Rows of the dataset that retain their true species label are the 'validated data' in our simulation study.
  - `simulation_scenarios.R`: A file containing the simulation scenarios that are fed to `mask_by_spp.R` to obtain the masked data used in the main text.
  - `get_sim_datasets.R`: This file replicates all datasets simulated under all validation and classifier scenarios that are described in the main text. Because specific seeds were used in the data simulation, the resulting datasets will be exactly the ones we used in our simulation study. In order to avoid errors, the working directory must have a folder named "Simulation data" within it. Data simulation requires the other files in this folder, described below. Notice that the output is a nested list. The outer layer corresponds with the vetting scenario. Within each of the 'vetting scenario' list elements, there is a list of the 50  dataframes that were output from `mask_by_spp.R` according to that vetting scenario. 
  
- Model Fitting & Simulation: 
  - `run_sims.R`: This is the main file in this folder, and it is used for fitting the simulated data obtained by running `simulate_BySpeciesValidation.R`.  Note: to save simulation results, the folders /ThetaX/fits and /ThetaX/individual_summaries are expected (X is the classifier scenario ID). This function depends on functions contained in `MCMC_sum.R` and `runMCMC_fit.R`. For an example of how to use this function, see the vignette.
  - `MCMC_sum.R`: Functions for summarizing MCMC output. 
  - `runMCMC_fit`: This file automates the NIMBLE workflow of defining inits functions, configuring and compiling models and MCMC and then running chains. Notice that by default, the number of chains is set to 1 because this is used to parallelize model fitting in `run_sims.R`. 
  
- Summary Figures:
  - `visualize_sims.R`: This script contains two wrapper functions that can visualize the simulation output from `run_sims.R`. See the pdf vignette for an example of how to use these.  
  - `Appendix_figures.R`: This file contains code to generate the figures contained in the supplementary material (all validation scenarios, all classifiers). It assumes that an object called `results` has already been read into the global environment. Assuming simulations have run correctly, this can be obtained from the output of `run_sims.r`. 
  
[TESTING DESCRIPTION IS A BIT CONFUSING, LET'S DICUSS WHAT NEEDS TO GO HERE...]

- Testing: This is an empty directory with the requisite file structure for testing [WHAT DOES THIS MEAN?]. The testing folder contains blank text files called placeHold.txt to hold the file structure. These can be ignored.  After running `runall.R` this directory will contain the full datasets, masked datasets, and site-visit-true-autoID combinations with counts of zero for each simulated dataset. 
  
- VIGNETTE: ONCE FINISHED SHOULD JUST BE THE PDF AS IT IS A STATIC DOC. SEE ALSO, R PACKAGES FOR MORE GUIDELINES/HOW TO STRUCTURE THIS WITHIN AN R PKG. 

