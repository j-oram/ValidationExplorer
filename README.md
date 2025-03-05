# ValidationExplorer

This repository contains the `ValidationExplorer` package, as described in the manuscript "`ValidationExplorer`:  Streamlined simulations to provide bioacoustic study design guidance in the presence of misclassification," submitted to Methods in Ecology and Evolution Applications. 

**Disclaimer:**  This information is preliminary and is subject to revision. It is being provided to meet the need for timely best science.
The information is provided on the condition that neither the U.S. Geological Survey nor the U.S. Government shall be
held liable for any damages resulting from the authorized or unauthorized use of the information.

## Getting started 

To install `ValidationExplorer`, see the instructions in the manuscript. Installation instructions will be added to this README after double-blind peer review is complete.

Note that this package depends on [NIMBLE](https://r-nimble.org/) for model fitting. You may be prompted to install this package as well. For other packages required to use our software, see Table 1 in the manuscript.

## Contents 

The main subdirectories are:

- R: Directory containing scripts for package functions. 
- man: Documentation for package functions. 
- data: package data. Includes example output from fitting a model, summarizing simulated data and summarizing a simulation study.
- tests: A small directory containing scripts for automated tests. Current automated test coverage is 60%. Extensive manual testing has also been conducted for functions that don't allow automated tests.
- inst: includes package citation. This citation will be updated after publication.
- Vignette: A detailed vignette and associated files, with examples of fixed-effort and stratified-by-species validation designs. This vignette also includes greater detail on the example provided in the main manuscript. 
- Testing: Subdirectory used for manual testing of this package. 

