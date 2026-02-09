# ValidationExplorer

This repository contains the `ValidationExplorer` package, as described in the manuscript "*ValidationExplorer*: Streamlined simulations to inform bioacoustic study design in the presence of misclassification," submitted to *Journal of Data Science*. 

**Disclaimer**: This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.

## Getting started 

To install `ValidationExplorer`, see the instructions in the manuscript. Installation instructions will be added to this README after peer review is complete.

Note that this package depends on [NIMBLE](https://r-nimble.org/) for model fitting. You may be prompted to install this package as well. For other packages required to use our software, see Table 1 in the manuscript.

## Contents 

The main subdirectories are:

- R: Directory containing scripts for package functions. 
- man: Documentation for package functions. 
- data: package data. Includes example output from fitting a model, summarizing simulated data and summarizing a simulation study.
- tests: A small directory containing scripts for automated tests. Current automated test coverage is 60%. Extensive manual testing has also been conducted for functions that don't allow automated tests.
- Vignette: A detailed vignette and associated files, with examples of fixed-effort and stratified-by-species validation designs. This vignette also includes greater detail on the example provided in the main manuscript. 
- Testing: Directory for manual testing of this package. Because some functions require parallelization, automated tests will fail even if the function is working correctly.

