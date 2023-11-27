# Code and experiments for "Non-convex SVM for cancer diagnosis based on morphologic features of tumor microenvironment"

This repository contains the simulation code and output for the experiments in "Non-convex SVM for cancer diagnosis based on morphologic features of tumor microenvironment". The motivating dataset of collagen fiber features is not included for privacy reasons, however, it can be made available upon request. Besides that, this repository should allow for full reproducibility of the results. 

The experiments take considerable time to run, and so we recommend running in a high throughput manner. See the file `run.sh` for inspiration for how to do this in a batch-wise manner. 

Figures from the paper can be recreated by running `analysis/create-figures.R`. Tables can be recreated by running `analysis/create-tables.Rmd` and copying the information from the resulting .tex file. 

The main model code relies on the `mildsvm` package in R, which can be found here: https://github.com/skent259/mildsvm 


## A note on fiber features data

Because the motivating dataset of collagen fiber features is not included, an artificially simulated dataset of similar structure is included in this repository. The details for this simulation are provided in `data/create-artificial-data_dcis.R`. 

Before running the fiber feature experiment files, you must run `data/process-data_dcis-ARTIFICIAL.R` to pre-compute the kernel matrices and set up the correct file names. 
