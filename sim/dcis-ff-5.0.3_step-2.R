##-----------------------------------------------------------------------------#
#' Simulation 5.0.3
#'   DCIS Fiber Features, CV evaluation
#'   Step 2 - Combine .csv files and add meta-data
#' 
#' See simulation-spreadsheet.xlsx for details 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(pROC)
source(here("sim/utils.R"))

## Command line arguments -----------------------------------------------------#
#' @argument `output_dir` the directory to save output to
#' @argument `data_dir` the directory that data is saved in 
args = commandArgs(trailingOnly = TRUE)
print(args)

output_dir <- args[1]

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if (is.na(.x)) val else .x 
}
output_dir <- set_default(output_dir, "output/5.0")

## Output file ----------------------------------------------------------------#
sim <- "5.0.3"
step <- "2"
output_fname <- glue("mildsvm-sims-results-{sim}-{step}.rds")
output_fname <- here(output_dir, output_fname)

step2_pattern <- glue("dcis-ff-{sim}-1_metrics_i=*")
# step2_pattern <- here(output_dir, step2_pattern)

## Combine files and add meta-data --------------------------------------------#

out <- 
  list.files(path = output_dir, pattern = step2_pattern, full.names = TRUE) %>% 
  map(read_csv) %>% 
  bind_rows()

out$...1 <- NULL
out$i <- NULL

out$fun <- "minet"
out$.fns <- list("univariate cor" = "univariate cor")
out$sim <- sim
out$gs_fold <- NA_integer_


## Save output ----------------------------------------------------------------#
saveRDS(out, output_fname)
