##-----------------------------------------------------------------------------#
#' Simulation X.0.0
#'   Works for 1.0.0, 2.0.0, 3.0.0, 4.0.0
#'   Step 2 - Test set evaluation on optimal parameters
#' 
#' See simulation-spreadsheet.xlsx for details 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(microbenchmark)
library(moments)
# devtools::install_github("skent259/mildsvm", ref = "dev-version")
library(mildsvm)
source(here("sim/utils.R"))
source(here("analysis/utils.R"))

## Command line arguments -----------------------------------------------------#
#' @argument `sim` the simulation number to run
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
#' @argument `output_dir` the directory where the output files should be written
args <- commandArgs(trailingOnly = TRUE)
print(args)

sim <- args[1]
i <- as.integer(args[2]) + 1
batch_size <- as.integer(args[3])
output_dir <- args[4]

#' Set defaults for interactive session 
set_default <- function(.x, val) {
  if (is.na(.x)) val else .x 
}
sim <- set_default(sim, "1.0.0")
i <- set_default(i, 1)
batch_size <- set_default(batch_size, 4)
output_dir <- set_default(output_dir, "output/1.0")
# 1050 runs at `batch_size` = 4, for 4,200 total

## Output file ----------------------------------------------------------------#
step <- "2"
output_fname <- glue("sim-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
step1_fname <- glue("mildsvm-sims-results-{sim}-1.rds")
step1_fname <- here(output_dir, step1_fname)


## Data set parameters --------------------------------------------------------#
# see sim/dataset-parameters.R
data_param <- read_data_param(sim) %>% select(-train_name, -sim)

y <- "bag_label"
bags <- "bag_name"

nbag_test <- 500


## Find optimal parameters from step 1 ----------------------------------------#
gridsearch_spec <- readRDS(step1_fname)
model_vars <- c("train_name", "test_name", "method", "fun", ".fns", "cor")
gs_vars <- c("cost", "sigma")
metric_to_optimize <- "auc"

gridsearch_spec <- 
  gridsearch_spec %>% 
  hoist(control, "sigma", .remove = FALSE) %>% 
  group_by(across(all_of(c(model_vars, gs_vars)))) %>% 
  mutate(mean_metric = mean(.data[[metric_to_optimize]], na.rm = TRUE)) %>% 
  ungroup()

## Set up test-set evaluation specification -----------------------------------#
eval_spec <- 
  gridsearch_spec %>% 
  group_by(across(all_of(model_vars))) %>% 
  mutate(across(time, list(sum = ~sum(.x, na.rm = TRUE)))) %>% 
  slice_max(order_by = mean_metric, n = 1, with_ties = FALSE) %>% 
  ungroup()

eval_spec <- eval_spec %>% select(-auc, -auc_inst, -f1, -time, -mipgap, -sigma)
eval_spec_this_batch <- slice(eval_spec, batch_index(i, batch_size))

print(eval_spec)
print(eval_spec_this_batch)

## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- eval_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x,
                          train_name = .x$train_name,
                          test_name = .x$test_name,
                          train = TRUE,
                          test = TRUE,
                          data_fun = mildsvm::generate_mild_df,
                          data_param = data_param,
                          nbag_test = nbag_test)) %>%
  bind_cols(eval_spec_this_batch)


## Save output ----------------------------------------------------------------#
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
print(out)
saveRDS(out, output_fname)








