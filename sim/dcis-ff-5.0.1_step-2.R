##-----------------------------------------------------------------------------#
#' Simulation 5.0.0 - fiber features
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

## Command line arguments -----------------------------------------------------#
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
#' @argument `output_dir` the directory where the output files should be written
#' @argument `data_dir` the directory that data is saved in 
args <- commandArgs(trailingOnly = TRUE)
print(args)

i <- as.integer(args[1]) + 1
batch_size <- as.integer(args[2])
output_dir <- args[3]
data_dir <- args[4]

#' Set defaults for interactive session 
set_default <- function(.x, val) {
  if (is.na(.x)) val else .x 
}
i <- set_default(i, 1)
batch_size <- set_default(batch_size, 1)
output_dir <- set_default(output_dir, "output/5.0")
data_dir <- set_default(data_dir, "data/processed")
# 1 runs at `batch_size` = 1, for 500 total

## Output file ----------------------------------------------------------------#
sim <- "5.0.0"
step <- "2"
output_fname <- glue("sim-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
step1_fname <- glue("mildsvm-sims-results-{sim}-1.rds")
step1_fname <- here(output_dir, step1_fname)
cv_fname <- here(output_dir, "gridsearch_spec_5.0.0.rds")

## Data set -------------------------------------------------------------------#

df <- readRDS(here(data_dir, "mild-dcis-ff.rds"))
y <- df[["bag_label"]]
bags <- df[["bag_name"]]
x <- df[, 4:23]

kernel_files <- list.files(data_dir, pattern = "dcis-ff-kernel-full_")
kernels <- set_names(kernel_files) %>% map(~readRDS(here(data_dir, .x)))

get_kernel <- function(sigma_, fun = "mildsvm") {
  sigma_ <- as.character(sigma_)
  nm <- glue::glue("dcis-ff-kernel-full_sigma=({sigma_}).rds")
  if (fun == "smm_bag") {
    nm <- glue::glue("dcis-ff-kernel-full_bag_sigma=({sigma_}).rds")
  }
  kernels[[nm]]
}


## Find optimal parameters from step 1 ----------------------------------------#
gridsearch_spec <- readRDS(step1_fname)
cv_spec <- readRDS(cv_fname)
model_vars <- c("rep", "fold", "fun", "method", "kernel")
gs_vars <- c("cost", "sigma")
metric_to_optimize <- "auc"

gridsearch_spec <- 
  gridsearch_spec %>% 
  hoist(control, "kernel", "sigma", .remove = FALSE) %>% 
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
eval_spec_this_batch <- slice(eval_spec, batch_index(i, batch_size)) %>%
  left_join(cv_spec, by = c("rep", "fold", "gs_fold"))

print(eval_spec)
print(eval_spec_this_batch)

## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- eval_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model2(
    .x, 
    df = df, 
    kernel = get_kernel(.x$control$sigma, .x$fun),
    train = c(.x$train, .x$val), 
    test = .x$test
  )) %>% 
  bind_cols(eval_spec_this_batch)


## Save output ----------------------------------------------------------------#
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
print(out)
saveRDS(out, output_fname)

