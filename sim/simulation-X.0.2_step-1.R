##-----------------------------------------------------------------------------#
#' Simulation X.0.2
#'   Works for 1.0.2, 2.0.2, 3.0.2, 4.0.2
#'   Step 1 - Grid-search cross-validation for optimal parameters
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
#' @argument `i` the process number when using distributed computing
#' @argument `batch_size` the number of models to run in this iteration
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
sim <- set_default(sim, "1.0.2")
i <- set_default(i, 1)
batch_size <- set_default(batch_size, 200)
output_dir <- set_default(output_dir, "output/1.0")
# 540 runs at `batch_size` = 200, for 108,000 total

## Output file ----------------------------------------------------------------#
step <- "1"
output_fname <- glue("sim-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
gs_fname <- here(output_dir, glue("gridsearch_spec_{sim}.rds"))

## Data set parameters --------------------------------------------------------#
# see sim/dataset-parameters.R
data_param <- read_data_param(sim) %>% select(-train_name, -sim)

y <- "bag_label"
bags <- "bag_name"

nbag_test <- 500

## Model parameters -----------------------------------------------------------#
n_cols <- unique(data_param$ncov)
.cost <- c(1, 10, 100)
.sigma <- (1 / n_cols) * 2 ^ c(-1, 0, 1)

default_fns <- list(
  mean = mean,
  sd = sd
)
univariate_fns <- list(
  skew = skewness,
  kurt = kurtosis,
  qtl25 = ~quantile(.x, 0.25),
  qtl75 = ~quantile(.x, 0.75)
)
.fns <- list(
  "baseline" = default_fns,
  "univariate" = c(default_fns, univariate_fns)
)

model_param <-
  bind_rows(
    # mildsvm::smm()
    expand_grid(fun = "smm_bag",
                cost = .cost,
                method = NA, 
                control = transpose(expand_grid(
                  kernel = "radial", sigma = .sigma
                )))
  )

## CV parameters --------------------------------------------------------------#
cv_param <- list(
  nrep = 1,
  nfolds = 1,
  nfolds_gs = 5,
  train = 1:nrow(data_param),
  test = 1:nrow(data_param)
)

## Set up grid-search specification -------------------------------------------#
set.seed(8)

if (file.exists(gs_fname)) {
  gridsearch_spec <- readRDS(gs_fname)
} else {
  gridsearch_spec <- define_gridsearch_specs(
    y, bags,
    cv_param, 
    method = "train-test",
    data_fun = mildsvm::generate_mild_df,
    data_param = data_param
  )
  gridsearch_spec <- shuffle_rows(gridsearch_spec)
  saveRDS(gridsearch_spec, gs_fname)
}

gs_spec_this_batch <- 
  gridsearch_spec %>% 
  select(train_name, test_name, gs_fold) %>% 
  expand_grid(model_param) %>% 
  slice(batch_index(i, batch_size)) %>%
  left_join(gridsearch_spec, by = c("train_name", "test_name", "gs_fold"))
# gs_spec_this_batch <- gs_spec_this_batch %>% group_by(fun) %>% slice_head(n = 1)


## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- gs_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x,
                          train_name = .x$train_name,
                          test_name = NULL,
                          train = .x$train,
                          test = .x$val,
                          data_fun = mildsvm::generate_mild_df,
                          data_param = data_param,
                          nbag_test = nbag_test)) %>%
  bind_cols(gs_spec_this_batch)

## Save output ----------------------------------------------------------------#
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
print(out)
out <- out %>% select(-train, -val)
saveRDS(out, output_fname)








