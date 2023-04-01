##-----------------------------------------------------------------------------#
#' Simulation 5.0.4 - fiber features
#'   Step 1 - Grid-search for optimal parameters and test set evaluation
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
library(milr)
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
# 100 runs at `batch_size` = 1, for 100 total

## Output file ----------------------------------------------------------------#
sim <- "5.0.4"
step <- "1"
output_fname <- glue("sim-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
gs_fname <- here(output_dir, glue("gridsearch_spec_5.0.0.rds"))

## Data set -------------------------------------------------------------------#

df <- readRDS(here(data_dir, "mild-dcis-ff.rds"))
y <- df[["bag_label"]]
bags <- df[["bag_name"]]
x <- df[, 4:23]

kernel_files <- list.files(data_dir, pattern = "dcis-ff-kernel-full_")
kernels <- set_names(kernel_files) %>% map(~readRDS(here(data_dir, .x)))

get_kernel <- function(sigma_) {
  sigma_ <- as.character(sigma_)
  nm <- glue::glue("dcis-ff-kernel-full_sigma=({sigma_}).rds")
  kernels[[nm]]
}

## Model parameters -----------------------------------------------------------#

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
  "univariate" = c(default_fns, univariate_fns)
)

model_param <-
  bind_rows(
    # milr::milr()
    expand_grid(fun = "milr",
                method = NA, 
                lambda = -1,
                lambdaCriterion = "deviance",
                nfold = 10, 
                maxit = 500,
                .fns = .fns,
                cor = TRUE)
  )


## CV parameters --------------------------------------------------------------#
cv_param <- list(
  nrep = 10,
  nfolds = 10,
  nfolds_gs = 5
)


## Set up grid-search specification -------------------------------------------#
set.seed(8)

if (file.exists(gs_fname)) {
  gridsearch_spec <- readRDS(gs_fname)
} else {
  gridsearch_spec <- define_gridsearch_specs(
    y, bags,
    cv_param, 
    method = "repeated k-fold"
  )
  saveRDS(gridsearch_spec, gs_fname)
}

# grid-search performed by milr internally, only perform evaluation
eval_spec <- gridsearch_spec %>% 
  filter(gs_fold == 1)

eval_spec_this_batch <- 
  eval_spec %>% 
  select(rep, fold, gs_fold) %>% 
  expand_grid(model_param) %>% 
  slice(batch_index(i, batch_size)) %>%
  left_join(eval_spec, by = c("rep", "fold", "gs_fold"))


## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- eval_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model2(
    .x,
    df = df,
    kernel = NULL,
    train = c(.x$train, .x$val),
    test = .x$test,
  )) %>%
  bind_cols(eval_spec_this_batch)

## Save output ----------------------------------------------------------------#

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
print(out)
out <- out %>% select(-train, -test, -val)
saveRDS(out, output_fname)


