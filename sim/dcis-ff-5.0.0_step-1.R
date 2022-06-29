##-----------------------------------------------------------------------------#
#' Simulation 5.0.0 - fiber features
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
batch_size <- set_default(batch_size, 10)
output_dir <- set_default(output_dir, "output/5.0")
data_dir <- set_default(data_dir, "data/processed")
# 3150 runs at `batch_size` = 10, for 31,500 total


## Output file ----------------------------------------------------------------#
sim <- "5.0.0"
step <- "1"
output_fname <- glue("sim-{sim}-{step}-results_i={str_pad(i, 4, pad = 0)}.rds")
output_fname <- here(output_dir, output_fname)
gs_fname <- here(output_dir, glue("gridsearch_spec_{sim}.rds"))

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
n_cols <- ncol(df) - 3
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
    # mildsvm::mildsvm()
    expand_grid(fun = "mildsvm", 
                cost = .cost,
                method = c("qp-heuristic", "mip"),
                control = transpose(expand_grid(
                  kernel = "radial", 
                  sigma = .sigma, 
                  nystrom_args = transpose(nesting(m = 360, r = 360)), 
                  time_limit = 600,
                  start = TRUE,
                  verbose = FALSE
                ))), 
    # mildsvm::smm()
    expand_grid(fun = "smm",
                cost = .cost,
                control = transpose(expand_grid(
                  kernel = "radial", sigma = .sigma
                ))),
    # mildsvm::misvm()
    expand_grid(fun = "misvm",
                cost = .cost,
                method = "qp-heuristic",
                .fns = .fns,
                cor = c(TRUE, FALSE), 
                control = transpose(expand_grid(
                  kernel = c("radial"),
                  sigma = .sigma
                )))
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

gs_spec_this_batch <- 
  gridsearch_spec %>% 
  select(rep, fold, gs_fold) %>% 
  expand_grid(model_param) %>% 
  slice(batch_index(i, batch_size)) %>%
  left_join(gridsearch_spec, by = c("rep", "fold", "gs_fold"))
# gs_spec_this_batch <- gs_spec_this_batch %>% group_by(fun, method, .fns, cor) %>% slice_head(n = 1)
# gs_spec_this_batch <- gs_spec_this_batch[2, ]

# gs_spec_sub <- gridsearch_spec %>% 
#   select(rep, fold, gs_fold) %>% 
#   expand_grid(model_param) %>% 
#   slice(batch_index(i, batch_size)) %>% 
#   left_join(gridsearch_spec, by = c("rep", "fold", "gs_fold"))
# gs_spec_sub <- gridsearch_spec %>% select(rep, fold, gs_fold) %>% expand_grid(model_param) %>%
#   group_by(fun, method , .fns, cor) %>% slice_head(n = 1) %>%
#   left_join(gridsearch_spec, by = c("rep", "fold", "gs_fold")) # one of each method
# gs_spec_sub <- gs_spec_sub[2, ]


## Evaluate models in current batch -------------------------------------------#
set.seed(8)
out <- gs_spec_this_batch %>% 
  transpose() %>% 
  map_dfr(~evaluate_model2(
    .x,
    df = df,
    kernel = get_kernel(.x$control$sigma),
    train = .x$train,
    test = .x$val,
  )) %>%
  bind_cols(gs_spec_this_batch)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
print(out)
out <- out %>% select(-train, -test, -val)
saveRDS(out, output_fname)

# if (!dir.exists(file_dir)) {
#   dir.create(file_dir, recursive = TRUE)
# }
# print(out)
# out <- out %>% select(-test, -train, -val) # remove large columns to preserve space
# saveRDS(out, file.path(file_dir, file_name))




