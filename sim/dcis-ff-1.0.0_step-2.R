##----------------------------------------------------------------------------#
#' Simulation DCIS-1.0.0-2
#' 
#' See simulation-spreadsheet.xlsx for details 
#' 
##----------------------------------------------------------------------------#

local <- FALSE

# library(here)
library(tidyverse)
library(microbenchmark)
library(moments)
# devtools::install_github("skent259/mildsvm", ref = "dev-version", auth_token = "0e3e95458718a4ae9a30fafeecdcb88ab4e3da85")
library(mildsvm)
if (local) source(here::here("simulations/dcis-data/helper_functions.R")) # for local
if (!local) source("helper_functions.R")

##-----------------------------------------------------------------------------#
## Pull in command line arguments ----------------------------------------------
##-----------------------------------------------------------------------------#

if (local) {
  i <- 1
} else {
  args = commandArgs()
  print(args)
  i <- as.integer(args[3]) + 1 # arguments in condor are /path/R --vanilla $.(Process)
}

##-----------------------------------------------------------------------------#
## Define Parameters for simulation --------------------------------------------
##-----------------------------------------------------------------------------#

# OUTPUT FILE AND DIRECTORY
file_dir <- "/z/Comp/spkent/simulation/mil2/1.3"
file_name <- paste0("sim-DCIS-1.3.0-2-results_i=", stringr::str_pad(i, 4, pad = 0), ".rds")

# PREVIUOS OUTPUT FILE AND DIRECTORY
prev_file_dir <- ifelse(local,
                        here::here("simulations/dcis-data/results/1.0"),
                        "/z/Comp/spkent/simulation/mil2")
prev_file_name <- "sim-1.3.0-1-results.rds"

# DATASET
data_dir <- ifelse(local,
                   "simulations/dcis-data/data/processed/1.3",
                   "/z/Comp/spkent/simulation/mil2/data/1.3")

df <- readRDS(file.path(data_dir, "trentham_1.3.rds"))
y <- df[["bag_label"]]
bags <- df[["bag_name"]]
x <- df[, 4:23]
n_cols <- ncol(df) - 3

kernel_files <- list.files(data_dir, pattern = "trentham-kernel-full")
kernels <- set_names(kernel_files) %>% map(~readRDS(file.path(data_dir, .x)))

get_kernel <- function(sigma_) {
  sigma_ <- as.character(sigma_)
  nm <- glue::glue("trentham-kernel-full_sigma=({sigma_})_1.3.rds")
  kernels[[nm]]
}

# CV PARAMETERS
cv_param <- list(
  nrep = 10,
  nfolds = 10,
  nfolds_gs = 5
)

# MODEL PARAMETERS
.cost <- c(1, 10, 100)
.sigma <- (1/n_cols) * 2 ^ c(-1, 0, 1)

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
    # mildsvm arguments 
    expand_grid(fun = "mildsvm", 
                cost = .cost,
                method = c("heuristic", "mip"),
                control = transpose(expand_grid(
                  kernel = "radial", 
                  sigma = .sigma, 
                  nystrom_args = transpose(nesting(m = 360, r = 360)), 
                  time_limit = 600,
                  start = TRUE,
                  verbose = FALSE
                ))), 
    # smm arguments
    expand_grid(fun = "smm",
                cost = .cost,
                control = transpose(expand_grid(
                  kernel = "radial", sigma = .sigma
                ))),
    # misvm
    expand_grid(fun = "misvm",
                cost = .cost,
                method = "heuristic",
                .fns = .fns,
                cor = c(TRUE, FALSE), 
                control = transpose(expand_grid(
                  kernel = c("linear", "radial"),
                  sigma = .sigma
                )))
  )

# NUMBER OF MODELS TO RUN
batch_size <- 2

##-----------------------------------------------------------------------------#
## Execute simulation ----------------------------------------------------------
##-----------------------------------------------------------------------------#

# BUILD THE FULL GRIDSEARCH SPECIFICATION
set.seed(8)
prev_gs_spec <- readRDS(file.path(prev_file_dir, prev_file_name))
cv_spec <- readRDS(file.path(data_dir, "gridsearch_spec_1.3.0.rds"))

# find the set of parameters with best auc over the gs_folds 
model_vars <- c("rep", "fold", "fun", "method", "kernel", ".fns", "cor")
gs_vars <- c("cost", "sigma")
  
gridsearch_spec <-
  prev_gs_spec %>% 
  hoist(control, "kernel", "sigma", .remove = FALSE) %>% 
  group_by(across(all_of(c(model_vars, gs_vars)))) %>%
  mutate(mean_auc = mean(auc)) %>% 
  ungroup() %>% 
  group_by(across(all_of(model_vars))) %>%
  mutate(sum_time = sum(time)) %>% 
  slice_max(order_by = mean_auc, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-auc, -time, -mipgap, -kernel, -sigma)

# For 1.3.0, didn't pass `cor` in `misvm()`. Need to re-run when `cor = TRUE`,
# in 1.3.1, so don't bother with them here.
gridsearch_spec <- gridsearch_spec %>% 
  filter(!(fun == "misvm" & cor == TRUE))

# slice and append the indices to pull
gs_spec_sub <-
  gridsearch_spec %>% 
  slice(batch_index(i, batch_size)) %>% 
  left_join(cv_spec, by = c("rep", "fold", "gs_fold"))

# EVALUATE MODEL
out <- gs_spec_sub %>% 
  transpose() %>% 
  map_dfr(~evaluate_model(.x, df = df, kernel = get_kernel(.x$control$sigma),
                          train = c(.x$train, .x$val), test = .x$test)) %>% 
  bind_cols(gs_spec_sub)

# SAVE OUTPUT
if (!dir.exists(file_dir)) {
  dir.create(file_dir, recursive = TRUE)
}
print(out)
out <- out %>% select(-test, -train, -val) # remove large columns to preserve space
saveRDS(out, file.path(file_dir, file_name))


# check out the gridsearch parameters 
# gridsearch_spec %>% 
#   hoist(control, "sigma", "kernel") %>% 
#   mutate(name = paste(fun, method, kernel, names(.fns), cor, sep = "_")) %>% 
#   janitor::tabyl(cost, sigma, name)
