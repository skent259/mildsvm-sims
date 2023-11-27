##-----------------------------------------------------------------------------#
#' Check MILR operationally to understand poor performance
#' 
#' Based on dcis-ff-5.0.4_step-1.R and supporting functions in sim/utils.R
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

## Data set -------------------------------------------------------------------#

data_dir <- "data/processed"

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

best_features <- read_csv(here("analysis/dcis-ff_5.0.4_milr-best-features.csv"))

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
  "baseline" = default_fns, 
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
                cor = c(TRUE, FALSE))
  )

## Set up evaluation specification --------------------------------------------#

output_dir <- "output/5.0"
gs_fname <- here(output_dir, glue("gridsearch_spec_5.0.0.rds"))

gridsearch_spec <- readRDS(gs_fname)

eval_spec <- gridsearch_spec %>% 
  filter(gs_fold == 1)

eval_spec_this_batch <- 
  eval_spec %>% 
  select(rep, fold, gs_fold) %>% 
  expand_grid(model_param) %>% 
  slice(batch_index(4, 1)) %>%
  left_join(eval_spec, by = c("rep", "fold", "gs_fold"))

# Go with one example of the (Univ1, Univ2) features 

## Evaluate model -------------------------------------------------------------#


# set.seed(8)
# out <- eval_spec_this_batch %>% 
#   transpose() %>% 
#   map_dfr(~evaluate_model2(
#     .x,
#     df = df,
#     kernel = NULL,
#     train = c(.x$train, .x$val),
#     test = .x$test,
#   )) %>%
#   bind_cols(eval_spec_this_batch)

set.seed(8)

row <- transpose(eval_spec_this_batch)[[1]]
train <- c(row$train, row$val)
test <- row$test


train_df <- df[train, , drop = FALSE]
test_df <- df[test, , drop = FALSE]

# train_inst <- inst_level(df, train)
# test_inst <- inst_level(df, test)

if (row$fun == "milr") {
  train_df <- summarize_samples(train_df, .fns = row$.fns, cor = row$cor)
  test_df <- summarize_samples(test_df, .fns = row$.fns, cor = row$cor)
}

# Scale train_df and test_df for fitting 

train_x_scaled <- scale(train_df[, 4:ncol(train_df)])
test_x_scaled <- scale(
  test_df[, 4:ncol(test_df)],
  center = attr(train_x_scaled, "scaled:center"),
  scale = attr(train_x_scaled, "scaled:scale")
)

train_df_scaled <- bind_cols(train_df[, 1:3], train_x_scaled)
test_df_scaled <- bind_cols(test_df[, 1:3], test_x_scaled)

nan_cols <- purrr::map_lgl(train_df_scaled, ~ all(is.nan(.x)))

train_df_scaled <- train_df_scaled[ !nan_cols]
test_df_scaled <- test_df_scaled[ !nan_cols]

# Check only keeping the best features
top_features <- best_features %>% 
  filter(method_name == "MILR (univariate)") %>% 
  filter(top_features != "intercept") %>% 
  pull(top_features)


fit <- milr::milr(
  y = train_df_scaled$bag_label,
  # x = train_df_scaled[, 4:ncol(train_df_scaled)],
  x = train_df_scaled[, top_features],
  bag = train_df_scaled$bag_name,
  lambda = row$lambda,
  lambdaCriterion = row$lambdaCriterion,
  nfold = row$nfold,
  maxit = row$maxit
)

summary(fit)

predict_milr <- function(object, new_data, type = "raw") {
  raw <- milr:::logit(cbind(1, new_data), coef(object))
  
  return(tibble::tibble(.pred = raw[, 1]))
}

# pred <- predict_milr(fit, new_data = as.matrix(test_df_scaled[, 4:ncol(test_df_scaled)]), type = "raw")
pred <- predict_milr(fit, new_data = as.matrix(test_df_scaled[, top_features]), type = "raw")


distinct(pred) # All the same prediction because all the coefficients are 0



## Does the data look easily predictable? -------------------------------------#

bag_label_means <- 
  train_df %>%
  # train_df_scaled %>% 
  group_by(bag_label) %>% 
  select(-bag_name, -instance_name) %>% 
  summarize(across(everything(), mean)) %>% 
  pivot_longer(
    -bag_label
  )

ggplot(bag_label_means) +
  aes(value, name, color = as.factor(bag_label)) + 
  geom_point()

tmp <-
  train_df %>% 
  select(-bag_name, -instance_name) %>% 
  mutate(across(-bag_label, ~as.vector(scale(.x)))) %>% 
  pivot_longer(
    -bag_label
  ) 

ggplot(tmp) +
  aes(value, name, color = as.factor(bag_label)) +
  geom_boxplot()


