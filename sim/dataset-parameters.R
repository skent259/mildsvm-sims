##-----------------------------------------------------------------------------#
#' Data set parameters
#' 
#' Define parameters that are used in each simulation.  Want to avoid copying
#' them in every file. 
#' 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(mildsvm)
library(here)

out_dir <- "output"

## 1.0.0 ----------------------------------------------------------------------#

data_param <- expand_grid(
  nbag = c(250, 100, 50),
  ninst = c(3, 6),
  nsample = c(20, 50),
  ncov = 10, 
  nimp_pos = list(1:5),
  nimp_neg = list(1:5),
  positive_prob = 0.15,
  dist = list(c("mvt", 
                "mvnormal", 
                "mvnormal")),
  degree = list(c(3, NA, NA)), 
  mean = list(list(rep(0, 5), rep(0, 5), 0)),
  sd_of_mean = list(rep(0.5, 3)),
  cov = list(list(diag(1, nrow = 5), diag(1, nrow = 5), 1)),
  sample_cov = FALSE,
  replicate = 1:50
)

saveRDS(data_param, here(out_dir, "1.0", "data-param_1.0.0.rds"))

## 2.0.0 ----------------------------------------------------------------------#

data_param <- expand_grid(
  nbag = c(250, 100, 50),
  ninst = c(3, 6),
  nsample = c(20, 50),
  ncov = 10, 
  nimp_pos = list(1:2),
  nimp_neg = list(2:3),
  positive_prob = 0.15,
  dist = list(rep("mvnormal", 3)),
  mean = list(list(rep(0, 2), rep(0, 2), 0)),
  sd_of_mean = list(rep(0.5, 3)),
  cov = list(list(matrix(c(1,-0.5, -0.5, 1), nrow = 2),
                  matrix(c(1,0.5, 0.5, 1), nrow = 2),
                  1)),
  sample_cov = TRUE,
  df_wishart_cov = list(c(2, 2, 8)), 
  replicate = 1:50
)

saveRDS(data_param, here(out_dir, "2.0", "data-param_2.0.0.rds"))

## 3.0.0 ----------------------------------------------------------------------#

data_param <- expand_grid(
  nbag = c(50, 100, 250),
  ninst = c(3, 6),
  nsample = c(20, 50),
  ncov = 10, 
  nimp_pos = list(1:5),
  nimp_neg = list(1:5),
  positive_prob = 0.15,
  dist = list(rep("mvnormal", 3)),
  mean = list(list(
    rep(0.3, 5),
    rep(0, 5),
    0
  )),
  sd_of_mean = list(rep(0.5, 3)),
  cov = list(list(diag(1, nrow = 5), diag(1, nrow = 5), 1)),
  sample_cov = FALSE,
  replicate = 1:50
)

saveRDS(data_param, here(out_dir, "3.0", "data-param_3.0.0.rds"))

## 4.0.0 ----------------------------------------------------------------------#

cov_mat <- matrix(0.5, nrow = 5, ncol = 5) + diag(0.5, nrow = 5)

data_param <- expand_grid(
  nbag = c(250, 100, 50),
  ninst = c(3, 6),
  nsample = c(20, 50),
  ncov = 10, 
  nimp_pos = list(1:5),
  nimp_neg = list(6:10),
  positive_prob = 0.15,
  dist = list(rep("mvnormal", 3)),
  mean = list(list(rep(0, 5), rep(0, 5), 0)),
  sd_of_mean = list(rep(0.5, 3)),
  cov = list(list(cov_mat,
                  cov_mat,
                  1)),
  sample_cov = TRUE,
  df_wishart_cov = list(rep(5, 3)), 
  replicate = 1:50
)

saveRDS(data_param, here(out_dir, "4.0", "data-param_4.0.0.rds"))
