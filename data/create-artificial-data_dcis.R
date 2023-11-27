##-----------------------------------------------------------------------------#
#' Create artificial fiber feature data 
#' 
#' Start with processed fiber feature data and generate a simulated, artificial
#' version that contains a similar structure (and possibly similar results). The
#' main process is to use `mildsvm::generate_mild_df()` with means and
#' covariance structure from the original data, split by the bag label.
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(mildsvm)
library(mvtnorm)

proc_dir <- "data/processed"

## Pull in data ---------------------------------------------------------------#

df <- readRDS(here(proc_dir, "mild-dcis-ff.rds"))

## Pull out key information from df -------------------------------------------#

x_cols <- colnames(df)[4:ncol(df)]

dist_params <- 
  df %>% 
  nest_by(bag_label) %>% 
  mutate(
    x_mean = list(map_dbl(data[x_cols], mean)),
    x_sd_of_mean = list(
      data %>% 
        group_by(instance_name) %>% 
        select(instance_name, all_of(x_cols)) %>% 
        summarize(across(all_of(x_cols), mean)) %>%
        summarize(across(all_of(x_cols), sd)) %>% 
        as.numeric()
    ),
    x_cov = list(cov(data[x_cols])),
    x_df_wishart_cov = length(x_cols)
  )


## Create artificial data ------------------------------------------------------#

# Use anonymous version of bags, instances, and labels 

info_cols <- c("bag_label", "bag_name", "instance_name")
new_info <- df[info_cols]

new_info <- new_info %>% 
  mutate(
    bag_name = as.numeric(factor(bag_name)),
    bag_name = paste0("bag-", bag_name)
  )

new_info <- new_info %>% 
  group_by(bag_label, bag_name) %>% 
  mutate(
    instance_name = as.numeric(factor(instance_name)),
    instance_name = paste0(bag_name, "_inst-", instance_name)
  ) %>% 
  ungroup()



# Modified from `mildsvm::generate_mild_df()`
.generate_instance_samples <- function(
    nsample = 50, 
    mean, 
    sd_of_mean, 
    cov, 
    df_wishart_cov, 
    sample_cov = TRUE
) {
  
  dist_mean <- sapply(mean, stats::rnorm, n = 1, sd = sd_of_mean)
  if (sample_cov) {
    dist_cov <- stats::rWishart(1, df_wishart_cov, cov)
    dist_cov <- dist_cov[, , 1] / df_wishart_cov
  } else {
    dist_cov <- cov
  }
  
  x <- mvtnorm::rmvnorm(n = nsample,
                        mean = dist_mean,
                        sigma = dist_cov)
  
  colnames(x) <- paste0("X", 1:ncol(x))
  return(as_tibble(x))
}

# Use MVT-sampled covariates by bag label
set.seed(8)

new_df <- 
  new_info %>% 
  count(bag_label, bag_name, instance_name) %>% 
  left_join(dist_params, by = "bag_label") %>% 
  select(-data) %>% 
  rowwise() %>% 
  mutate(
    x = list(.generate_instance_samples(n, x_mean, x_sd_of_mean, x_cov, x_df_wishart_cov))
  ) %>% 
  select(bag_label, bag_name, instance_name, x) %>% 
  unnest(cols = c(x))

colnames(new_df) <- c(info_cols, x_cols)


## Post-processing, save output -----------------------------------------------#

# Scale
d <- mildsvm::as_mild_df(bind_cols(
  new_df %>% select(all_of(info_cols)),
  new_df %>% select(-all_of(info_cols)) %>%
    scale() %>%
    as.data.frame()
))

# Convert to MILD df
class(d) <- class(df)


# Save output
saveRDS(d, here(proc_dir, "ARTIFICIAL-mild-dcis-ff.rds"))

