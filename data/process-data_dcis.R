##-----------------------------------------------------------------------------#
#' Process fiber feature data 
#' 
#' Process the data files.  Pull relevant features, transform skewed features,
#' and create several interaction terms for a high-dimensional data set 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(mildsvm)

raw_dir <- "data/raw"
proc_dir <- "data/processed"

## Pull in data ---------------------------------------------------------------#

d <-readRDS(here(raw_dir, "dcis-fiber-features.rds")) %>%
  na.omit() # remove 63 out of 882706 rows with na value for nrba

# Set up MIL structure and select covariates
d <- d %>% 
  mutate(bag_label = 1*(TN == "T"),
         bag_name = paste0(pid, "-", TN),
         instance_name = paste0(pid, "-", TN, "-", sid)) %>% 
  select(bag_label, bag_name, instance_name, 
         tl, eel, w, nrba, c, 
         dn2, dn4, dn8, dn16, mnd, sdnd, 
         an2, an4, an8, an16, mna, sdna, 
         ba32, ba64, ba128)

## Transform and scale data --------------------------------------------------- #

# Transform
d <- d %>% 
  mutate(across(c(tl, eel, dn2, dn4, dn8, dn16, mnd, sdnd), log))

# Scale
info_cols <- c("bag_label", "bag_name", "instance_name")

d <- mildsvm::as_mild_df(bind_cols(
  d %>% select(all_of(info_cols)),
  d %>% select(-all_of(info_cols)) %>% 
    scale() %>% 
    as.data.frame()
))


## Save output ----------------------------------------------------------------#
saveRDS(d, here(proc_dir, "mild-dcis-ff.rds"))
write_csv(d, here(proc_dir, "mild-dcis-ff.csv"))

# Pre-compute kernel matrix, then save
n_cols <- ncol(d) - 3
sigma_list <- (1/n_cols) * 2 ^ c(-1, 0, 1)

for (sigma in sigma_list) {
  cat("sigma:", sigma, "\n")
  print(system.time({
    kernel <- mildsvm::kme(d, sigma = sigma)
    fname <- glue("dcis-ff-kernel-full_sigma=({sigma}).rds")
    saveRDS(kernel, here(proc_dir, fname))  
  })) 
  cat("\n")
}

# Pre-compute kernel matrix at bag level, for `smm_bag`
d2 <- d
d2$bag_label <- d2$instance_name <- NULL
d2 <- d2 %>% rename(instance_name = bag_name)

for (sigma in sigma_list) {
  cat("sigma:", sigma, "\n")
  print(system.time({
    kernel <- mildsvm::kme(d2, sigma = sigma)
    fname <- glue("dcis-ff-kernel-full_bag_sigma=({sigma}).rds")
    saveRDS(kernel, here(proc_dir, fname))  
  }))
}
