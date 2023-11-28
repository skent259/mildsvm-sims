##-----------------------------------------------------------------------------#
#' Process fiber feature data (Artificial)
#' 
#' For use ONLY when access to the real data is not available. 
#' WARNING: will overwrite the real data if run
#' 
#' Re-name the artificial data set to the real data (not stored in GitHub), then
#' run the kernel matrix pre-computation. This computation will take several 
#' hours. 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(mildsvm)

proc_dir <- "data/processed"

## Pull in data ---------------------------------------------------------------#

# d <- readRDS(here(proc_dir, "ARTIFICIAL-mild-dcis-ff.rds")) 
d <- read_csv(here(proc_dir, "ARTIFICIAL-mild-dcis-ff.csv"))
d <- mildsvm::as_mild_df(d)

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
