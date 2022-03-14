library(glue)
library(here)

#' Combine RDS files
#' @param dir Directory that files are located in.
#' @param pattern File pattern to look for.
combine_files <- function(dir, pattern) {
    files <- list.files(path = dir, pattern = pattern, full.names = TRUE)
    res <- lapply(files, FUN = readRDS)
    res <- dplyr::bind_rows(res)
    return(res)
}

## Command line arguments -----------------------------------------------------#
args = commandArgs(trailingOnly = TRUE)
print(args)

result_dir <- args[1]
sim <- args[2]
step <- args[3]
output_dir <- args[4]

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if(is.na(.x)) val else .x 
}
result_dir <- set_default(result_dir, "output/3.0")
sim <- set_default(sim, "3.0.0")
step <- set_default(step, 1)
output_dir <- set_default(output_dir, result_dir)

## Combine files and save -----------------------------------------------------#
sim_files_pattern <- glue("sim-{sim}-{step}-results_")
outfile_name <- glue("mildsvm-sims-results-{sim}-{step}.rds")
outfile_name <- here(output_dir, outfile_name)

out <- combine_files(result_dir, sim_files_pattern)
saveRDS(out, outfile_name)
