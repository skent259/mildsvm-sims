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

# default values 
result_dir <- if (is.na(result_dir)) "output/3.0" else result_dir 
sim <- if (is.na(sim)) "3.0.0" else sim 
step <- if (is.na(step)) "1" else step 
output_dir <- if (is.na(output_dir)) result_dir else output_dir

## Combine files and save -----------------------------------------------------#
sim_files_pattern <- glue("sim-{sim}-{step}-results_")
outfile_name <- glue("mildsvm-sims-results-{sim}-{step}.rds")
outfile_name <- here(output_dir, outfile_name)

out <- combine_files(result_dir, sim_files_pattern)
saveRDS(out, outfile_name)

# SIMULATION_FILES_DIR = "/z/Comp/spkent/simulation/orimdb/1.0/"
# SIMULATION_FILE_PATTERN = "sim-ORIMDB-1.0.2-2a-results_"
# OUTFILE_PATH_NAME = "/z/Comp/spkent/simulation/orimdb/sim-ORIMDB-1.0.2-2a-results.rds"
