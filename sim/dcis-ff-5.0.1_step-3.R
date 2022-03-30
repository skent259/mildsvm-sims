##-----------------------------------------------------------------------------#
#' Simulation 5.0.0
#'   DCIS Fiber Features, CV evaluation
#'   Step 3 - Calculate AUC based on raw predictions 
#' 
#' See simulation-spreadsheet.xlsx for details 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(pROC)
source(here("sim/utils.R"))

## Command line arguments -----------------------------------------------------#
#' @argument `output_dir` the directory to save output to
#' @argument `data_dir` the directory that data is saved in 
args = commandArgs(trailingOnly = TRUE)
print(args)

output_dir <- args[1]

#' Set defaults for interactive session 
set_default <- function(.x, val) { 
  if (is.na(.x)) val else .x 
}
output_dir <- set_default(output_dir, "output/5.0")

## Output file ----------------------------------------------------------------#
sim <- "5.0.1"
step <- "3"
output_fname <- glue("mildsvm-sims-results-{sim}-{step}.rds")
output_fname <- here(output_dir, output_fname)
step2_fname <- glue("mildsvm-sims-preds-{sim}-2.csv")
step2_fname <- here(output_dir, step2_fname)

## Calculate metrics on the predictions ----------------------------------------#
eval_preds <- read_csv(step2_fname) %>%
  select(-...1)

print(eval_preds)

eval <- 
  eval_preds %>% 
  group_by(rep, fold) %>%
  nest(data = c(y_pred, y_true)) 

out <- eval %>%
  mutate(
    auc = map_dbl(data, ~as.double(pROC::auc(
      response = .x$y_true,
      predictor = .x$y_pred,
      levels = c(0, 1), direction = "<"
    ))),
    auc_inst = NA,
    f1 = map_dbl(data, ~caret::F_meas(
      data = factor(1*(.x$y_pred > 0.5),
                    levels = c(0, 1)),
      reference = factor(.x$y_true, levels = c(0, 1))
    )), 
    time = NA,
    mipgap = NA
  ) %>% 
  select(-data) %>% 
  ungroup()

out$fun <- "mmil"

## Save output ----------------------------------------------------------------#
saveRDS(out, output_fname)

