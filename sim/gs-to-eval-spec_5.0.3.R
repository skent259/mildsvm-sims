## -----------------------------------------------------------------------------#
#' Transform the gridsearch spec to the evaluation spec
#'
#' MMIL approach doesn't require cross-validated gridsearch, so want to set up a
#' matrix that just contains the evaluation specification.
#' Simulation: 5.0.3
## -----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(rjson)

out_dir <- "output/5.0"
gridsearch_spec <- readRDS(here(out_dir, "gridsearch_spec_5.0.0.rds"))

## Summarize across rep, fold -------------------------------------------------#
eval_spec <-
    gridsearch_spec %>%
    group_by(rep, fold) %>%
    mutate(
        train = map2(train, val, ~ sort(c(.x, .y)))
    ) %>%
    select(-val, -gs_fold)

# # Check a few things
# all.equal(eval_spec$train[[1]], eval_spec$train[[2]])
# all.equal(eval_spec$test[[1]], eval_spec$test[[2]])

eval_spec <- eval_spec %>%
    slice_head(n = 1) %>%
    ungroup()

## Save output ----------------------------------------------------------------#

saveRDS(eval_spec, here(out_dir, "eval_spec_5.0.3.rds"))

# JSON for reading in Python
eval_spec_json <- toJSON(eval_spec)
write(eval_spec_json, here(out_dir, "eval_spec_5.0.3.json"))

# eval_spec_tmp <- fromJSON(file = here(out_dir, "eval_spec_5.0.0.json"))
# as_tibble(eval_spec_tmp)