##-----------------------------------------------------------------------------#
#' Generate figures 
#' 
#' Make all figures that are useful for analysis 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(mildsvm)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"
# sims <- c("1.0.0", "2.0.0", "3.0.0", "4.0.0")
sims <- c("3.0.0")

## Pull in results ------------------------------------------------------------#

results <- 
  map(sims, ~read_results(.x)) %>% 
  bind_rows()

data_param <- 
  map(sims, ~read_data_param(.x)) %>% 
  bind_rows()

results_mod <- results %>% 
  left_join(data_param, by = c("train_name", "sim")) %>% 
  hoist(control, "kernel") %>% 
  mutate(method_name = glue("{str_to_upper(fun)} (",
                            "{ifelse(is.na(method), '', glue('{method}'))}",
                            "{ifelse(fun == 'misvm', glue(' {kernel} '), '')}", 
                            "{ifelse(fun == 'misvm', names(.fns), '')}",
                            "{ifelse(fun == 'misvm', ifelse(cor, ' cor', ''), '')})")) %>% 
  select(method_name, everything())

methods_to_show <- tribble(
  ~method, ~short_name, ~color, 
  # "MISVM (qp-heuristic radial univariate cor)", "MI-SVM (univ1, univ2, cor)", "#13317DFF",
  # "MISVM (qp-heuristic radial baseline cor)", "MI-SVM (univ1, cor)", "#434C99FF", 
  # "MISVM (qp-heuristic radial univariate)", "MI-SVM (univ1, univ2)", "#5F66AFFF", 
  # "MISVM (qp-heuristic radial baseline)", "MI-SVM (univ1)", "#7180C1FF", 
  "MISVM (heuristic radial univariate cor)", "MI-SVM (univ1, univ2, cor)", "#13317DFF",
  "MISVM (heuristic radial baseline cor)", "MI-SVM (univ1, cor)", "#434C99FF", 
  "MISVM (heuristic radial univariate)", "MI-SVM (univ1, univ2)", "#5F66AFFF", 
  "MISVM (heuristic radial baseline)", "MI-SVM (univ1)", "#7180C1FF", 
  "SMM ()", "SI-SMM", "#79D1D1FF",
  "MILDSVM (qp-heuristic)", "MI-SMM (heuristic)", "#EB7157FF",
  "MILDSVM (mip)", "MI-SMM (MIQP)", "#F5542CFF",
)

## Results figure for each sim ------------------------------------------------#

for (s in str_sub(sims, 1, 3)) {
  
  p <- results_mod %>% 
    filter(str_sub(sim, 1, 3) == s) %>% 
    create_results_plot(methods_to_show) +
    ggtitle(glue("Results of CV procedure across methods, simulation {sim}")) +
    theme(plot.title.position = "plot")
  
  print(p)
  fname <- glue("results-plot_{s}.png")
  ggsave(here(fig_dir, fname), p, width = 8, height = 8, bg = "white")
}







