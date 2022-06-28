##-----------------------------------------------------------------------------#
#' Generate figures 
#' 
#' Make all figures that are useful for analysis 
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(scales)
library(mildsvm)
library(knitr)
library(kableExtra)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"

kable_standard <- function(...) {
  kable(booktabs = TRUE, linesep = "", ...) %>% 
    kable_styling(full_width = FALSE)
}

##-----------------------------------------------------------------------------#
## Fiber Features --------------------------------------------------------------
##-----------------------------------------------------------------------------#

## Pull in results ------------------------------------------------------------#

sims <- c("5.0.0", "5.0.1", "5.0.2")
steps <- c(2, 3, 2)

results <- 
  map2(sims, steps, ~read_results(.x, step = .y)) %>% 
  bind_rows()

results_mod <- results %>% 
  hoist(control, "kernel") %>% 
  mutate(method_name = glue("{str_to_upper(fun)} (",
                            "{ifelse(is.na(method), '', glue('{method}'))}",
                            "{ifelse(fun == 'misvm', glue(' {kernel} '), '')}", 
                            "{ifelse(fun == 'misvm', names(.fns), '')}",
                            "{ifelse(fun == 'misvm', ifelse(cor, ' cor', ''), '')})"),
         sim = str_sub(sim, 1, 3)) %>% 
  select(method_name, sim, everything())

results_mod <-
  results_mod %>% 
  group_by(method_name, fun, method, kernel, .fns, cor, rep) %>% 
  summarize(auc = mean(auc), 
            time = sum(time) + sum(sum_time), 
            .groups = "drop_last")

methods_to_show <- tribble(
  ~method, ~short_name, ~color, 
  "MISVM (qp-heuristic radial univariate cor)", "MI-SVM (univ1, univ2, cor)", "#13317DFF",
  "MISVM (qp-heuristic radial baseline cor)", "MI-SVM (univ1, cor)", "#434C99FF",
  "MISVM (qp-heuristic radial univariate)", "MI-SVM (univ1, univ2)", "#5F66AFFF",
  "MISVM (qp-heuristic radial baseline)", "MI-SVM (univ1)", "#7180C1FF",
  "MMIL ()", "MMIL", "#C1AF95FF", 
  "SMM_BAG ()", "SMM", "#79D1D1FF",
  "SMM ()", "SI-SMM", "#79D1D1FF",
  "MILDSVM (qp-heuristic)", "MI-SMM (heuristic)", "#EB7157FF",
  "MILDSVM (mip)", "MI-SMM (MIQP)", "#F5542CFF",
)


## Plot of AUC metrics -------------------------------------------------------#

p <- create_results_plot(results_mod, methods_to_show, facets = NULL, alpha = 1) +
  labs(
    title = glue("Results of CV procedure across methods"),
    subtitle = glue("Fiber Features DCIS")
  ) +
  theme(plot.title.position = "plot")

print(p)
fname <- glue("results-plot_5.0.png")
ggsave(here(fig_dir, fname), p, width = 6, height = 6, bg = "white")


##-----------------------------------------------------------------------------#
## Simulations  ----------------------------------------------------------------
##-----------------------------------------------------------------------------#

## Pull in results ------------------------------------------------------------#
sims <- c("1.0.0", "2.0.0", "3.0.0", "4.0.0", 
          "1.0.1", "2.0.1", "3.0.1", "4.0.1",
          "1.0.2", "2.0.2", "3.0.2", "4.0.2")

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
                            "{ifelse(fun == 'misvm', ifelse(cor, ' cor', ''), '')})"),
         sim = str_sub(sim, 1, 3)) %>% 
  select(method_name, sim, everything())

methods_to_show <- tribble(
  ~method, ~short_name, ~color, 
  "MISVM (qp-heuristic radial univariate cor)", "MI-SVM (univ1, univ2, cor)", "#13317DFF",
  "MISVM (qp-heuristic radial baseline cor)", "MI-SVM (univ1, cor)", "#434C99FF",
  "MISVM (qp-heuristic radial univariate)", "MI-SVM (univ1, univ2)", "#5F66AFFF",
  "MISVM (qp-heuristic radial baseline)", "MI-SVM (univ1)", "#7180C1FF",
  # "MISVM (heuristic radial univariate cor)", "MI-SVM (univ1, univ2, cor)", "#13317DFF",
  # "MISVM (heuristic radial baseline cor)", "MI-SVM (univ1, cor)", "#434C99FF",
  # "MISVM (heuristic radial univariate)", "MI-SVM (univ1, univ2)", "#5F66AFFF",
  # "MISVM (heuristic radial baseline)", "MI-SVM (univ1)", "#7180C1FF",
  "SMM_BAG ()", "SMM", "#79D1D1FF",
  "SMM ()", "SI-SMM", "#79D1D1FF",
  "MILDSVM (qp-heuristic)", "MI-SMM (heuristic)", "#EB7157FF",
  "MILDSVM (mip)", "MI-SMM (MIQP)", "#F5542CFF",
)

sim_labels <- c(
  "1.0" = "1. Multivariate t vs multivariate normal",
  "2.0" = "2. Covariance differences",
  "3.0" = "3. Mean differences",
  "4.0" = "4. Large covariance differences"
)

## Results figure for each sim ------------------------------------------------#

for (s in unique(str_sub(sims, 1, 3))) {
  
  p <- results_mod %>% 
    filter(str_sub(sim, 1, 3) == s) %>% 
    create_results_plot(methods_to_show) +
    labs(
      title = glue("Results of CV procedure across methods"),
      subtitle = glue("{sim_labels[[s]]}")
    ) +
    theme(plot.title.position = "plot")
  
  print(p)
  fname <- glue("results-plot_{s}.png")
  ggsave(here(fig_dir, fname), p, width = 8, height = 8, bg = "white")
}



## Critical difference plot ---------------------------------------------------#

# Stratified ranks by the data set 
rank_df <- 
  results_mod %>%
  filter(method_name %in% methods_to_show$method) %>% 
  mutate(dataset = glue("sim-{sim}_{nsample}_{ninst}_{nbag}_id-{train_name}")) %>% 
  group_by(dataset) %>% 
  mutate(auc_rank = rank(-auc)) %>% 
  group_by(method_name) %>% 
  summarize(mean.rank = mean(auc_rank))

cd_plot <- my_cd_plot(rank_df, methods_to_show)

print(cd_plot)
fname <- "toy-sim_average-rank-plot.pdf"
ggsave(here(fig_dir, fname), cd_plot, width = 6, height = 3)


# Faceted rank plot by simulation 
rank_df <- 
  results_mod %>%
  filter(method_name %in% methods_to_show$method) %>% 
  mutate(dataset = glue("sim-{sim}_{nsample}_{ninst}_{nbag}_id-{train_name}")) %>% 
  group_by(dataset, sim) %>% 
  mutate(auc_rank = rank(-auc)) %>% 
  group_by(sim, method_name) %>% 
  summarize(mean.rank = mean(auc_rank))

cd_plot_facets <-
  my_cd_plot(rank_df, methods_to_show) + 
  facet_wrap(~sim, scales = "free", labeller = labeller(sim = sim_labels)) +
  theme(strip.text = element_text(size = 14))

print(cd_plot_facets)
fname <- "toy-sim_average-rank-plot_faceted.pdf"
ggsave(here(fig_dir, fname), cd_plot_facets, width = 12, height = 6)


## Performance comparison for 1 data set size ---------------------------------#

p_4scen <- results_mod %>% 
  filter(nbag == 100, ninst == 3, nsample == 50) %>% 
  create_results_plot(methods_to_show) + 
  facet_wrap(~sim, labeller = labeller(sim = sim_labels), ncol = 1) +
  labs(title = NULL)

print(p_4scen)
fname <- "toy-sim_comparison-of-4-scenarios.pdf"
ggsave(here(fig_dir, fname), p_4scen, width = 5, height = 7)
