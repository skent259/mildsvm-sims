library(tidyverse)
library(here)
library(glue)
library(mildsvm)
library(patchwork)
library(readxl)
library(scales)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"

theme_set(theme_minimal())



## Read in gurobi data --------------------------------------------------------#

log_df <- read_xlsx(here(res_dir, "5.0", "mildsvm-sims-gurobi-logs-5.1.1-2.xlsx"), skip = 1)


log_df <- log_df %>% 
  janitor::clean_names() %>% 
  mutate(
    time = as.numeric(str_remove_all(time, "s")),
    time = time / 60, # in minutes
    group = as.factor(group)
  )


## Plot convergence -----------------------------------------------------------#

# Compare to 10 minute cutoff to look for justification

p_obj <- 
  log_df %>% 
  filter(time > 5 / 60) %>% 
  ggplot(aes(time, incumbent, color = group)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    trans='log10',
    breaks=trans_breaks('log10', function(x) 10^x),
    labels=trans_format('log10', math_format(10^.x)),
    limits = c(1, NA)
  ) +
  labs(
    x = "Time (minutes)",
    y = "Best objective (log-scale)"
  )

p_bd <- 
  log_df %>% 
  filter(time > 5 / 60) %>% 
  ggplot(aes(time, best_bd, color = group)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey50") +
  labs(
    x = "Time (minutes)",
    y = "Best bound"
  )


p_gap <- 
  log_df %>% 
  ggplot(aes(time, gap, color = group)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey50") +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.2)
  ) + 
  labs(
    x = "Time (minutes)",
    y = "Optimality gap"
  )

p_log <- 
  (p_obj / p_bd / p_gap) & theme(legend.position = "none") &
  plot_annotation(
    title = "Gurobi convergence metrics for 10 replications of TMA data", 
    caption = "Models were allowed to run for up to 5 hours using the MI-SMM (MIQP) algorithm. Dashed line shows 10 minute time mark. "
  )

fname <- "revision-analysis_gurobi-logs.pdf"
ggsave(here(fig_dir, fname), p_log, width = 10, height = 8)

p_log2 <- 
  (p_obj / p_bd) & theme(legend.position = "none") &
  plot_annotation(
    title = "Gurobi convergence metrics for 10 replications of TMA data", 
    caption = "Models were allowed to run for up to 5 hours using the MI-SMM (MIQP) algorithm. Dashed line shows 10 minute time mark. "
  )

fname <- "revision-analysis_gurobi-logs_v2.pdf"
ggsave(here(fig_dir, fname), p_log2, width = 10, height = 8)



## Compare performance of these long-running models ---------------------------#

results <- read_rds(here(res_dir, "5.0", "mildsvm-sims-results-5.1.1-2.rds")) %>% 
  select(-fit, -pred) %>% 
  mutate(sim = "5.1.1")

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
  group_by(method_name, fun, method, kernel, rep) %>% 
  summarize(auc = mean(auc), 
            time = sum(time) + sum(time_sum), 
            .groups = "drop_last") %>% 
  mutate(auc_se = auc_sd / sqrt(n))

results_mod %>% 
  summarize(auc_mean = mean(auc), 
            auc_sd = sd(auc),
            n = n(), 
            .groups = "drop") %>% 
  mutate(auc_se = auc_sd / sqrt(n))

#' What the hell... now QP-heuristic is a clear winner in terms of performance...
#' 0.830 AUC vs 0.695
#' Need to ask Menggang what his thoughts are.
#' Also, maybe need to re-run the results on the new mildsvm package?\
#' 
#' Going back, I fixed a prediction bug in the following commit: https://github.com/skent259/mildsvm/pull/65/commits/970596e316fd386ef21e26ab7a02cbf8e0c701cb#diff-85c3c4e10c7f90d0452429471f2d083bedb282cd4fc0a3349b6d0b627d7edf18
#' So this will impact the MILDSVM methods...
#' 
  



results <- read_rds(here(res_dir, "5.0", "mildsvm-sims-results-5.1.0-2.rds")) %>% 
  select(-fit, -pred) %>% 
  mutate(sim = "5.1.0")

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
  group_by(method_name, fun, method, kernel, rep) %>% 
  summarize(auc = mean(auc), 
            time = sum(time) + sum(time_sum), 
            .groups = "drop_last") 

results_mod %>% 
  summarize(auc_mean = mean(auc), 
            auc_sd = sd(auc),
            n = n(), 
            .groups = "drop") %>% 
  mutate(auc_se = auc_sd / sqrt(n))
