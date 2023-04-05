library(tidyverse)
library(here)
library(glue)
library(mildsvm)
library(patchwork)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"

theme_set(theme_minimal())

methods_to_show <- tribble(
  ~method, ~short_name, ~color,
  "MISVM (qp-heuristic radial univariate cor)", "MI-SVM (UNIV1, UNIV2, COR)", "#13317DFF",
  "MISVM (qp-heuristic radial baseline cor)", "MI-SVM (UNIV1, COR)", "#434C99FF",
  "MISVM (qp-heuristic radial univariate)", "MI-SVM (UNIV1, UNIV2)", "#5F66AFFF",
  "MISVM (qp-heuristic radial baseline)", "MI-SVM (UNIV1)", "#7180C1FF",
  "SMM ()", "SI-SMM", "#79D1D1FF",
  "MILDSVM (qp-heuristic)", "MI-SMM (HEURISTIC)", "#EB7157FF",
  "MILDSVM (mip)", "MI-SMM (MIQP)", "#F5542CFF",
)

## -----------------------------------------------------------------------------#
## Simulation experiment -------------------------------------------------------
## -----------------------------------------------------------------------------#

sims <- c(
  "1.0.0", "2.0.0", "3.0.0", "4.0.0",
  "1.0.1", "2.0.1", "3.0.1", "4.0.1"
)

results <-
  map(sims, ~ read_results(.x, res_dir)) %>%
  bind_rows()

data_param <-
  map(sims, ~ read_data_param(.x, res_dir)) %>%
  bind_rows()

results_mod <- results %>%
  left_join(data_param, by = c("train_name", "sim")) %>%
  hoist(control, "kernel") %>%
  mutate(
    method_name = glue(
      "{str_to_upper(fun)} (",
      "{ifelse(is.na(method), '', glue('{method}'))}",
      "{ifelse(fun == 'misvm', glue(' {kernel} '), '')}",
      "{ifelse(fun == 'misvm', names(.fns), '')}",
      "{ifelse(fun == 'misvm', ifelse(cor, ' cor', ''), '')})"
    ),
    sim = str_sub(sim, 1, 3)
  ) %>%
  select(method_name, everything())


sim_labels <- c(
  "1.0" = "1. Multivariate t vs multivariate normal",
  "2.0" = "2. Covariance differences",
  "3.0" = "3. Mean differences",
  "4.0" = "4. Large covariance differences"
)




create_results_plot2 <- function(data, x, methods, facets = ninst + nsample ~ nbag,
                                 alpha = 0.5) {
  data %>%
    ggplot(aes(x = {{ x }}, y = method_name, color = method_name)) +
    ggbeeswarm::geom_quasirandom(
      aes(color = method_name, group = method_name),
      groupOnX = FALSE,
      alpha = alpha
    ) +
    # geom_vline(xintercept = 0.5, color = "grey40") +
    geom_errorbarh(
      stat = "summary",
      fun.min = mean, fun.max = mean,
      color = "grey10"
    ) +
    facet_grid(facets,
      labeller = label_both
    ) +
    scale_y_discrete(
      limits = methods$method,
      labels = methods$short_name
    ) +
    scale_color_manual(
      limits = methods$method,
      labels = methods$short_name,
      values = methods$color
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(
      title = "Comparing replications of CV procedure across methods",
      y = NULL
    )
}

# How long did simulations take?
results_mod %>%
  filter(nbag == 100, ninst == 3, nsample == 50) %>%
  create_results_plot2(x = time, methods_to_show) +
  facet_wrap(~sim, labeller = labeller(sim = sim_labels), ncol = 2) +
  labs(title = NULL)

# How long did simulations take?
results_mod %>%
  filter(nbag == 100, ninst == 3, nsample == 50) %>%
  create_results_plot2(x = time + time_sum, methods_to_show) +
  facet_wrap(~sim, labeller = labeller(sim = sim_labels), ncol = 2) +
  labs(title = NULL)

# What was the MIP gap?
results_mod %>%
  rename(
    `N group` = nbag,
    `N instance` = ninst,
    `N sample` = nsample,
    `S` = sim
  ) %>%
  create_results_plot2(
    x = mipgap,
    methods = filter(methods_to_show, short_name == "MI-SMM (MIQP)"),
    facets = `S` + `N sample` ~ `N group` + `N instance`
  ) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = NULL)


# results_mod %>%
#   filter(method_name %in% methods_to_show$method) %>%
#   count(method_name)




## -----------------------------------------------------------------------------#
## DCIS data -------------------------------------------------------------------
## -----------------------------------------------------------------------------#


sims <- c("5.0.0")
steps <- c(2)
res_dir <- "output"

results <-
  map2(sims, steps, ~ read_results(.x, res_dir, step = .y)) %>%
  bind_rows()

results_mod <- results %>%
  hoist(control, "kernel") %>%
  mutate(
    method_name = glue(
      "{str_to_upper(fun)} (",
      "{ifelse(is.na(method), '', glue('{method}'))}",
      "{ifelse(fun == 'misvm', glue(' {kernel} '), '')}",
      "{ifelse(fun == 'misvm', names(.fns), '')}",
      "{ifelse(fun == 'misvm', ifelse(cor, ' cor', ''), '')})"
    ),
    sim = str_sub(sim, 1, 3)
  ) %>%
  select(method_name, sim, everything())

results_mod2 <-
  results_mod %>%
  group_by(method_name, fun, method, kernel, .fns, cor, rep) %>%
  summarize(
    auc = mean(auc),
    time = sum(time) + sum(sum_time),
    .groups = "drop_last"
  )

# Plot of optimality gap for DCIS
results_mod %>%
  create_results_plot2(
    x = mipgap,
    methods = filter(methods_to_show, short_name == "MI-SMM (MIQP)"),
    facets = NULL
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = NULL)

# SMM bag approach
read_results("5.0.2", res_dir, step = 2) %>%
  summarize(mean(auc), sd(auc))