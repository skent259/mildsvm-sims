##-----------------------------------------------------------------------------#
#' Generate figures 
#' 
#' Make all figures that are used in paper
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(glue)
library(mildsvm)
library(patchwork)
source(here("analysis/utils.R"))

res_dir <- "output"
fig_dir <- "fig"

theme_set(theme_minimal())

##-----------------------------------------------------------------------------#
## Figures on motivating data set ----------------------------------------------
##-----------------------------------------------------------------------------#

data_dir <- "data/raw"
d <- readRDS(here(data_dir, "dcis-fiber-features.rds")) %>% 
  as_tibble()

# Set up MIL structure and select covariates
d <- d %>% 
  mutate(bag_label = 1*(TN == "T"),
         bag_name = paste0(pid, "-", TN),
         instance_name = paste0(pid, "-", TN, "-", sid)) %>% 
  select(bag_label, bag_name, instance_name, 
         tl, eel, w, nrba, c, 
         dn2, dn4, dn8, dn16, mnd, sdnd, 
         an2, an4, an8, an16, mna, sdna, 
         ba32, ba64, ba128) %>% 
  mildsvm::as_mild_df()

common_elements <- list(
  scale_x_log10(),
  scale_y_log10(limits = c(3, 11)),
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = NA)),
  labs(
    x = "Distance to 8th closest fiber", 
    y = "Fiber width"
  )
)

d_sm <- filter(d, bag_name %in% c("553969-T", "543849-N"))

## Data example --------------------------------------------------------------#

p_ex_a <- 
  d_sm %>% 
  ggplot(aes(x = dn8, y = w, shape = as.factor(bag_label), color = bag_name)) +
  geom_point(size = 0.5, alpha = 0.6, show.legend = FALSE) +
  geom_point(
    color = "black", 
    size = 2.5, 
    data = d_sm %>% 
            group_by(instance_name, bag_name, bag_label) %>%
            summarize(across(c(dn8, w), mean), .groups = "drop")
  ) +
  geom_point(
    data = d_sm %>%
      group_by(instance_name, bag_name, bag_label) %>%
      summarize(across(c(dn8, w), mean), .groups = "drop")
  ) +
  scale_color_brewer(name = "Subject", palette = "Set1",
                     limits = rev, labels = 1:2) +
  scale_shape(name = "Label", limits = 0:1, labels = c("Non-tumor", "Tumor")) + 
  common_elements

# p_ex_a 
d_sm2 <- d_sm %>% filter(bag_label == 1)

p_ex_b <- d_sm2 %>% 
  ggplot(aes(x = dn8, y = w, color = instance_name)) +
  geom_point(data = d_sm, size = 0) + 
  geom_point(size = 0.5, alpha = 0.6, show.legend = FALSE) +
  geom_point(
    color = "black", 
    size = 2.5, 
    shape = 17,
    data = d_sm2 %>% 
      group_by(instance_name, bag_name, bag_label) %>%
      summarize(across(c(dn8, w), mean), .groups = "drop")
  ) +
  geom_point(
    shape = 17,
    data = d_sm2 %>%
      group_by(instance_name, bag_name, bag_label) %>%
      summarize(across(c(dn8, w), mean), .groups = "drop")
  ) +
  scale_color_brewer(
    name = "Spot", palette = "Set2", 
    limits = paste0("553969-T-", c(4, 2, 3, 1, 5)),
    labels = 1:5
  ) +
  common_elements
  

p_ex <- p_ex_a / p_ex_b + plot_annotation(tag_levels = "A", tag_suffix = ".")
print(p_ex)
fname <- "intro-ff-data-example.pdf"
ggsave(here(fig_dir, fname), p_ex, width = 8, height = 7)

## Histograms of spots, fibers ------------------------------------------------#

p_spots <-
  d %>% 
  distinct(bag_name, instance_name) %>% 
  count(bag_name) %>% 
  ggplot(aes(n)) + 
  geom_bar(stat = "count", fill = "#80b1d3") +
  geom_text(aes(label = after_stat(count)), 
            color = "grey30", 
            stat = "count",
            vjust = -0.5) +
  scale_x_discrete(limits = factor(1:8)) +
  scale_y_continuous(expand = c(0, 0, 0.15, 0)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  labs(
    x = "Number of spots per tissue",
    y = "Count"
  )

p_fibers <- 
  d %>% 
  count(instance_name) %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "#8dd3c7", color = "white",
                 binwidth = 50) + 
  scale_x_continuous(n.breaks = 10) + 
  scale_y_continuous(expand = c(0, 0, 0.15, 0)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  labs(
    x = "Number of fibers per spot",
    y = "Count"
  )

p_hists <- p_spots + p_fibers + plot_annotation(tag_levels = "A", tag_suffix = ".")
print(p_hists)  
fname <- "ff-data_counts-of-spots-fibers.pdf"
ggsave(here(fig_dir, fname), p_hists, width = 10, height = 5)

##-----------------------------------------------------------------------------#
## Figures from mildsvm-sims ---------------------------------------------------
##-----------------------------------------------------------------------------#

## Pull in results ------------------------------------------------------------#

sims <- c("1.0.0", "2.0.0", "3.0.0", "4.0.0",
          "1.0.1", "2.0.1", "3.0.1", "4.0.1")

results <- 
  map(sims, ~read_results(.x, res_dir)) %>% 
  bind_rows()

data_param <- 
  map(sims, ~read_data_param(.x, res_dir)) %>% 
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
  select(method_name, everything())

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

sim_labels <- c(
  "1.0" = "1. Multivariate t vs multivariate normal",
  "2.0" = "2. Covariance differences",
  "3.0" = "3. Mean differences",
  "4.0" = "4. Large covariance differences"
)

## Figure 3 - performance comparison for 1 data set size ----------------------#

p_4scen <- results_mod %>% 
  filter(nbag == 100, ninst == 3, nsample == 50) %>% 
  create_results_plot(methods_to_show) + 
  facet_wrap(~sim, labeller = labeller(sim = sim_labels), ncol = 2) +
  labs(title = NULL)

print(p_4scen)
fname <- "mildsvm-sims_comparison-of-4-scenarios.pdf"
ggsave(here(fig_dir, fname), p_4scen, width = 8, height = 4)


## Figure 4 - Critical difference plot ----------------------------------------#

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
  theme(strip.text = element_text(size = 11))

print(cd_plot_facets)
fname <- "mildsvm-sims_average-rank-plot_faceted.pdf"
ggsave(here(fig_dir, fname), cd_plot_facets, width = 12, height = 6)

## Appendix - Results figure for each sim -------------------------------------#

for (s in unique(str_sub(sims, 1, 3))) {
  
  p <- 
    results_mod %>% 
    filter(str_sub(sim, 1, 3) == s) %>% 
    rename(`N group` = nbag,
           `N instance` = ninst,
           `N sample` = nsample) %>% 
    create_results_plot(methods_to_show,
                        facets = `N group` ~ `N instance` + `N sample`) + 
    labs(title = NULL)
    
  print(p)
  fname <- glue("mildsvm-sims-appendix_full-results-plot_{s}.pdf")
  ggsave(here(fig_dir, fname), p, width = 8, height = 4)
}

## Appendix - Training time results figure for each sim -----------------------#

for (s in unique(str_sub(sims, 1, 3))) {
  
  p <- 
    results_mod %>% 
    filter(str_sub(sim, 1, 3) == s) %>% 
    mutate(time_hr = time / 60 / 60) %>% 
    rename(`N group` = nbag,
           `N instance` = ninst,
           `N sample` = nsample) %>% 
    create_results_plot2(
      x = time_hr, 
      methods = methods_to_show,
      facets = `N group` ~ `N instance` + `N sample`
    ) + 
    labs(title = NULL, x = "Time (hr)")
  
  print(p)
  fname <- glue("mildsvm-sims-appendix_time-plot_revision_{s}.pdf")
  ggsave(here(fig_dir, fname), p, width = 8, height = 5)
}

# # For claim about range of run times
# results_mod %>% 
#   group_by(method_name, sim, nbag, ninst, nsample) %>% 
#   summarize(mean_time = mean(time / 60),
#             .groups = "drop") %>% 
#   filter(nbag == 250, ninst == 6, nsample == 50) %>% 
#   group_by(method_name) %>% 
#   summarize(range(mean_time)) %>% 
#   print(n = Inf)
#   


##-----------------------------------------------------------------------------#
## Figures from dcis-ff --------------------------------------------------------
##-----------------------------------------------------------------------------#

sims <- c("5.0.0")
steps <- c(2)
res_dir <- "output"

results <- 
  map2(sims, steps, ~read_results(.x, res_dir, step = .y)) %>% 
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

## Appendix - Training time results figure for each sim -----------------------#

p <- results_mod %>% 
  mutate(time_hr = time / 60 / 60) %>% 
  create_results_plot2(
    x = time_hr, 
    methods = methods_to_show,
    facets = NULL
  ) + 
  scale_x_continuous(labels = scales::number_format(big.mark = ",")) + 
  labs(title = NULL, x = "Time (hr)")

print(p)
fname <- glue("dcis-ff-appendix_time-plot_revision.pdf")
ggsave(here(fig_dir, fname), p, width = 8, height = 4)
