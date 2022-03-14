#' Read in results file 
#' @param sim A string denoting the simulation number, i.e. "3.0.0".
read_results <- function(sim, results_dir = "output") {
  sim_sm <- str_sub(sim, 1, 3) 
  fname <- here(results_dir, sim_sm, glue("mildsvm-sims-results-{sim}-2.rds"))
  read_rds(fname) %>% 
    dplyr::mutate(sim = sim)
}

#' Read in data parameter file 
#' @param sim A string denoting the simulation number, i.e. "3.0.0".
read_data_param <- function(sim,  results_dir = "output") {
  sim_sm <- str_sub(sim, 1, 3)
  fname <- here(results_dir, sim_sm, glue("data-param_{sim_sm}.0.rds"))
  read_rds(fname) %>% 
    dplyr::mutate(train_name = dplyr::row_number(), sim = sim)
}


#' Plot AUC vs Method with facets
#' 
#' @param data A data.frame with key columns `auc`, `method_name`, where each row
#' corresponds to a simulation result.
#' @param methods A data.frame with key columns `method`, `short_name`, and
#'   `color`, where `method` matches `data$method_name`, `short_name` gives the
#'   display names, and `color` indicates which color to use for the points.
#' @param facets A formula to pass to `ggplot2::facet_grid()`, using variables
#'   from `data`. The default facets by three sample size measures, `ninst`,
#'   `nsample`, and `nbag`.
#'   
#' @return A ggplot object. 
create_results_plot <- function(data, methods, facets = ninst + nsample ~ nbag) {
  data %>% 
    ggplot(aes(x = auc, y = method_name, color = method_name)) +
    geom_quasirandom(aes(color = method_name, group = method_name), groupOnX = FALSE, alpha = 0.5) +
    geom_vline(xintercept = 0.5, color = "grey40") +
    geom_errorbarh(stat = "summary",fun.min = mean, fun.max = mean, color = "grey10") + 
    facet_grid(facets, 
               labeller = label_both) + 
    scale_y_discrete(limits = methods$method,
                     labels = methods$short_name) +
    scale_color_manual(limits = methods$method,
                       labels = methods$short_name,
                       values = methods$color) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = "Comparing replications of CV procedure across methods", 
         x = "AUROC",
         y = NULL)
}
