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
    ggbeeswarm::geom_quasirandom(
      aes(color = method_name, group = method_name),
      groupOnX = FALSE, 
      alpha = 0.5
    ) +
    geom_vline(xintercept = 0.5, color = "grey40") +
    geom_errorbarh(stat = "summary",
                   fun.min = mean, fun.max = mean, 
                   color = "grey10") + 
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

#' Plot critical differences
#' 
#' @param rank_df A data.frame with two columns `method_name`, which gives the
#'   method and `mean.rank` which is the average rank across a number of data
#'   sets or runs.
#' @param methods A data.frame with key columns `method`, `short_name`, and
#'   `color`, where `method` matches `data$method_name`, `short_name` gives the
#'   display names, and `color` indicates which color to use for the points.
#'   
#' @return A ggplot object
my_cd_plot <- function(rank_df, methods) {
  # Set up data.frame for plot
  obj <- list(
    data = rank_df %>% 
      mutate(
        rank = rank(mean.rank),
        right = ifelse(rank < (n() / 2 + 1), 0, 1), 
        xend = ifelse(rank < (n() / 2 + 1), 0, max(rank)+1),
        yend = ifelse(rank < (n() / 2 + 1), rank - 0.5, max(rank) - rank + 0.5)
      ) %>% 
      left_join(methods, by = c("method_name" = "method")) %>% 
      rename(learner.id = short_name)
  )
  
  
  p <- ggplot(obj$data) +
    geom_segment(y = 0, x = 0, xend = max(obj$data$xend), yend = 0) +
    # geom_point(aes_string("mean.rank", 0, colour = "learner.id"), 
    #            size = 3) +
    geom_segment(aes_string("mean.rank", 0, xend = "mean.rank", 
                            yend = "yend", color = "learner.id"),
                 arrow = arrow(ends = "first", length = unit(0.1, "inches")),
                 lineend = "round", linejoin = "mitre", 
                 size = 1) +
    geom_segment(aes_string("mean.rank", "yend", xend = "xend", 
                            yend = "yend", color = "learner.id"), size = 1) +
    geom_text(aes_string("xend", "yend", label = "learner.id", 
                         hjust = "right"), color = "black", vjust = -1) +
    xlab("Average Rank") +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.4))) +
    scale_x_continuous(breaks = c(0:max(obj$data$xend))) + 
    scale_color_manual(limits = methods$short_name,
                       labels = methods$short_name,
                       values = methods$color,
                       guide = FALSE) +
    theme_minimal() + 
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks.x = element_line(), 
          legend.position = "none", 
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          # axis.line = element_line(size = 1), 
          axis.line.y = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          plot.background = element_blank())
  
  if (!is.null(obj$cd.info)) {
    cd.x = obj$cd.info$x
    cd.y = obj$cd.info$y
    cd = obj$cd.info$cd
    if (obj$cd.info$test == "bd") {
      if (!is.null(baseline)) {
        assertChoice(baseline, as.character(obj$data$learner.id))
        cd.x = obj$data$mean.rank[obj$data$learner.id == 
                                    baseline]
      }
      p = p + annotate("segment", x = cd.x + cd, xend = cd.x - 
                         cd, y = cd.y, yend = cd.y, alpha = 0.5, color = "darkgrey", 
                       size = 2)
      p = p + annotate("segment", x = cd.x + cd, xend = cd.x + 
                         cd, y = cd.y - 0.05, yend = cd.y + 0.05, color = "darkgrey", 
                       size = 1)
      p = p + annotate("segment", x = cd.x - cd, xend = cd.x - 
                         cd, y = cd.y - 0.05, yend = cd.y + 0.05, color = "darkgrey", 
                       size = 1)
      p = p + annotate("point", x = cd.x, y = cd.y, alpha = 0.5)
      p = p + annotate("text", label = stri_paste("Critical Difference =", 
                                                  round(cd, 2), sep = " "), x = cd.x, y = cd.y + 0.05)
    }
    else {
      nemenyi.data = obj$cd.info$nemenyi.data
      if (!(nrow(nemenyi.data) == 0L)) {
        p <- p + 
          geom_segment(aes_string("xstart", "y", xend = "xend", 
                                  yend = "y"), data = nemenyi.data, size = 1.5, color = "dimgrey", 
                       alpha = 0.9) +
          annotate(
            "text", 
            label = paste0("Critical Difference = ", round(cd, 2)),
            y = max(obj$data$yend) + 0.5 + 0.15, 
            x = mean(obj$data$mean.rank)
          ) +
          # annotate(
          #   "segment",
          #   x = mean(obj$data$mean.rank) - 0.5 * cd,
          #   xend = mean(obj$data$mean.rank) + 0.5 * cd,
          #   y = max(obj$data$yend) + 0.5 + 0.40,
          #   yend = max(obj$data$yend) + 0.5 + 0.40,
          #   size = 1L
          # ) +
          annotate(
            "errorbar", 
            xmin = mean(obj$data$mean.rank) - 0.5 * cd,
            xmax = mean(obj$data$mean.rank) + 0.5 * cd,
            y = max(obj$data$yend) + 0.5 + 0.40,
            # yend = max(obj$data$yend) + 0.5 + 0.40,
            width = 0.1, 
            size = 1L
          )
      }
      else {
        message("No connecting bars to plot!")
      }
    }
  }
  return(p)
}