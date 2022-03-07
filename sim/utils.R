
#' Cross-validation for MIL data
#' 
#' For a MIL data set, we want to ensure that any cross validation is done at
#' the bag level.  This function passes the MIL data set info to
#' `rsample::vfold_cv()` in a way that ensures this constraint holds.
#' Stratification on the label `y` is done when possible.  The folds are
#' returned at the instance level, so that there is fold information for each
#' row of the data set.
#' 
#' @param y The bag label from a MIL data set.
#' @param bags The bag ids from a MIL data set.
#' @param n_fold The number of folds for cross-validation (default 5).  
#' 
#' @return A list with cross-validation info: `n_fold`, the number of folds
#'   used; `fold_id`: a vector indicating which fold each row belongs to.
select_cv_folds <- function(y, bags, n_fold = 5) {
  info <- data.frame(y = y, bags = bags)
  info_bag_layer <- unique(info)
    
  if (min(table(info_bag_layer$y)) < n_fold) {
    # Not enough for stratification on y
    vfold <- rsample::vfold_cv(info_bag_layer, v = n_fold)
  } else {
    vfold <- rsample::vfold_cv(info_bag_layer, v = n_fold, strata = "y")
  }
  
  ids <- imap(vfold$splits, function(.x, .y) {
    out <- setdiff(1:nrow(info_bag_layer), .x$in_id)
    names(out) <- rep(.y, length(out))
    out
  })
  ids <- sort(unlist(ids))
  info_bag_layer$bag_id <- as.numeric(names(ids))
  
  tmp <- info %>%
    dplyr::left_join(info_bag_layer, by = c("bags", "y"))
  
  fold_id <- tmp$bag_id
  return(list(n_fold = n_fold, fold_id = fold_id))
}

#' Specification of grid-search parameters
#' 
#' @param y The column name for the outcome. 
#' @param bags The column name for the bag label. 
#' @param cv_param A list of parameters that define the cross-validation,
#'   including `nfolds_gs`, `train`, and `test`.  `nfolds_gs` gives the number
#'   of cross-validation folds, while `train` and `test` denote the data set
#'   number in this case.
#' @param method The method to use for grid-search (default: 'train-test').  
#' @param ... Additional parameters for this special case, including:
#' - `data_fun` the function used to generate the data set
#' - `data_param` A data.frame where each row indicates the arguments passed to
#' `data_fun`.  
#' 
#' @return A data.frame where each row provides the information needed to perform 
#' a training and evaluation for the grid-search.  This includes columns of 
#' - `train_name` the data set number, pulled from `cv_param$train`
#' - `test_name` the data set number, pulled from `cv_param$test`
#' - `gs_fold` the fold number for each data set
#' - `train` the data set rows used for training
#' - `val` the data set rows used for validation
#' - `seed` the seed number set before data set generation and CV fold
#'   selection. (using `set.seed()` before both operations). 
define_gridsearch_specs <- function(y, bags, cv_param, model_param = NULL, method = "train-test", ...) {
  method = match.arg(method)
  dots <- list(...)
  
  if (method == "train-test") {
    out <- with(cv_param, 
                expand_grid(nesting(train_name = train, test_name = test),
                            gs_fold = 1:nfolds_gs))
    
    rep_seeds <- ceiling(runif(length(cv_param$train), 0, 2^30))
    for (ds in 1:length(cv_param$train)) {
      
      set.seed(rep_seeds[ds])
      args <- transpose(dots$data_param)[[ds]]
      train_ds <- do.call(dots$data_fun, args)
      
      y_ <- train_ds[[y]]
      bags_ <- train_ds[[bags]]
      
      set.seed(rep_seeds[ds])
      order <- sample(1:length(y_))
      gs_folds <- select_cv_folds(y_[order], bags_[order], n_fold = cv_param$nfolds_gs)
      
      for (gs_fold in 1:cv_param$nfolds_gs) {
        train <- gs_folds$fold_id != gs_fold
        val <- gs_folds$fold_id == gs_fold
        
        i <- which(out$train_name == cv_param$train[ds] & 
                     out$test_name == cv_param$test[ds] &
                     out$gs_fold == gs_fold)
        out[[i, "train"]] <- list(order[train])
        out[[i, "val"]] <- list(order[val])
        out[[i, "seed"]] <- rep_seeds[ds]
      }
    }
  }
  return(out)
}

#' Get the indices of the `i`th batch of size `batch_size`
#' 
#' @param i The index number.
#' @param batch_size The size of all batches.
#' @return A vector of indices.  
batch_index <- function(i, batch_size) {
  start <- (i-1) * batch_size + 1
  end <- i * batch_size
  return(start:end)
}

#' Fit and evaluate a given model
#' 
#' This function is designed to run both steps during a cross-validation or
#' train/test procedure depending on the arguments used.  The first option is to
#' use only a training data set that is broken up into train/validation rows
#' which are used separately.  The second option is to create a separate
#' training and testing data set following a generating function.
#' 
#' @param row A row of the grid-search specification. This should include both
#'   grid-search parameters and the model parameters.
#' @param train_name The training data set name/number.
#' @param test_name The testing data set name/number, or `NULL` to use the
#'   training data set.  If `NULL`, make sure you pass `train` and `test`
#'   arguments indicating how to split up the data.
#' @param train Indices of the data to train the model on.  Alternatively, can
#'   pass `TRUE` to use the entire training data.
#' @param test  Indices of the data to validate/test the model on.
#'   Alternatively, can pass `TRUE` to use the entire testing data.
#' @param ... Additional parameters for this special case, including:
#' - `data_fun` the function used to generate the data set
#' - `data_param` A data.frame where each row indicates the arguments passed to
#' `data_fun`. 
#' - `nbag_test` The number of bags to use in the testing data set. 
#' 
#' @return A data.frame with performance metrics such as 
#' - `auc` The area under ROC based on the bag-level predictions
#' - `time` The time taken for fitting and prediction
#' - `mipgap` The reported gap from the MIP procedure in Gurobi, if applicable
evaluate_model <- function(row, train_name, test_name, train = TRUE, test = TRUE, ...) {
  dots <- list(...)
  args <- transpose(dots$data_param)[[train_name]]
  
  set.seed(row$seed)
  train_df <- do.call(dots$data_fun, args)
  
  if (!is.null(test_name)) {
    set.seed(row$seed + 1)
    args$nbag <- dots$nbag_test
    test_df <- do.call(dots$data_fun, args)
  } else {
    test_df <- train_df
  }
  
  train_df <- train_df[train, , drop = FALSE]
  test_df <- test_df[test, , drop = FALSE]
  
  tryCatch({
    benchmark <- microbenchmark({
      
      fit <- switch(
        row$fun,
        "mildsvm" = mildsvm(
          train_df,
          cost = row$cost, 
          method = row$method,
          control = row$control
        ),
        "smm" = smm(
          train_df, 
          cost = row$cost, 
          control = row$control
        ),
        "misvm" = misvm(
          train_df, 
          .fns = row$.fns, 
          cor = row$cor,
          cost = row$cost,
          method = row$method,
          control = row$control
        )
      )

      pred <- switch(
        row$fun, 
        "mildsvm" = predict(fit, new_data = test_df, type = "raw"),
        "smm" = predict(fit, new_data = test_df, type = "raw"),
        "misvm" = predict(fit, new_data = test_df, type = "raw")
      )
      
    }, times = 1)
    
    y_true_bag <- classify_bags(test_df[[y]], test_df[[bags]])
    y_pred_bag <- classify_bags(pred$.pred, test_df[[bags]])
    
    return(tibble(
      auc = as.double(pROC::auc(response = y_true_bag,
                                predictor = y_pred_bag,
                                levels = c(0,1), direction = "<")),
      auc_inst = as.double(pROC::auc(response = test_df[[y]],
                                     predictor = pred$.pred,
                                     levels = c(0,1), direction = "<")),
      f1 = caret::F_meas(data = factor(1*(y_pred_bag > 0), levels = c(0, 1)),
                         reference = factor(y_true_bag, levels = c(0, 1))),
      time = benchmark$time / 1e9,
      mipgap = ifelse(row$method == "mip", fit$gurobi_fit$mipgap, NA)
    ))
  }, 
  error = function(e) { 
    cat("ERROR :",conditionMessage(e), "\n") 
    return(tibble(auc = NA, auc_inst = NA, f1 = NA, time = NA, mipgap = NA))
  })
}
