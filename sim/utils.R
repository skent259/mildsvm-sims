#' Shuffle rows of a data frame
#' @param df A data.frame object
#' @return A data.frame 
shuffle_rows <- function(df) {
  df[sample.int(nrow(df)), ]  
}

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
define_gridsearch_specs <- function(y, bags, cv_param, model_param = NULL, method = c("train-test", "repeated k-fold"), ...) {
  method = match.arg(method, c("train-test", "repeated k-fold"))
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
  } else if (method == "repeated k-fold") {
    out <- with(cv_param, 
                expand_grid(rep = 1:nrep, fold = 1:nfolds, gs_fold = 1:nfolds_gs))
    
    # split the dataset into test, train, and validation for each row of `out`
    rep_seeds <- ceiling(runif(cv_param$nrep, 0, 2^30))
    for (rep in 1:cv_param$nrep) {
      set.seed(rep_seeds[rep])
      order <- sample(1:length(y))
      
      folds <- select_cv_folds(y[order], bags[order], n_fold = cv_param$nfolds)
      
      for (fold in 1:cv_param$nfolds) {
        trainval <- folds$fold_id != fold
        test <- which(folds$fold_id == fold)
        
        gs_folds <- select_cv_folds(y[order][trainval], bags[order][trainval], n_fold = cv_param$nfolds_gs)
        
        for (gs_fold in 1:cv_param$nfolds_gs) {
          train <- which(trainval)[gs_folds$fold_id != gs_fold]
          val <- which(trainval)[gs_folds$fold_id == gs_fold]
          
          i <- which(out$rep == rep & out$fold == fold & out$gs_fold == gs_fold)
          out[[i, "test"]] <- list(order[test])
          out[[i, "train"]] <- list(order[train])
          out[[i, "val"]] <- list(order[val])
        }
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
  
  # Treat bags as instances, and ignore the instance structure.
  if (row$fun == "smm_bag") {
    train_df$instance_name <- NULL
    test_df$instance_name <- NULL
    formula <- bag_label ~ .
    class(train_df) <- class(test_df) <- "data.frame"
  }
  
  tryCatch({
    benchmark <- microbenchmark({
      
      fit <- switch(
        row$fun,
        "mildsvm" = mismm(
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
        ),
        "smm_bag" = smm(
          formula,
          train_df, 
          instances = "bag_name", 
          cost = row$cost,
          control = row$control
        )
      )

      pred <- switch(
        row$fun, 
        "mildsvm" = predict(fit, new_data = test_df, type = "raw"),
        "smm" = predict(fit, new_data = test_df, type = "raw"),
        "misvm" = predict(fit, new_data = test_df, type = "raw"),
        "smm_bag" = predict(fit, new_data = test_df, type = "raw")
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


#' Map samples to instances
#'
#' Take a sample level indicator on a data frame and translate that to an
#' instance level indicator.
#'
#' @param df A data.frame with column `instance_name` indicating the instance
#'   level variable
#' @param sample_level A vector to slice the data frame with.
#' 
#' @return A vector which can slive an instance level version of `df`.   
inst_level <- function(df, sample_level) {
  kernel_rows <- unique(df[, "instance_name"])
  rows_to_grab <- unique(df[sample_level, "instance_name"])
  
  sapply(rows_to_grab, function(i) which(i == kernel_rows))
}

#' Map samples to bags
#' 
#' Similar to `inst_level()`.
#' 
#' @inheritParams inst_level
bag_level <- function(df, sample_level) {
  kernel_rows <- unique(df[, "bag_name"])
  rows_to_grab <- unique(df[sample_level, "bag_name"])
  
  sapply(rows_to_grab, function(i) which(i == kernel_rows))
}

#' Fit and evaluate a given model
#'
#' This function is similar to the `evaluate_model()` function, except it works
#' by passing in the full data set and not generating the data automatically. It
#' also encourages a user to pass in a pre-specified kernel matrix.
#'
#' @inheritParams evaluate_model
#' @param df A data.frame to train on.
#' @param kernel A pre-computed kernel matrix that is the output of
#'   `mildsvm::kme()` called on `df`.
#' @param save A logical; if `TRUE`, will include fit and predictions in saved 
#'   output; if `FALSE`, only metrics are saved. 
#'   
#' @return A data.frame with performance metrics such as 
#' - `auc` The area under ROC based on the bag-level predictions
#' - `time` The time taken for fitting and prediction
#' - `mipgap` The reported gap from the MIP procedure in Gurobi, if applicable
evaluate_model2 <- function(row, df, kernel, train, test, verbose = TRUE, save = FALSE, features = NULL) {
  if (verbose) {
    cat("Function:", row$fun, ", ", 
        "Method:", row$method, "\n")
  }
  
  train_df <- df[train, , drop = FALSE]
  test_df <- df[test, , drop = FALSE]
  
  train_inst <- inst_level(df, train)
  test_inst <- inst_level(df, test)
  
  # Treat bags as instances, and ignore the instance structure.
  if (row$fun == "smm_bag") {
    train_df$instance_name <- NULL
    test_df$instance_name <- NULL
    formula <- bag_label ~ .
    class(train_df) <- class(test_df) <- "data.frame"
    
    train_inst = bag_level(df, train)
    test_inst = bag_level(df, train)
  }
  
  train_kernel <- kernel[train_inst, train_inst]
  test_kernel <- kernel[test_inst, train_inst]
  
  tryCatch({
    benchmark <- microbenchmark({
      
      if (
        row$fun == "smm" || 
        (row$fun == "mildsvm" && row$method %in% c("heuristic", "qp-heuristic")) || 
        (row$fun == "smm_bag")
      ) {
        row$control$kernel <- train_kernel  
      }
      
      if (row$fun == "milr") {
        train_df <- summarize_samples(train_df, .fns = row$.fns, cor = row$cor)
        test_df <- summarize_samples(test_df, .fns = row$.fns, cor = row$cor)
        
        # milr::milr() doesn't do internal scaling, so we must do scaling for it 
        train_x_scaled <- scale(train_df[, 4:ncol(train_df)])
        test_x_scaled <- scale(
          test_df[, 4:ncol(test_df)],
          center = attr(train_x_scaled, "scaled:center"),
          scale = attr(train_x_scaled, "scaled:scale")
        )
        
        train_df <- bind_cols(train_df[, 1:3], train_x_scaled)
        test_df <- bind_cols(test_df[, 1:3], test_x_scaled)
        
        # After scaling, identity columns will become NaN, so remove them
        nan_cols <- purrr::map_lgl(train_df, ~ all(is.nan(.x)))
        
        train_df <- train_df[ !nan_cols]
        test_df <- test_df[ !nan_cols]
      }
      
      if (!is.null(features)) {
        features <- union(c("bag_label", "bag_name", "instance_name"), features)
        
        train_df <- train_df[, features]
        test_df <- test_df[, features]
      }
      
      # browser()
      
      fit <- switch(
        row$fun,
        "mildsvm" = mismm(
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
        ),
        "smm_bag" = smm(
          formula,
          train_df, 
          instances = "bag_name", 
          cost = row$cost,
          control = row$control
        ),
        "milr" = milr::milr(
          y = train_df$bag_label,
          x = train_df[, 4:ncol(train_df)],
          bag = train_df$bag_name,
          lambda = row$lambda,
          lambdaCriterion = row$lambdaCriterion,
          nfold = row$nfold,
          maxit = row$maxit
        )
      )
      
      #' Custom prediction function for milr
      #' 
      #' [milr:::predict.milr] doesn't support raw predictions, but this is easy
      #' to do using their logic. 
      predict_milr <- function(object, new_data, type = "raw") {
        raw <- milr:::logit(cbind(1, new_data), coef(object))
        
        return(tibble::tibble(.pred = raw[, 1]))
      }
      
      pred <- switch(
        row$fun, 
        "mildsvm" = predict(fit, new_data = test_df, type = "raw", kernel = test_kernel),
        "smm" = predict(fit, new_data = test_df, type = "raw", kernel = test_kernel),
        "misvm" = predict(fit, new_data = test_df, type = "raw"),
        "smm_bag" = predict(fit, new_data = test_df, type = "raw", kernel = test_kernel),
        "milr" = predict_milr(fit, new_data = as.matrix(test_df[, 4:ncol(test_df)]), type = "raw")
      )

    }, times = 1)
    
    y_true_bag <- classify_bags(y[test], bags[test])
    if (row$fun == "milr") {
      y_pred_bag <- classify_bags(pred$.pred, test_df$bag_name)
    } else {
      y_pred_bag <- classify_bags(pred$.pred, bags[test])
    }
    
    out <- tibble(
      auc = as.double(pROC::auc(response = y_true_bag,
                                predictor = y_pred_bag,
                                levels = c(0,1), direction = "<")),
      auc_inst = ifelse(row$fun == "milr",
                        NA, 
                        as.double(pROC::auc(response = y[test],
                                            predictor = pred$.pred,
                                            levels = c(0,1), direction = "<"))),
      f1 = caret::F_meas(data = factor(1*(y_pred_bag > 0), levels = c(0, 1)),
                         reference = factor(y_true_bag, levels = c(0, 1))),
      time = benchmark$time / 1e9,
      mipgap = ifelse(row$method == "mip", fit$gurobi_fit$mipgap, NA)
    )
    if (save) {
      out$fit = list(fit)
      out$pred = list(pred)
    }
    
    return(out)
  },
  error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    if (save) {
      return(tibble(auc = NA, auc_inst = NA, f1 = NA, time = NA, mipgap = NA, fit = NA, pred = NA))
    } else {
      return(tibble(auc = NA, auc_inst = NA, f1 = NA, time = NA, mipgap = NA))
    }
    
  })
}
