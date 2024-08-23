library(glmnet)
library(caret)
library(pROC)



# Ridge Automate ----------------------------------------------------------

automate_ridge <- function(train_data, test_data, response_col_name) {
  # Ensure response is a factor for classification
  train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
  test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
  
  # Convert to matrix and separate response from predictors
  x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
  y_train <- train_data[[response_col_name]]
  
  x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
  y_test <- test_data[[response_col_name]]
  
  # Cross-validated Ridge regression
  fit_ridge_cv <- cv.glmnet(x = x_train, y = y_train, family = 'binomial', nfolds = 10, alpha = 0)
  
  # Final Ridge regression using the best lambda
  fit_ridge <- glmnet(x = x_train, y = y_train, family = 'binomial', lambda = fit_ridge_cv$lambda.min, alpha = 0)
  
  # Get coefficients
  fit_ridge_coef <- coef(fit_ridge)
  selected_vars_count <- sum(fit_ridge_coef != 0)
  # cat("The number of selected Variables:", selected_vars_count, "\n")
  
  # Predictions on train set
  pred_train <- predict(fit_ridge, x_train, type = "class") %>% factor
  pred_train_prob <- predict(fit_ridge, x_train, type = "response")
  
  # Predictions on test set
  pred_test <- predict(fit_ridge, x_test, type = "class") %>% factor
  pred_test_prob <- predict(fit_ridge, x_test, type = "response")
  
  # Confusion Matrix on train set
  train_confmatrix <- confusionMatrix(y_train, pred_train)
  
  # Confusion Matrix on test set
  test_confmatrix <- confusionMatrix(y_test, pred_test)
  
  # ROC Curve objects with custom class for automatic plotting
  roc_train <- roc(y_train, as.numeric(pred_train_prob))
  class(roc_train) <- c("autoROC", class(roc_train))
  
  roc_test <- roc(y_test, as.numeric(pred_test_prob))
  class(roc_test) <- c("autoROC", class(roc_test))
  
  # Save all variables in a list for easy access
  results <- list(
    fit_ridge_cv = fit_ridge_cv,
    fit_ridge = fit_ridge,
    fit_ridge_coef = fit_ridge_coef,
    selected_vars_count = selected_vars_count,
    pred_train = pred_train,
    pred_train_prob = pred_train_prob,
    pred_test = pred_test,
    pred_test_prob = pred_test_prob,
    train_confmatrix = train_confmatrix,
    test_confmatrix = test_confmatrix,
    roc_train = roc_train,
    roc_test = roc_test
  )
  
  return(results)
}

# Custom print method for autoROC class
print.autoROC <- function(x, ...) {
  plot.roc(x, 
           main = "ROC curve",
           xlab = "False Positive Rate (FPR)",
           ylab = "True Positive Rate (TPR)",
           print.auc = TRUE,
           auc.polygon = TRUE,
           print.auc.col = "#1c61b6",
           auc.polygon.col = "lightgreen",
           legacy.axes = TRUE, ...)
}


# LASSO Automate ----------------------------------------------------------

automate_lasso <- function(train_data, test_data, response_col_name) {
  # Ensure response is a factor for classification
  train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
  test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
  
  # Convert to matrix and separate response from predictors
  x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
  y_train <- train_data[[response_col_name]]
  
  x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
  y_test <- test_data[[response_col_name]]
  
  # Cross-validated LASSO regression
  fit_lasso_cv <- cv.glmnet(x = x_train, y = y_train, family = 'binomial', nfolds = 10, alpha = 1)
  
  # Final LASSO regression using the best lambda
  fit_lasso <- glmnet(x = x_train, y = y_train, family = 'binomial', lambda = fit_lasso_cv$lambda.min, alpha = 1)
  
  # Get coefficients
  fit_lasso_coef <- coef(fit_lasso)
  selected_vars_count <- sum(fit_lasso_coef != 0)
  # cat("The number of selected Variables:", selected_vars_count, "\n")
  
  # Predictions on train set
  pred_train <- predict(fit_lasso, x_train, type = "class") %>% factor
  pred_train_prob <- predict(fit_lasso, x_train, type = "response")
  
  # Predictions on test set
  pred_test <- predict(fit_lasso, x_test, type = "class") %>% factor
  pred_test_prob <- predict(fit_lasso, x_test, type = "response")
  
  # Confusion Matrix on train set
  train_confmatrix <- confusionMatrix(y_train, pred_train)
  
  # Confusion Matrix on test set
  test_confmatrix <- confusionMatrix(y_test, pred_test)
  
  # ROC Curve objects with custom class for automatic plotting
  roc_train <- roc(y_train, as.numeric(pred_train_prob))
  class(roc_train) <- c("autoROC", class(roc_train))
  
  roc_test <- roc(y_test, as.numeric(pred_test_prob))
  class(roc_test) <- c("autoROC", class(roc_test))
  
  # Save all variables in a list for easy access
  results <- list(
    fit_lasso_cv = fit_lasso_cv,
    fit_lasso = fit_lasso,
    fit_lasso_coef = fit_lasso_coef,
    selected_vars_count = selected_vars_count,
    pred_train = pred_train,
    pred_train_prob = pred_train_prob,
    pred_test = pred_test,
    pred_test_prob = pred_test_prob,
    train_confmatrix = train_confmatrix,
    test_confmatrix = test_confmatrix,
    roc_train = roc_train,
    roc_test = roc_test
  )
  
  return(results)
}



# Adaptive LASSO Automate -------------------------------------------------

automate_adaptive_lasso <- function(train_data, test_data, response_col_name, lasso_coef) {
  # Ensure response is a factor for classification
  train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
  test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
  
  # Convert to matrix and separate response from predictors
  x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
  y_train <- train_data[[response_col_name]]
  
  x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
  y_test <- test_data[[response_col_name]]
  
  # Cross-validated Adaptive LASSO regression
  fit_Alasso_cv <- cv.glmnet(
    x = x_train, 
    y = y_train, 
    family = 'binomial', 
    nfolds = 10, 
    alpha = 1,
    penalty.factor = 1 / abs(lasso_coef[-1])
  )
  
  # Final Adaptive LASSO regression using the best lambda
  fit_Alasso <- glmnet(
    x = x_train, 
    y = y_train, 
    family = 'binomial', 
    lambda = fit_Alasso_cv$lambda.min, 
    alpha = 1,
    penalty.factor = 1 / abs(lasso_coef[-1])
  )
  
  # Get coefficients
  fit_Alasso_coef <- coef(fit_Alasso)
  selected_vars_count <- sum(fit_Alasso_coef != 0)
  # cat("The number of selected Variables:", selected_vars_count, "\n")
  
  # Predictions on train set
  pred_train <- predict(fit_Alasso, x_train, type = "class") %>% factor
  pred_train_prob <- predict(fit_Alasso, x_train, type = "response")
  
  # Predictions on test set
  pred_test <- predict(fit_Alasso, x_test, type = "class") %>% factor
  pred_test_prob <- predict(fit_Alasso, x_test, type = "response")
  
  # Confusion Matrix on train set
  train_confmatrix <- confusionMatrix(y_train, pred_train)
  
  # Confusion Matrix on test set
  test_confmatrix <- confusionMatrix(y_test, pred_test)
  
  # ROC Curve objects with custom class for automatic plotting
  roc_train <- roc(y_train, as.numeric(pred_train_prob))
  class(roc_train) <- c("autoROC", class(roc_train))
  
  roc_test <- roc(y_test, as.numeric(pred_test_prob))
  class(roc_test) <- c("autoROC", class(roc_test))
  
  # Save all variables in a list for easy access
  results <- list(
    fit_Alasso_cv = fit_Alasso_cv,
    fit_Alasso = fit_Alasso,
    fit_Alasso_coef = fit_Alasso_coef,
    selected_vars_count = selected_vars_count,
    pred_train = pred_train,
    pred_train_prob = pred_train_prob,
    pred_test = pred_test,
    pred_test_prob = pred_test_prob,
    train_confmatrix = train_confmatrix,
    test_confmatrix = test_confmatrix,
    roc_train = roc_train,
    roc_test = roc_test
  )
  
  return(results)
}


# ROC=Automation ----------------------------------------------------------

# Custom print method for autoROC class
print.autoROC <- function(x, ...) {
  plot.roc(x, 
           main = "ROC curve",
           xlab = "False Positive Rate (FPR)",
           ylab = "True Positive Rate (TPR)",
           # xlim = c(-0.05, 1.05),
           print.auc = TRUE,
           auc.polygon = TRUE,
           print.auc.col = "#1c61b6",
           auc.polygon.col = "lightgreen",
           legacy.axes = TRUE, ...)
}



# Elastic Net Automate ----------------------------------------------------------
automate_elastic_net <- function(train_data, test_data, response_col_name, alpha_values = seq(0, 1, by = 0.1)) {
  # Ensure response is a factor for classification in both training and test data
  train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
  test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
  
  # Convert to matrix and separate response from predictors
  x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
  y_train <- train_data[[response_col_name]]
  
  x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
  y_test <- test_data[[response_col_name]]
  
  # Initialize objects to store the results
  best_alpha <- NULL
  best_lambda <- NULL
  best_model <- NULL
  lowest_cv_error <- Inf
  
  # Cross-validated Elastic Net regression over a range of alpha values
  for (alpha in alpha_values) {
    fit_enet_cv <- cv.glmnet(x = x_train, y = y_train, family = 'binomial', nfolds = 10, alpha = alpha)
    
    if (fit_enet_cv$cvm[fit_enet_cv$lambda == fit_enet_cv$lambda.min] < lowest_cv_error) {
      best_alpha <- alpha
      best_lambda <- fit_enet_cv$lambda.min
      best_model <- fit_enet_cv
      lowest_cv_error <- fit_enet_cv$cvm[fit_enet_cv$lambda == fit_enet_cv$lambda.min]
    }
  }
  
  # Final Elastic Net regression using the best alpha and lambda
  fit_enet <- glmnet(x = x_train, y = y_train, family = 'binomial', lambda = best_lambda, alpha = best_alpha)
  
  # Get coefficients
  fit_enet_coef <- coef(fit_enet)
  selected_vars_count <- sum(fit_enet_coef != 0)
  
  # Predictions on train set
  pred_train <- predict(fit_enet, x_train, type = "class") %>% factor(levels = levels(y_train))
  pred_train_prob <- predict(fit_enet, x_train, type = "response")
  
  # Predictions on test set
  pred_test <- predict(fit_enet, x_test, type = "class") %>% factor(levels = levels(y_test))
  pred_test_prob <- predict(fit_enet, x_test, type = "response")
  
  # Confusion Matrix on train set
  train_confmatrix <- confusionMatrix(y_train, pred_train)
  
  # Confusion Matrix on test set
  test_confmatrix <- confusionMatrix(y_test, pred_test)
  
  # ROC Curve objects with custom class for automatic plotting
  roc_train <- roc(y_train, as.numeric(pred_train_prob))
  class(roc_train) <- c("autoROC", class(roc_train))
  
  roc_test <- roc(y_test, as.numeric(pred_test_prob))
  class(roc_test) <- c("autoROC", class(roc_test))
  
  # Save all variables in a list for easy access
  results <- list(
    best_alpha = best_alpha,
    fit_enet_cv = best_model,
    fit_enet = fit_enet,
    fit_enet_coef = fit_enet_coef,
    selected_vars_count = selected_vars_count,
    pred_train = pred_train,
    pred_train_prob = pred_train_prob,
    pred_test = pred_test,
    pred_test_prob = pred_test_prob,
    train_confmatrix = train_confmatrix,
    test_confmatrix = test_confmatrix,
    roc_train = roc_train,
    roc_test = roc_test
  )
  
  return(results)
}


# CBNEW -------------------------------------------------------------------

remove_highly_correlated <- function(X, threshold = 0.98) {
  # Calculate the correlation matrix
  corr_matrix <- cor(X)
  
  # Find the pairs of variables with high correlation
  high_corr_pairs <- which(abs(corr_matrix) > threshold, arr.ind = TRUE)
  
  # Filter out duplicates and self-correlations
  high_corr_pairs <- high_corr_pairs[high_corr_pairs[, 1] < high_corr_pairs[, 2], ]
  
  # Identify variables to remove (keep the first one in each pair)
  variables_to_remove <- unique(high_corr_pairs[, 2])
  
  # Return the matrix with the remaining variables
  return(X[, -variables_to_remove])
}


# Calculate W matrix from correlation matrix
calculate_W <- function(cor_matrix) {
  W <- matrix(0, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      if (i == j) {
        W[i, j] <- 2 * sum(1 / (1 - cor_matrix[i, -i]^2))
      } else {
        W[i, j] <- -2 * cor_matrix[i, j] / (1 - cor_matrix[i, j]^2)
      }
    }
  }
  
  return(W)
}



# Define the logistic function
logistic <- function(z) {
  return(1 / (1 + exp(-z)))
}

# Define the negative log-likelihood function for logistic regression
neg_log_likelihood <- function(X, y, beta) {
  pi <- logistic(X %*% beta)
  return(-sum(y * log(pi) + (1 - y) * log(1 - pi)))
}

# Coordinate Descent for Logistic Regression with L2 Penalty
coordinate_descent <- function(X, y, lambda, W, beta_init = NULL, tol = 1e-6, max_iter = 1000) {
  p <- ncol(X)
  if (is.null(beta_init)) {
    beta <- rep(0, p)
  } else {
    beta <- beta_init
  }
  
  for (iter in 1:max_iter) {
    beta_old <- beta
    
    for (j in 1:p) {
      # Compute gradient for the j-th coordinate
      X_j <- X[, j, drop = FALSE]
      grad <- -t(X_j) %*% (y - logistic(X %*% beta)) / length(y) + 2 * lambda * W[j, j] * beta[j]
      
      # Update beta_j
      beta[j] <- beta[j] - grad / (2 * lambda * W[j, j])
    }
    
    # Check convergence
    if ((sum(abs(beta - beta_old))) < tol) {
      cat("Converged after", iter, "iterations.\n")
      break
    }
  }
  
  return(beta)
}

lambda <- 10


# Automate Ridge Regression using W
automate_ridge_regression <- function(train_data, test_data, response_col_name) {
  # Ensure response is a factor for classification
  train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
  test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
  
  # Convert to matrix and separate response from predictors
  x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
  y_train <- train_data[[response_col_name]]
  # y_train <- ifelse(y_train == 'good', 0, 1) # RUN FOR GRAVIER DATASET
  y_train <- as.numeric(y_train)
  
  x_train <- remove_highly_correlated(x_train) # REMOVING THE HIGHLY CORRELATED VARIABLES
  
  # Ensure x_test has the same variables as x_train
  x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
  x_test <- x_test[, colnames(x_train), drop = FALSE]  # Keep only columns in x_train
  
  y_test <- test_data[[response_col_name]]
  # y_test <- ifelse(y_test == 'good', 0, 1) # RUN FOR GRAVIER DATASET
  
  
  # Calculate correlation matrix and W matrix
  cor_matrix <- cor(x_train)
  W <- calculate_W(cor_matrix)
  
  
  # result <- coordinate_descent(x_train, y_train, lambda, W)
  
  y_train <- as.factor(y_train)
  y_test <- as.factor(y_test)
  
  # Fit Ridge regression model with W matrix
  fit_ridge_cv <- cv.glmnet(
    x = x_train, 
    y = y_train, 
    family = 'binomial', 
    nfolds = 10, 
    alpha = 1,
    penalty.factor = (diag(W))^(3)
  )
  
  # Final Ridge regression using the best lambda
  fit_ridge <- glmnet(
    x = x_train, 
    y = y_train, 
    family = 'binomial', 
    lambda = fit_ridge_cv$lambda.min, 
    alpha = 1,
    penalty.factor = (diag(W))^(3)
  )
  
  
  # Get coefficients
  fit_ridge_coef <- coef(fit_ridge)
  selected_vars_count <- sum(fit_ridge_coef != 0)
  
  # Predictions on train set
  pred_train <- predict(fit_ridge, x_train, type = "class") %>% factor
  pred_train_prob <- predict(fit_ridge, x_train, type = "response")
  
  # Predictions on test set
  pred_test <- predict(fit_ridge, x_test, type = "class") %>% factor
  pred_test_prob <- predict(fit_ridge, x_test, type = "response")
  
  # Confusion Matrix on train set
  train_confmatrix <- confusionMatrix(y_train, pred_train)
  
  # Confusion Matrix on test set
  test_confmatrix <- confusionMatrix(y_test, pred_test)
  
  # ROC Curve objects with custom class for automatic plotting
  roc_train <- roc(y_train, as.numeric(pred_train_prob))
  class(roc_train) <- c("autoROC", class(roc_train))
  
  roc_test <- roc(y_test, as.numeric(pred_test_prob))
  class(roc_test) <- c("autoROC", class(roc_test))
  
  # Save all variables in a list for easy access
  results <- list(
    fit_ridge_cv = fit_ridge_cv,
    fit_ridge = fit_ridge,
    fit_ridge_coef = fit_ridge_coef,
    selected_vars_count = selected_vars_count,
    pred_train = pred_train,
    pred_train_prob = pred_train_prob,
    pred_test = pred_test,
    pred_test_prob = pred_test_prob,
    train_confmatrix = train_confmatrix,
    test_confmatrix = test_confmatrix,
    roc_train = roc_train,
    roc_test = roc_test
  )
  
  return(results)
}


# CBPLR -------------------------------------------------------------------

# automate_correlation_based_lasso <- function(train_data, test_data, response_col_name, CB_coef) {
#   
#   # Ensure response is a factor for classification
#   train_data[[response_col_name]] <- factor(train_data[[response_col_name]])
#   test_data[[response_col_name]] <- factor(test_data[[response_col_name]])
#   
#   # Convert to matrix and separate response from predictors
#   x_train <- as.matrix(train_data[, -which(names(train_data) == response_col_name)])
#   y_train <- train_data[[response_col_name]]
#   
#   x_test <- as.matrix(test_data[, -which(names(test_data) == response_col_name)])
#   y_test <- test_data[[response_col_name]]
#   
#   # Cross-validated Correlation-Based Adaptive LASSO regression
#   fit_CB_cv <- cv.glmnet(
#     x = x_train, 
#     y = y_train, 
#     family = 'binomial', 
#     nfolds = 10, 
#     alpha = 1,
#     penalty.factor = 1 / abs(CB_coef[-1])
#   )
#   
#   # Final Correlation-Based Adaptive LASSO regression using the best lambda
#   fit_CB <- glmnet(
#     x = x_train, 
#     y = y_train, 
#     family = 'binomial', 
#     lambda = fit_CB_cv$lambda.min, 
#     alpha = 1,
#     penalty.factor = 1 / abs(CB_coef[-1])
#   )
#   
#   # Get coefficients
#   fit_CB_coef <- coef(fit_CB)
#   selected_vars_count <- sum(fit_CB_coef != 0)
#   # cat("The number of selected Variables:", selected_vars_count, "\n")
#   
#   # Predictions on train set
#   pred_train <- predict(fit_CB, x_train, type = "class") %>% factor
#   pred_train_prob <- predict(fit_CB, x_train, type = "response")
#   
#   # Predictions on test set
#   pred_test <- predict(fit_CB, x_test, type = "class") %>% factor
#   pred_test_prob <- predict(fit_CB, x_test, type = "response")
#   
#   # Confusion Matrix on train set
#   train_confmatrix <- confusionMatrix(y_train, pred_train)
#   
#   # Confusion Matrix on test set
#   test_confmatrix <- confusionMatrix(y_test, pred_test)
#   
#   # ROC Curve objects with custom class for automatic plotting
#   roc_train <- roc(y_train, as.numeric(pred_train_prob))
#   class(roc_train) <- c("autoROC", class(roc_train))
#   
#   roc_test <- roc(y_test, as.numeric(pred_test_prob))
#   class(roc_test) <- c("autoROC", class(roc_test))
#   
#   # Save all variables in a list for easy access
#   results <- list(
#     fit_CB_cv = fit_CB_cv,
#     fit_CB = fit_CB,
#     fit_CB_coef = fit_CB_coef,
#     selected_vars_count = selected_vars_count,
#     pred_train = pred_train,
#     pred_train_prob = pred_train_prob,
#     pred_test = pred_test,
#     pred_test_prob = pred_test_prob,
#     train_confmatrix = train_confmatrix,
#     test_confmatrix = test_confmatrix,
#     roc_train = roc_train,
#     roc_test = roc_test
#   )
#   
#   return(results)
# }


# Metrics Extraction ------------------------------------------------------

compute_metrics <- function(results, model_name) {
  # Extract metrics from confusion matrix
  train_cm <- results$train_confmatrix
  test_cm <- results$test_confmatrix
  
  # Accuracy
  train_accuracy <- train_cm$overall['Accuracy']
  test_accuracy <- test_cm$overall['Accuracy']
  
  # Sensitivity, Specificity, Precision, Recall, F1 Score
  train_sensitivity <- train_cm$byClass['Sensitivity']
  test_sensitivity <- test_cm$byClass['Sensitivity']
  train_specificity <- train_cm$byClass['Specificity']
  test_specificity <- test_cm$byClass['Specificity']
  train_precision <- train_cm$byClass['Precision']
  test_precision <- test_cm$byClass['Precision']
  train_recall <- train_cm$byClass['Recall']
  test_recall <- test_cm$byClass['Recall']
  train_f1 <- train_cm$byClass['F1']
  test_f1 <- test_cm$byClass['F1']
  
  # Selected Variables Count
  selected_vars_count <- results$selected_vars_count
  
  # AUC
  train_auc <- auc(results$roc_train)
  test_auc <- auc(results$roc_test)
  
  # Train Metrics Data Frame
  train_metrics_df <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(train_accuracy),
    Sensitivity = as.numeric(train_sensitivity),
    Specificity = as.numeric(train_specificity),
    Precision = as.numeric(train_precision),
    Recall = as.numeric(train_recall),
    F1_Score = as.numeric(train_f1),
    Selected_Vars_Count = as.numeric(selected_vars_count),
    AUC = as.numeric(train_auc)
  )
  
  # Test Metrics Data Frame
  test_metrics_df <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(test_accuracy),
    Sensitivity = as.numeric(test_sensitivity),
    Specificity = as.numeric(test_specificity),
    Precision = as.numeric(test_precision),
    Recall = as.numeric(test_recall),
    F1_Score = as.numeric(test_f1),
    Selected_Vars_Count = as.numeric(selected_vars_count),
    AUC = as.numeric(test_auc)
  )
  
  return(list(Train_Metrics = train_metrics_df, Test_Metrics = test_metrics_df))
}



# Function to calculate the average metrics from metrics_list
average_metrics <- function(metrics_list) {
  # Initialize empty lists to store the aggregated metrics for train and test sets
  aggregated_train_metrics <- list()
  aggregated_test_metrics <- list()
  
  # Loop through the metrics_list to aggregate train and test metrics
  for (i in seq_along(metrics_list)) {
    # For each model (ridge, lasso, Alasso, CBPLR) in the current iteration
    for (model_name in names(metrics_list[[i]])) {
      # Extract train and test metrics for the current model
      train_metrics <- metrics_list[[i]][[model_name]]$Train_Metrics
      test_metrics <- metrics_list[[i]][[model_name]]$Test_Metrics
      
      # Append the metrics to the aggregated lists
      aggregated_train_metrics <- append(aggregated_train_metrics, list(train_metrics))
      aggregated_test_metrics <- append(aggregated_test_metrics, list(test_metrics))
    }
  }
  
  # Combine all train and test metrics across all iterations
  combined_train_metrics <- do.call(rbind, aggregated_train_metrics)
  combined_test_metrics <- do.call(rbind, aggregated_test_metrics)
  
  # Calculate the mean of each metric grouped by the model
  mean_train_metrics <- aggregate(. ~ Model, data = combined_train_metrics, FUN = mean)
  mean_test_metrics <- aggregate(. ~ Model, data = combined_test_metrics, FUN = mean)
  
  return(list(Mean_Train_Metrics = mean_train_metrics, Mean_Test_Metrics = mean_test_metrics))
}

# Train Test Split -------------------------------------------------------------------------

train_test_split_repeat <- function(input, train_rate = 0.7, num_repeats = 10, seed_start = 2000, save_dir = "data_splits") {
  # Ensure the directory exists
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  
  # Initialize a counter for file naming
  file_counter <- 1
  
  # Get unique classes in the response variable 'y'
  classes <- unique(input$y)
  
  for (repeat_idx in 1:num_repeats) {
    # Setting the seed for reproducibility
    set.seed(seed_start + repeat_idx - 1)
    
    # Initialize empty data frames for train and test sets
    train_data <- data.frame()
    test_data <- data.frame()
    
    # Stratify, shuffle, and split within each class
    for (class in classes) {
      # Subset data for the current class
      class_data <- input[input$y == class, ]
      
      # Shuffle the rows within the class
      class_data <- class_data[sample(nrow(class_data)), ]
      
      # Determine the size of the training set
      train_size <- floor(train_rate * nrow(class_data))
      
      # Split the data into training and testing sets
      train_indices <- seq_len(train_size)
      class_train <- class_data[train_indices, ]
      class_test <- class_data[-train_indices, ]
      
      # Append to the overall train and test data frames
      train_data <- rbind(train_data, class_train)
      test_data <- rbind(test_data, class_test)
    }
    
    # Shuffle the combined training and testing data to further randomize the order
    train_data <- train_data[sample(nrow(train_data)), ]
    test_data <- test_data[sample(nrow(test_data)), ]
    
    # Define filenames for saving the splits
    train_filename <- file.path(save_dir, paste0("train_set_repeat_", file_counter, ".csv"))
    test_filename <- file.path(save_dir, paste0("test_set_repeat_", file_counter, ".csv"))
    
    # Save the train and test sets
    write.csv(train_data, file = train_filename, row.names = FALSE)
    write.csv(test_data, file = test_filename, row.names = FALSE)
    
    # Increment the file counter
    file_counter <- file_counter + 1
  }
}


# Temp --------------------------------------------------------------------

# ridge_vec_accuracy_train <- c()
# ridge_vec_Sensitivity_train <- c()
# ridge_vec_Specificity_train <- c()
# ridge_vec_Precision_train <- c()
# ridge_vec_Recall_train <- c()
# ridge_vec_F1_Score_train <- c()
ridge_vec_Selected_Vars_Count_train <- c()
lasso_vec_Selected_Vars_Count_train <- c()
Alasso_vec_Selected_Vars_Count_train <- c()
CBPLR_vec_Selected_Vars_Count_train <- c()
# ridge_vec_AUC_train <- c()
# 
# 
# ridge_vec_accuracy_test <- c()
# ridge_vec_Sensitivity_test <- c()
# ridge_vec_Specificity_test <- c()
# ridge_vec_Precision_test <- c()
# ridge_vec_Recall_test <- c()
# ridge_vec_F1_Score_test <- c()
# ridge_vec_Selected_Vars_Count_test <- c()
# lasso_vec_Selected_Vars_Count_test <- c()
# Alasso_vec_Selected_Vars_Count_test <- c()
# CBPLR_vec_Selected_Vars_Count_test <- c()
# ridge_vec_AUC_test <- c()
# 
for(i in 1:10){
#   ridge_vec_accuracy_train <- c(ridge_vec_accuracy_train, results_list[[i]]$result_ridge$train_confmatrix$overall["Accuracy"])
#   ridge_vec_Sensitivity_train <- c(ridge_vec_Sensitivity_train, results_list[[i]]$result_ridge$train_confmatrix$byClass["Sensitivity"])
#   ridge_vec_Specificity_train <- c(ridge_vec_Specificity_train, results_list[[i]]$result_ridge$train_confmatrix$byClass["Specificity"])
#   ridge_vec_Precision_train <- c(ridge_vec_Precision_train, results_list[[i]]$result_ridge$train_confmatrix$byClass["Precision"])
#   ridge_vec_Recall_train <- c(ridge_vec_Recall_train, results_list[[i]]$result_ridge$train_confmatrix$byClass["Recall"])
#   ridge_vec_F1_Score_train <- c(ridge_vec_F1_Score_train, results_list[[i]]$result_ridge$train_confmatrix$byClass["F1"])
  ridge_vec_Selected_Vars_Count_train <- c(ridge_vec_Selected_Vars_Count_train, results_list[[i]]$result_ridge$selected_vars_count)
  lasso_vec_Selected_Vars_Count_train <- c(lasso_vec_Selected_Vars_Count_train, results_list[[i]]$result_lasso$selected_vars_count)
  Alasso_vec_Selected_Vars_Count_train <- c(Alasso_vec_Selected_Vars_Count_train, results_list[[i]]$result_Alasso$selected_vars_count)
  CBPLR_vec_Selected_Vars_Count_train <- c(CBPLR_vec_Selected_Vars_Count_train, results_list[[i]]$result_CBPLR$selected_vars_count)
#   ridge_vec_AUC_train <- c(ridge_vec_AUC_train, results_list[[i]]$result_ridge$roc_train$auc)
}
# 
# 
# for(i in 1:10){
#   ridge_vec_accuracy_test <- c(ridge_vec_accuracy_test, results_list[[i]]$result_ridge$test_confmatrix$overall["Accuracy"])
#   ridge_vec_Sensitivity_test <- c(ridge_vec_Sensitivity_test, results_list[[i]]$result_ridge$test_confmatrix$byClass["Sensitivity"])
#   ridge_vec_Specificity_test <- c(ridge_vec_Specificity_test, results_list[[i]]$result_ridge$test_confmatrix$byClass["Specificity"])
#   ridge_vec_Precision_test <- c(ridge_vec_Precision_test, results_list[[i]]$result_ridge$test_confmatrix$byClass["Precision"])
#   ridge_vec_Recall_test <- c(ridge_vec_Recall_test, results_list[[i]]$result_ridge$test_confmatrix$byClass["Recall"])
#   ridge_vec_F1_Score_test <- c(ridge_vec_F1_Score_test, results_list[[i]]$result_ridge$test_confmatrix$byClass["F1"])
#   ridge_vec_Selected_Vars_Count_test <- c(ridge_vec_Selected_Vars_Count_test, results_list[[i]]$result_ridge$selected_vars_count)
#   lasso_vec_Selected_Vars_Count_test <- c(lasso_vec_Selected_Vars_Count_test, results_list[[i]]$result_lasso$selected_vars_count)
#   Alasso_vec_Selected_Vars_Count_test <- c(Alasso_vec_Selected_Vars_Count_test, results_list[[i]]$result_Alasso$selected_vars_count)
#   CBPLR_vec_Selected_Vars_Count_test <- c(CBPLR_vec_Selected_Vars_Count_test, results_list[[i]]$result_CBPLR$selected_vars_count)
#   ridge_vec_AUC_test <- c(ridge_vec_AUC_test, results_list[[i]]$result_ridge$roc_test$auc)
# }

