
# Loading The Datasets: ---------------------------------------------------




# Fitting the models: -----------------------------------------------------

# result_ridge <- automate_ridge(train_data = colonData_train,
#                                test_data = colonData_test,
#                                response_col_name = "y")
# 
# result_lasso <- automate_lasso(train_data = colonData_train,
#                                test_data = colonData_test,
#                                response_col_name = "y")
# 
# result_Alasso <- automate_adaptive_lasso(train_data = colonData_train,
#                                          test_data = colonData_test,
#                                          response_col_name = "y",
#                                          lasso_coef = result_lasso$fit_lasso_coef)
# 
# result_CBPLR <- automate_correlation_based_lasso(train_data = colonData_train,
#                                                  test_data = colonData_test,
#                                                  response_col_name = "y",
#                                                  CB_coef_file_path = CB_coef_file_path)


# Metrics Extraction: -----------------------------------------------------

# Calculate metrics for each model
# metrics_ridge <- compute_metrics(result_ridge, "Ridge")
# metrics_lasso <- compute_metrics(result_lasso, "LASSO")
# metrics_Alasso <- compute_metrics(result_Alasso, "Adaptive LASSO")
# metrics_CBPLR <- compute_metrics(result_CBPLR, "Correlation-Based PLR")


# Combine all metrics into one data frame for train and test sets
# all_train_metrics <- rbind(metrics_ridge$Train_Metrics, 
#                            metrics_lasso$Train_Metrics, 
#                            metrics_Alasso$Train_Metrics, 
#                            metrics_CBPLR$Train_Metrics)
# 
# all_test_metrics <- rbind(metrics_ridge$Test_Metrics, 
#                           metrics_lasso$Test_Metrics, 
#                           metrics_Alasso$Test_Metrics, 
#                           metrics_CBPLR$Test_Metrics)



# Train Test Split --------------------------------------------------------

train_test_split_repeat(input = colonData, train_rate = 0.7, num_repeats = 100, seed_start = 2024, save_dir = "data_splits_stratify2")

# -------------------------------------------------------------------------

# Directory containing the CB coefficient files
cb_coef_dir <- "D:/Github/MScDissertationDocs/MScDissertationDocs/cbBetas_temp2/lambda12"

# Pattern to match CB coefficient files
cb_coef_pattern <- "beta_repeat_"

# List to store the results
results_list <- list()

# List to store the metrics
metrics_list <- list()

# Load all CB coefficient file paths with the specific pattern
cb_coef_files <- list.files(path = cb_coef_dir, pattern = cb_coef_pattern, full.names = TRUE)

# Loop over all 50 train and test set pairs
for (i in 1:50) {
  
  # Load the train and test datasets
  train_file_path <- paste0("D:/Github/MScDissertationDocs/MScDissertationDocs/data_splits_stratify2/train_set_repeat_", i, ".csv")
  test_file_path <- paste0("D:/Github/MScDissertationDocs/MScDissertationDocs/data_splits_stratify2/test_set_repeat_", i, ".csv")
  colonData_train <- read.csv(train_file_path)
  colonData_test <- read.csv(test_file_path)
  
  # Load the corresponding CB coefficient file
  cb_coef_file_name <- paste0(cb_coef_pattern, i, ".csv")
  cb_coef_file_path <- file.path(cb_coef_dir, cb_coef_file_name)
  CB_coef <- read.csv(cb_coef_file_path)[, 2]
  
  # Fit the models
  result_ridge <- automate_ridge(train_data = colonData_train,
                                 test_data = colonData_test,
                                 response_col_name = "y")
  result_lasso <- automate_lasso(train_data = colonData_train, 
                                 test_data = colonData_test,
                                 response_col_name = "y")
  result_Alasso <- automate_adaptive_lasso(train_data = colonData_train, 
                                           test_data = colonData_test, 
                                           response_col_name = "y",
                                           lasso_coef = result_lasso$fit_lasso_coef)
  result_CBPLR <- automate_correlation_based_lasso(train_data = colonData_train,
                                                   test_data = colonData_test,
                                                   response_col_name = "y",
                                                   CB_coef = CB_coef)
  
  # Store the results in the list
  results_list[[i]] <- list(result_ridge = result_ridge,
                            result_lasso = result_lasso,
                            result_Alasso = result_Alasso,
                            result_CBPLR = result_CBPLR)
  
  metrics_list[[i]] <- list(ridgeMetrics = compute_metrics(results_list[[i]]$result_ridge, "ridge"),
                            lassoMetrics = compute_metrics(results_list[[i]]$result_lasso, "lasso"),
                            AlassoMetrics = compute_metrics(results_list[[i]]$result_Alasso, "Alasso"),
                            CBPLRMetrics = compute_metrics(results_list[[i]]$result_CBPLR, "CBPLR"))
}



# Calculate the average metrics
average_metrics_results <- average_metrics(metrics_list)

# Print the results
print("Average Train Metrics:")
print(average_metrics_results$Mean_Train_Metrics)

print("Average Test Metrics:")
print(average_metrics_results$Mean_Test_Metrics)





