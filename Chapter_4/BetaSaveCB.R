
################################################################################
#######################THIS CODE SHOULD BE RUN BY R (VERSION 3.2.3)#############
################################################################################

process_and_save_betas <- function(train_dir, test_dir, num_repeats, lam_opt, output_dir) {
  # Load required libraries
  library(lqa) # If you don't have `lqa` package, you need to install it first
  
  for (i in 1:num_repeats) {
    # Construct file paths
    train_file <- file.path(train_dir, paste0("train_set_repeat_", i, ".csv"))
    test_file <- file.path(test_dir, paste0("test_set_repeat_", i, ".csv"))
    
    # Load data
    colonData_train <- read.csv(train_file)
    colonData_test <- read.csv(test_file)
    
    # X and y
    colon_X_train <- colonData_train[, -1]
    colon_y_train <- colonData_train[, 1]
    
    # Converting colon_y_train from 1&2 into 0&1
    colon_y_train <- ifelse(colon_y_train == 1, 0, 1)
    
    # Fit the model
    obj <- lqa(as.matrix(colon_X_train),
               as.vector(colon_y_train),
               family = binomial(),
               penalty = penalreg(lam_opt),
               control = lqa.control())
    
    # Get coefficients
    beta <- obj$coef
    
    # Save coefficients to CSV
    output_file <- file.path(output_dir, paste0("beta_repeat_", i, ".csv"))
    write.csv(beta, output_file)
  }
}

process_and_save_betas(train_dir = "D:/Github/MScDissertationDocs/MScDissertationDocs/data_splits",
                       test_dir = "D:/Github/MScDissertationDocs/MScDissertationDocs/data_splits",
                       num_repeats = 50,
                       lam_opt = 0.1,
                       output_dir = "D:/Github/MScDissertationDocs/MScDissertationDocs/cbBetas")











