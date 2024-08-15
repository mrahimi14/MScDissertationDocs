require(caret)
MLDataR::heartdisease

myData <- MLDataR::heartdisease

# Data Structure:
glimpse(myData)

# Checking for duplication:
sum(duplicated(myData))

# Checking the missing rate:
sapply(myData, function(x) mean(is.na(x)))

myData <- myData %>% 
  mutate_if(is.character, as.factor)

myData$HeartDisease <- as.factor(myData$HeartDisease)
myData$FastingBS <- as.factor(myData$FastingBS)

# Setting a seed for reproducibility:
set.seed(2000)

# Train_Test_Split:
idx <- createDataPartition(myData$HeartDisease,
                           p = 0.75,
                           list = FALSE)
train_set <- myData[idx, ]
test_set <- myData[-idx, ]


# Fitting the Logistic Regression model:
# Setting the 10-fold Cross Validation:
train_control <- trainControl(method = 'cv', number = 10)

# Fitting Logisic Regression:
fit_lr <- train(HeartDisease ~ .,
                data = myData,
                trCtrl = train_control,
                method = 'glm')

pred_lr <- predict(fit_lr, newdata = test_set)
pred_lr <- ifelse(pred_lr <= 0.5, 0, 1)
confusionMatrix(factor(pred_lr), factor(test_set$HeartDisease))


# Define a range of k values for k-fold cross-validation
k_values <- seq(2, 20, by = 1)  # Example range from 2 to 20

# Initialize a vector to store error rates
error_rates <- numeric(length(k_values))

# Iterate over different k values
for (i in seq_along(k_values)) {
  # Define the training control with current k-fold value
  train_control <- trainControl(method = "cv",
                                number = k_values[i])
  
  # Train the logistic regression model with current k-fold value
  fit_lr <- train(factor(HeartDisease) ~ .,
                  data = train_set,
                  method = "glm",
                  trControl = train_control)
  
  # Calculate error rate and store it
  error_rates[i] <- fit_lr$results$Accuracy
}

# Create a data frame for plotting
plot_data <- data.frame(k_values, error_rates)

# Plot error rate vs. k
ggplot(plot_data, aes(x = k_values, y = error_rates)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Error Rate vs. K-Fold Cross-Validation",
       x = "K-Fold Value", y = "Error Rate")






