# Clearing the Environment:
rm(list = ls())
if(!is.null(dev.list())) dev.off()

# Loading the required packages -------------------------------------------

pkg <- c('tidyverse',
         'scatterplot3d',
         'caret',
         'MLDataR')

for(i in 1:length(pkg)){
  if(!(pkg[i] %in% installed.packages())) {
    install.packages(pkg[i])
  }
  library(pkg[i], character.only = TRUE)
}


# Chapter-3 Codes --------------------------------------------------------


# Curse_Of_Dimensionality_Plot ------------------------------------------------

# Setting a seed for reproducibility:
set.seed(2003)

windows(20, 14)
# Splitting the Layout:
layout(matrix(c(1, 1, 1, 1, 2, 2, 3, 3, 2, 2, 3, 3), nrow = 3, byrow = TRUE))
# layout(matrix(1))

# 1-Dimension
df1 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = rep(0.5, 9),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df1 <- df1 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

# win.graph(14, 5)
with(df1,
     plot(y ~ x, 
          col = g, 
          pch = g,
          cex = 4,
          xlim = c(0, 3),
          xlab = "",
          ylab = "",
          main = "داده در یک بعد (الف)"))
abline(v = c(1, 2), col = 'red', lt = 2, lw = 2)


# 2-Dimension
df2 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df2 <- df2 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

with(df2,
     plot(y ~ x, 
          col = g, 
          pch = g,
          cex = 3,
          xlim = c(0, 3),
          ylim = c(0, 3),
          xlab = "",
          ylab = "",
          main = "داده در دو بعد (ب)"))
abline(v = c(1, 2), col = 'red', lt = 2, lw = 2)
abline(h = c(1, 2), col = 'red', lt = 2, lw = 2)


# 3-Dimension
df3 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  z = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df3 <- df3 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

plt <- with(df3, 
            scatterplot3d(x, y, z,
                          color = g,
                          pch = g,
                          angle = 200,
                          cex.symbols = 3,
                          zlim = c(0,3),
                          xlab = "",
                          ylab = "",
                          zlab = "",
                          main= "داده در سه بعد (ج)"))

plt$plane3d(2, 0, 0, col = 2, lw = 1.5)
plt$plane3d(1, 0, 0, col = 2, lw = 1.5)



# Bias_Variance_TradeOff --------------------------------------------------

rm(list = ls())
if(!is.null(dev.list())) dev.off()

n <- 100
r <- 1
alpha <- 0.5
beta <- 20
f <- function(x) dexp(x, r) + dgamma(1/x, alpha, beta) + 0.015
f <- Vectorize(f)

curve(dexp(x, r), 0, 8, col = 2, lwd = 2, ylim = c(0, 0.4))
curve(dgamma(1/x, alpha, beta), 0, 8, lwd = 2, col = 4, add = T)
curve(f, 0, 10, lt = 2, lwd = 2, add = T)
abline(v = optimize(f, c(2, 6))$min, col = 'orange', lwd = 2, lt = 3)
legend(6.3, 0.415, legend = c('Variance', 'Bias^2', 'MSE'), col = c(2, 4, 1), lt = c(1, 1, 2), bg = 'white')

# Heart_Disease_Data_Modeling ---------------------------------------------

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
       x = "K-Fold Value", y = "Error Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal()







