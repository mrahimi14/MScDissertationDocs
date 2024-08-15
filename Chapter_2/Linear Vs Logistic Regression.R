# sleep <- data.frame(
#   time = c(30, 45, 60, 75, 90, 105, 105, 120, 135,
#            150, 165, 180, 195, 210, 240, 255, 270,
#            285, 300, 330),
#   remind = c(0, 0, 0, 0, 0, 0, 1, 0, 0,
#              0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1)
# )
# 
# # fitting a linear regression model
# fit_lr <- lm(remind ~ time, data = sleep)
# 
# par(mfrow = c(1, 2))
# with(data = sleep, plot(remind ~ time,
#                         col = 'red3',
#                         pch = 19))
# abline(fit_lr, lw = 2)
# 
# 
# fit_glm <- glm(remind ~ time,
#                family = 'binomial',
#                data = sleep)
# pred_glm <- fitted(fit_glm)
# with(data = sleep, plot(remind ~ time,
#                         col = 'red3',
#                         pch = 19))
# lines(sleep$time,
#       pred_glm,
#       lw = 2)


# Load the Required Package:
library(ggplot2)

# DataFrame:
sleep <- data.frame(
  time = c(30, 45, 60, 75, 90, 105, 105, 120, 135,
           150, 165, 180, 195, 210, 240, 255, 270,
           285, 300, 330),
  remind = c(0, 0, 0, 0, 0, 0, 1, 0, 0,
             0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1)
)

# Fitting a linear regression model:
fit_lr <- lm(remind ~ time, data = sleep)

# Fitting a logistic regression model:
fit_glm <- glm(remind ~ time, family = binomial, data = sleep)
pred_glm <- predict(fit_glm, type = "response")

# Create two plots beside each other:
plot_lr <- ggplot(sleep, aes(x = time, y = remind)) +
  geom_point(color = "red3", shape = 19, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  labs(title = "Linear Regression Model",
       x = "x",
       y = "y") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

plot_glm <- ggplot(sleep, aes(x = time, y = remind)) +
  geom_point(color = "red3", shape = 19, size = 3) +
  geom_line(aes(y = pred_glm), color = "black", size = 1) +
  labs(title = "Logistic Regression Model",
       x = "x",
       y = "y") +
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1)) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plots side by side
gridExtra::grid.arrange(plot_lr, plot_glm, ncol = 2)


# Loading the Required Package:
require(pROC)

# DataFrame:
sleep <- data.frame(
  time = c(30, 45, 60, 75, 90, 105, 105, 120, 135,
           150, 165, 180, 195, 210, 240, 255, 270,
           285, 300, 330),
  remind = c(0, 0, 0, 0, 0, 0, 1, 0, 0,
             0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1)
)

# Calculate predicted probabilities
sleep$predicted_prob <- predict(fit_glm, type = "response")

# Create a ROC curve object
roc_obj <- roc(sleep$remind, sleep$predicted_prob)

# Plot the ROC curve
plot(roc_obj, 
     main = "ROC Curve for Logistic Regression Model", 
     col = "blue",
     print.auc = T)

