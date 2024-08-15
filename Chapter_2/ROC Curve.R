
# Empirical ROC -----------------------------------------------------------

xpoints <- c(0, 0, 0, 0.2, 0.2, 0.2, 0.4, 0.6, 0.8, 0.8, 1)
ypoints <- c(0, 0.2, 0.4, 0.4, 0.6, 0.8, 0.8, 0.8, 0.8, 1, 1)
plot(xpoints, ypoints, type = 'o')


# Load necessary library
library(ggplot2)

# Define the data points
xpoints <- c(0, 0, 0, 0.2, 0.2, 0.2, 0.4, 0.6, 0.8, 0.8, 1)
ypoints <- c(0, 0.2, 0.4, 0.4, 0.6, 0.8, 0.8, 0.8, 0.8, 1, 1)

# Create a dataframe
data <- data.frame(x = xpoints, y = ypoints)

# Generate the plot using ggplot2 with smoothing
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = 'red', size = 1.5) + 
  geom_line(color = 'red') +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Empirical ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))




Obs <- 1:10
Class <- c("P", "P", "N", "P", "P", "N", "N", "N", "P", "N")
prob <- c(0.90, 0.80, 0.70, 0.60, 0.55, 0.54, 0.53, 0.51, 0.50, 0.40)
TP <- c(1, 2, 2, 3, 4, 4, 4, 4, 5, 5)
FP <- c(0, 0, 1, 1, 1, 2, 3, 4, 4, 5)
TN <- c(5, 5, 4, 4, 4, 3, 2, 1, 0, 0)
FN <- c(4, 3, 3, 2, 1, 1, 1, 1, 1, 0)
TPR <- c(0.2, 0.4, 0.4, 0.6, 0.8, 0.8, 0.8, 0.8, 1, 1)
FPR <- c(0, 0, 0.2, 0.2, 0.2, 0.4, 0.6, 0.8, 0.8, 1)





