
# Logistic Function -------------------------------------------------------

# Load the required packges:
library(ggplot2)

# Generate x values:
x <- seq(-10, 10, by = 0.1)

# Create a data frame:
logistic_data <- data.frame(x = rep(x, 5), 
                            beta = rep(c(0.5, 1, 2, 4, 10), each = length(x)))

# Calculate y values for each beta (Logistic Function):
logistic_data$y <- 1 / (1 + exp(-logistic_data$beta * logistic_data$x))

# Plot the logistic functions for different beta values:
ggplot(logistic_data, aes(x = x, y = y, color = factor(beta))) +
  geom_line(size = 1) +
  labs(title = "Logistic Functions with Different Parameters",
       x = "x",
       y = "f(x)",
       color = "Beta") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = -3, y = 0.75, label = bquote(f(x) == frac(1, 1 + e^(-beta*x))), color = "black", size = 6, hjust = 1, vjust = -1)
