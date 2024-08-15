# Load the necessary libraries
require(ggplot2)

# Parameters for the Binomial distribution
n_values <- c(10, 20, 30)  # Different values of n
p_values <- c(0.3, 0.5, 0.7)  # Different values of p

# Generate data for the Binomial distribution with more points for smoother lines
binomial_data <- expand.grid(n = n_values, p = p_values, x = seq(0, max(n_values), by = 0.1))

# Calculate the probabilities for each combination of n, p, and x
binomial_data$probability <- dbinom(binomial_data$x, size = binomial_data$n, prob = binomial_data$p)

# Define custom labels for the facets
n_labels <- c("10" = "n = 10", "20" = "n = 20", "30" = "n = 30")
p_labels <- c("0.3" = "p = 0.3", "0.5" = "p = 0.5", "0.7" = "p = 0.7")

# Plot the Binomial distribution with facets for n and p values with custom labels
ggplot(binomial_data, aes(x = x, y = probability, color = factor(p))) +
  geom_line(size = 1) +
  facet_grid(rows = vars(n), cols = vars(p), labeller = labeller(n = as_labeller(n_labels), p = as_labeller(p_labels))) +
  labs(title = "Binomial Distribution with Different Parameters",
       x = "X",
       y = "Probability",
       color = "p") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 24, hjust = 0.5),  
        axis.title = element_text(size = 18),  
        axis.text = element_text(size = 18),  
        strip.text = element_text(size = 18),  
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18)) 
