# Load the Required Package:
library(ggplot2)

# Create sample data
set.seed(123) 
x <- seq(-10, 10, by = 1)
y <- 1 / (1 + exp(-x)) 

# Create a dataframe
data <- data.frame(x = x, y = y)

# Generate scatter plot 
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = 'red', size = 3) + 
  geom_smooth(method = 'glm',
              method.args=list(family=binomial),
              color='blue',
              se = F) +  
  scale_y_continuous(breaks = c(0, 1), limits = c(0, 1.2)) + 
  geom_segment(aes(x = -8, xend = 0, y = 0, yend = 0), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 8, y = 1, yend = 1), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1.15), linetype = "solid", size = 1) +
  theme_minimal() +
  # xlab("") +
  ylab("f(x)") +
  theme(plot.title = element_text(hjust = 0.5)) 
