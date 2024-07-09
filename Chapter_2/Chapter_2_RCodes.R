
# Chapter-2 Codes ---------------------------------------------------------

# Load the ggplot2 library
library(ggplot2)

# Define the Logistic Function
Logistic_Function <- function(z){
  1/(1 + exp(-z))
}

# Create a data frame for the curve
df <- data.frame(z = seq(-8, 8, by = 0.01),
                 y = Logistic_Function(seq(-8, 8, by = 0.01)))

# Create the ggplot object
ggplot(df, aes(x = z, y = y)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  geom_segment(aes(x = 0, xend = 8, y = 1, yend = 1), linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1), linetype = "solid", size = 1) +
  scale_x_continuous(breaks = c(-8, 0, 8), labels = c("-∞", "0", "+∞")) +
  geom_text(aes(x = -1.2, y = 0.5, label = "1/2"), vjust = -0.5, hjust = 0) +
  geom_text(aes(x = -0.75, y = 0.98, label = "1"), vjust = -0.5, hjust = 0) +
  geom_point(aes(x = 0, y = 0.5), color = "red", shape = 19, size = 3) +
  annotate("text", x = -6, y = 0.75, label = expression(f(z) == frac(1, 1 + e^-z)), hjust = 0, vjust = 0) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  



