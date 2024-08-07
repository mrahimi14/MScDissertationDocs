
# Loading the required packages -------------------------------------------

pkg <- c('tidyverse',
         'caret',
         'glmnet',
         'ggpubr')

for(i in 1:length(pkg)){
  if(!(pkg[i] %in% installed.packages())) {
    install.packages(pkg[i])
  }
  library(pkg[i], character.only = TRUE)
}


# Chapter-2 Codes ---------------------------------------------------------

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
  geom_text(aes(x = -1.5, y = 0.5, label = "1/2"), vjust = -0.5, hjust = 0) +
  geom_text(aes(x = -0.75, y = 0.98, label = "1"), vjust = -0.5, hjust = 0) +
  geom_point(aes(x = 0, y = 0.5), color = "red", shape = 19, size = 3) +
  annotate("text", x = -6, y = 0.75, label = expression(f(z) == frac(1, 1 + e^-z)), hjust = 0, vjust = 0) +
  annotate("text", x = -8, y = 0.25, label = expression(f(z) ~ approx ~ 0), hjust = 0, vjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text.y = element_blank(), 
        axis.title.y = element_blank())


# Example-1 ---------------------------------------------------------------

# Creating the data.frame:
insect_df <- data.frame(
  y = c(6, 13, 18, 28, 52, 53, 61, 60),
  n = c(59, 60, 62, 56, 63, 59, 62, 60),
  x = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
)

# Plotting the `pi=y/n` Vs x:
insect_df %>% 
  ggplot(aes(x = x, y = y/n)) + 
  geom_point(size = 2) + 
  ggtitle(expression("Scatter Plot of " ~ pi ~ " Vs X")) +
  ylab("y") +
  geom_segment(aes(x = 1.6, xend = 1.9, y = 0, yend = 0)) +
  geom_segment(aes(x = 1.6, xend = 1.6, y = 0, yend = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Add Y-axis labels as percentages
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())


# Recreating the response variable:
responseMatrix <- with(insect_df, cbind(y, n-y))

# Fitting the logistic regression model:
fit_lr <- glm(responseMatrix ~ insect_df$x,
              family = binomial(link = "logit"))

# The fitted model properties:
cbind(summary(fit_lr)$coef, Exp_Estimate = exp(coef(fit_lr)))


# Assuming you have the xtable package installed
library(xtable)

# Create the data frame with the coefficients and exponentiated estimates
model_summary <- cbind(summary(fit_lr)$coef, Exp_Estimate = exp(coef(fit_lr)))

# Convert the data frame to a LaTeX table
latex_table <- xtable(model_summary,
                      caption = "جدول برآورد ضرایب مدل رگرسیون لوژستیک مثال اول",
                      auto = T)

# Print the LaTeX code for the table
print(latex_table, include.rownames = TRUE)



# Logistic Loss Functions -------------------------------------------------

# Generate a sequence of probabilities
probs <- seq(0, 1, by = 0.01)

# Calculate the logistic loss functions
log_loss_1 <- -log(probs)
log_loss_2 <- -log(1 - probs)

# Create a data frame for plotting
df <- data.frame(probs, log_loss_1, log_loss_2)

# Plot the logistic loss functions using ggplot2
ggplot(df, aes(x = probs)) +
  geom_line(aes(y = log_loss_1, color = "h(x) = -log(P(x))")) +
  geom_line(aes(y = log_loss_2, color = "h(x) = -log(1 - P(x))")) +
  labs(title = "Logistic Loss Functions",
       x = expression("P(x)"),
       y = expression("L(p, y)"),
       color = "Function") +
  geom_text(aes(x = 0.25, y = 4, label = "y = 1"),
            size = 5,
            col = 'blue') +
  geom_text(aes(x = 0.75, y = 4, label = "y = 0"),
            size = 5,
            col = 'red') +
  scale_color_manual(values = c("h(x) = -log(P(x))" = "blue", "h(x) = -log(1 - P(x))" = "red")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 

# ggsave('bothLogisticLossFunctions.png')
# Sep ---------------------------------------------------------------------

# Generate a sequence of probabilities
probs <- seq(0, 1, by = 0.01)

# Calculate the logistic loss functions
log_loss_1 <- -log(probs)
log_loss_2 <- -log(1 - probs)

# Create a data frame for plotting
df <- data.frame(probs, log_loss_1, log_loss_2)

# Plot the logistic loss functions using ggplot2
p1 <- ggplot(df, aes(x = probs)) +
  geom_line(aes(y = log_loss_1)) +
  labs(title = "Logistic Loss Functions",
       subtitle = expression("h(x) = -log(P(x))"),
       x = expression("P(x)"),
       y = expression("L(p, y)")) +
  geom_text(aes(x = 0.5, y = 4, label = "y = 1"),
            size = 5) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 
# ggsave('fig_y1.jpg')

# Plot the logistic loss functions using ggplot2
p2 <- ggplot(df, aes(x = probs)) +
  geom_line(aes(y = log_loss_2)) +
  labs(title = "Logistic Loss Functions",
       subtitle = expression("h(x) = -log(1 - P(x))"),
       x = expression("P(x)"),
       y = expression("L(p, y)")) +
  geom_text(aes(x = 0.5, y = 4, label = "y = 0"),
            size = 5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) 
ggsave('fig_y0.jpg')

ggarrange(p1, p2, ncol = 2)
ggsave('SepLogisticLossFunctions.png')
