# Define the probability
probability <- 0.3

# Calculate the PMF values for the Bernoulli distribution with p = 0.3
pmf <- data.frame(x = c(0, 1), y = c(1 - probability, probability))

# Plot the PMF for the Bernoulli distribution with p = 0.3 using ggplot2 with ylim
p1 <- ggplot(pmf, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       subtitle = "p = 0.3",
       x = "Outcome",
       y = "Probability",
       fill = "Outcome") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0, 1)


# Define the probability
probability <- 0.5

# Calculate the PMF values for the Bernoulli distribution with p = 0.3
pmf <- data.frame(x = c(0, 1), y = c(1 - probability, probability))

# Plot the PMF for the Bernoulli distribution with p = 0.3 using ggplot2 with ylim
p2 <- ggplot(pmf, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bernoulli Distribution Probability Mass Function",
       subtitle = "p = 0.5",
       x = "Outcome",
       y = "Probability",
       fill = "Outcome") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)


# Define the probability
probability <- 0.7

# Calculate the PMF values for the Bernoulli distribution with p = 0.3
pmf <- data.frame(x = c(0, 1), y = c(1 - probability, probability))

# Plot the PMF for the Bernoulli distribution with p = 0.3 using ggplot2 with ylim
p3 <- ggplot(pmf, aes(x = factor(x), y = y, fill = factor(x))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       subtitle = "p = 0.7",
       x = "Outcome",
       y = "Probability",
       fill = "Outcome") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0, 1)

ggarrange(p1, p2, p3, ncol = 3)
ggsave("BerDistPlots.png", width = 2, height = 4)
