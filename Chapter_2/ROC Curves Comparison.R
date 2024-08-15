
# ROC ---------------------------------------------------------------------


# plot(y ~ x, type = 'l', col = 'purple')

df <- data.frame(
  x = seq(0, 1, 0.01),
  y = c(log(x), log(x^2)),
  g = rep(2:3, each = length(x))
)

# df %>%
#   ggplot(aes(x = x, y = y, group = g, col = factor(g))) +
#   geom_ribbon(aes(ymin = -Inf, ymax = y, fill = factor(g)), alpha = 0.3) +
#   geom_line() +
#   geom_abline(slope = 10, intercept = -10, linetype = "dashed", color = "orange", size = 1) +
#   ggtitle("ROC Curve") +
#   scale_y_continuous(breaks = seq(-10, 0, 2), labels = seq(0, 1, 0.2)) +
#   scale_color_manual(values = c("red", "blue"), labels = c("Model-1", "Model-2")) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_blank())


df %>%
  ggplot(aes(x = x, y = y, group = g, col = factor(g))) +
  geom_ribbon(aes(ymin = -Inf, ymax = y, fill = factor(g)), alpha = 0.3) +  # Decrease alpha for less transparency
  geom_line() +
  geom_abline(slope = 10, intercept = -10, linetype = "dashed", color = "orange", size = 1.2) +
  ggtitle("ROC Curve") +
  xlab("False Positive Rate (FPR)") + 
  ylab("True Positive Rate (TPR)") + 
  scale_y_continuous(breaks = seq(-10, 0, 2), labels = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("cyan", "blue"), labels = c("Model-1 ROC", "Model-2 ROC")) +
  scale_fill_manual(values = c("cyan", "blue"), labels = c("Model-1 AUC", "Model-2 AUC")) +  # Set custom fill colors and labels
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.title = element_blank())
