
# Logistic Regression Example ---------------------------------------------

require(faraway)
require(tidyverse)
require(ggpubr)
data("wcgs", package = "faraway")
wcgs <- na.omit(wcgs)
wcgs <- wcgs %>% 
  filter(chol < 450)
wcgs <- sample(wcgs, 100)
# fit_glm <- glm(chd ~ .,
#                family = "binomial",
#                data = wcgs)
# step(fit_glm)

fit_glm <- glm(chd ~ chol + cigs,
               family = 'binomial',
               data = wcgs)
summary(fit_glm)
pred_glm <- fitted(fit_glm)
plot(pred_glm ~ wcgs$chol, cex = 0.5)
plot(pred_glm ~ wcgs$cigs, cex = 0.5)

# Assuming pred_glm and wcgs are available in your environment


# Create a scatter plot with a linear regression line using ggplot2
p1 <- ggplot(data = wcgs, aes(x = chol, y = pred_glm)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "CHD logits by cholestero",
       x = "Empirical Cholesterol",
       y = "Log")


# Create a scatter plot with a linear regression line using ggplot2
p2 <- ggplot(data = wcgs, aes(x = cigs, y = pred_glm)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "CHD logits by cigarette",
       x = "Cigarettes",
       y = "Empirical Logit")


ggarrange(p1, p2, ncol = 1)
