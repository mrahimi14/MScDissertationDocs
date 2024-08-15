

require(tidyverse)
require(ggpubr)
require(xtable)
require(pROC)
require(caret)

# path <- "D:\\Github\\MScDissertationDocs\\MScDissertationDocs\\Chapter_2\\leukemia_remission.txt"
# luckData <- read.table(path, header = T)
# 
# fit_glm <- glm(REMISS ~ LI + BLAST,
#                family = 'binomial',
#                data = luckData) 
# 
# summary(fit_glm)
# pred_glm <- fitted(fit_glm)
# plot(pred_glm ~ luckData$LI)
# plot(pred_glm ~ luckData$BLAST)


path <- "D:\\Github\\MScDissertationDocs\\MScDissertationDocs\\Chapter_2\\RR_Data_Hale.csv"
HaleData <- read.csv(path, header = T)
HaleData <- na.omit(HaleData)

fit_glm <- glm(cbind(Yes.1st.Vote, Num.Voting.1st - Yes.1st.Vote) ~
                 Distance.from.RR + X..Pop..Black,
               family = 'binomial',
               data = HaleData) 

summary(fit_glm)
pred_glm <- fitted(fit_glm)
expCoef <- exp(coef(fit_glm))

p1 <- ggplot(data = HaleData, aes(x = Distance.from.RR, y = pred_glm)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "green3") +
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.001, 1)) +
  labs(title = "Hale Empirical Logits by Distance",
       x = "Distance",
       y = "Empirical Logits") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(data = HaleData, aes(x = X..Pop..Black, y = pred_glm)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "green3") +
  scale_y_continuous(breaks = c(0, 1), limits = c(-0.001, 1)) +
  labs(title = "Hale Empirical Logits by Percentage of Black Residents",
       x = "Percentage of Black Residents",
       y = "Empirical Logits") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(p1, p2, ncol = 1)



# Classification for the predictions:
pred_glm_Category <- ifelse(pred_glm <= 0.5, "No", "Yes")

# Classification for the responses:
# responseCategory <- ifelse(HaleData$Yes.1st.Vote/HaleData$Num.Voting.1st <= 0.5, "No", "Yes")
responseCategory <- ifelse(HaleData$Yes.1st.Vote/HaleData$Num.Voting.1st <= 0.5, 0, 1)


# Confusion Matrix:
confusion_matrix <- confusionMatrix(factor(pred_glm_Category),
                factor(responseCategory),
                positive = "Yes")

confusion_matrix$table
TP <- confusion_matrix$table[2, 2]
TN <- confusion_matrix$table[1, 1]
FP <- confusion_matrix$table[2, 1]
FN <- confusion_matrix$table[1, 2]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
TPR <- (TP) / (TP + FN)
FPR <- (FP) / (FP + TN)
specificity <- (TN) / (TN + FP)
preci <- (TP) / (TP + FP)
F1 <- (TP) / (TP + (1/2)*(FP + FN))

formulae <- c("TP + TN / (TP + TN + FP + FN)",
              "(TP) / (TP + FN)",
              "(FP) / (FP + TN)",
              "(TN) / (TN + FP)",
              "(TP) / (TP + FP)",
              "(TP) / (TP + (1/2)*(FP + FN))")

values <- c(accuracy, TPR, FPR, specificity, preci, F1)
measures <- c("Accuracy", "TPR(Sensitivity)", "FPR", "Specificity", "Precision", "F1")
data.frame(
  Measure = measures,
  Formula = formulae,
  Value = values
) %>% xtable()

# Create ROC curve data
roc_data <- roc(responseCategory, pred_glm)

# Plot ROC curve with x-axis from 0 to 1
plot.roc(roc_data, main = "ROC curve of Logistic Regression model",
         xlab = "False Positive Rate (FPR)",
         ylab = "True Positive Rate (TPR)",
         print.auc = TRUE,
         auc.polygon = TRUE,
         print.auc.col = "#1c61b6",
         auc.polygon.col = "lightgreen",
         legacy.axes = TRUE)


# par(mfrow = c(1, 2))
# par(mfrow = c(1, 1))

beta <- matrix(-coef(fit_glm))
X <- HaleData %>%
  select(X..Pop..Black, Distance.from.RR) %>% 
  cbind(1, .) %>% 
  as.matrix()

func <- function(x2, x3) 1 / (1 + exp(15-beta[1]-x2*beta[2]-x3*beta[3]))
plot(HaleData$X..Pop..Black,
     responseCategory, 
     col = responseCategory+1,
     xlab = "Black Percentage",
     ylab = "Response",
     pch = 19)
curve(func,
      from = 10,
      to = 90,
      add = T, 
      col = 4,
      lw = 2)

func <- function(x) 1 / (1 + exp(-(15-beta[1]-x*beta[2]-x*beta[3])))

plot(HaleData$Distance.from.RR,
     responseCategory,
     col = responseCategory+1,
     xlab = "Black Percentage",
     ylab = "Distance",
     # xlim =,
     pch = 19)
curve(func,
      from = 0,
      to = 20,
      add = T,
      col = 4)


# -------------------------------------------------------------------------

z <- beta[1]*X[, 1] + beta[2]*X[, 2] + beta[3]*X[, 3]

fit <- glm(y ~ Distance.from.RR + X..Pop..Black,
           family = 'binomial',
           data = HaleData)


# -------------------------------------------------------------------------

library(ggplot2)
library(ggeffects)

HaleData$y <- responseCategory
fit <- glm(y ~ Distance.from.RR + X..Pop..Black,
           family = 'binomial',
           data = HaleData)

# Generate the ggpredict object
pred_plot <- ggpredict(fit, "Distance.from.RR", ci_level = NA)

# Convert to a data frame for ggplot
pred_data <- as.data.frame(pred_plot)

# Create the ggplot object
p3 <- ggplot(pred_data, aes(x = x, y = predicted)) + 
  geom_line(col = 'purple') + 
  geom_point(data = HaleData,
             aes(x = Distance.from.RR,
                 y = y,
                 size = 1,
                 colour = y)) + 
  scale_y_continuous(breaks = c(0, 1)) + 
  ggtitle("Predicted probabilities of y") + 
  xlab("Distance") +
  ylab("Response") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") 

# Generate the ggpredict object
pred_plot <- ggpredict(fit, "X..Pop..Black", ci_level = NA)

# Convert to a data frame for ggplot
pred_data <- as.data.frame(pred_plot)

# Create the ggplot object
p4 <- ggplot(pred_data, aes(x = x, y = predicted)) + 
  geom_line(col = 'purple') + 
  geom_point(data = HaleData,
             aes(x = X..Pop..Black,
                 y = y,
                 size = 1,
                 colour = y)) + 
  scale_y_continuous(breaks = c(0, 1)) + 
  ggtitle("Predicted probabilities of y") + 
  xlab("Black Percentages") +
  ylab("Response") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") 


ggarrange(p3, p4, ncol = 1)
