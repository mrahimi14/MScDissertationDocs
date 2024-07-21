# Clearing the Environment:
rm(list = ls())
if(!is.null(dev.list())) dev.off()

# Loading the required packages -------------------------------------------

pkg <- c('tidyverse',
         'scatterplot3d')

for(i in 1:length(pkg)){
  if(!(pkg[i] %in% installed.packages())) {
    install.packages(pkg[i])
  }
  library(pkg[i], character.only = TRUE)
}


# Chapter-3 Codes --------------------------------------------------------


# Curse_Of_Dimensionality_Plot ------------------------------------------------

# Setting a seed for reproducibility:
set.seed(2003)

# Splitting the Layout:
layout(matrix(c(1, 1, 1, 1, 2, 2, 3, 3, 2, 2, 3, 3), nrow = 3, byrow = TRUE))
# layout(matrix(1))

# 1-Dimension
df1 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = rep(0.5, 9),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df1 <- df1 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

# win.graph(14, 5)
with(df1,
     plot(y ~ x, 
          col = g, 
          pch = g,
          cex = 4,
          xlim = c(0, 3),
          xlab = "",
          ylab = "",
          main = "داده در یک بعد (الف)"))
abline(v = c(1, 2), col = 'red', lt = 2, lw = 2)


# 2-Dimension
df2 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df2 <- df2 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

with(df2,
     plot(y ~ x, 
          col = g, 
          pch = g,
          cex = 3,
          xlim = c(0, 3),
          ylim = c(0, 3),
          xlab = "",
          ylab = "",
          main = "داده در دو بعد (ب)"))
abline(v = c(1, 2), col = 'red', lt = 2, lw = 2)
abline(h = c(1, 2), col = 'red', lt = 2, lw = 2)


# 3-Dimension
df3 <- data.frame(
  x = c(runif(2, 0, 1.2), runif(4, 0.9, 2.2), runif(3, 1.8, 3)),
  y = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  z = c(runif(3, 1.8, 3), runif(4, 0.9, 2.2), runif(2, 0, 1.2)),
  g = rep(c(2, 4, 3), c(2, 4, 3))
)

df3 <- df3 %>%
  mutate(g = recode(g, `2` = 19, `3` = 15, `4` = 17))

plt <- with(df3, 
            scatterplot3d(x, y, z,
                          color = g,
                          pch = g,
                          angle = 200,
                          cex.symbols = 3,
                          zlim = c(0,3),
                          xlab = "",
                          ylab = "",
                          zlab = "",
                          main= "داده در سه بعد (ج)"))

plt$plane3d(2, 0, 0, col = 2, lw = 1.5)
plt$plane3d(1, 0, 0, col = 2, lw = 1.5)
