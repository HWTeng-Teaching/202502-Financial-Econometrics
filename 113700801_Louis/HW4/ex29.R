# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(MASS)

# Load the dataset
data <- read.table("FoodExp.txt", header = TRUE)

# Display basic information about the dataset
str(data)
summary(data)

# (4.29a) Descriptive statistics
mean_food <- mean(data$food)
mean_income <- mean(data$income)
var_food <- var(data$food)
var_income <- var(data$income)
cor_food_income <- cor(data$food, data$income)

cat("Mean food expenditure:", mean_food, "\n")
cat("Mean income:", mean_income, "\n")
cat("Variance of food expenditure:", var_food, "\n")
cat("Variance of income:", var_income, "\n")
cat("Correlation between food expenditure and income:", cor_food_income, "\n")

# (4.29b) Linear regression model
lm_model <- lm(food ~ income, data = data)
summary(lm_model)

# (4.29c) Residual analysis
par(mfrow = c(2, 2))
plot(lm_model)
par(mfrow = c(1, 1))

# (4.29d) Confidence interval for beta_1
confint(lm_model, level = 0.95)

# (4.29e) Log-log model
lm_log_model <- lm(log(food) ~ log(income), data = data)
summary(lm_log_model)

# (4.29f) Confidence interval for beta_1 in log-log model
confint(lm_log_model, level = 0.95)

# (4.29g) Residual analysis for log-log model
par(mfrow = c(2, 2))
plot(lm_log_model)
par(mfrow = c(1, 1))

# (4.29h) Comparison of models using adjusted R-squared
adj_r_squared_lm <- summary(lm_model)$adj.r.squared
adj_r_squared_log_lm <- summary(lm_log_model)$adj.r.squared

cat("Adjusted R-squared for linear model:", adj_r_squared_lm, "\n")
cat("Adjusted R-squared for log-log model:", adj_r_squared_log_lm, "\n")

# (4.29i) Confidence interval for elasticity (log-log model)
elasticity <- coef(lm_log_model)[2]
confint_elasticity <- confint(lm_log_model, level = 0.95)[2,]

cat("Estimated elasticity:", elasticity, "\n")
cat("95% Confidence Interval for elasticity:", confint_elasticity, "\n")

# (4.29j) Additional diagnostic tests
bptest(lm_model)  # Breusch-Pagan test for heteroskedasticity
bptest(lm_log_model)  # Breusch-Pagan test for log-log model

# Box-Cox transformation to check potential improvements
boxcox(lm_model, lambda = seq(-2, 2, by = 0.1))
