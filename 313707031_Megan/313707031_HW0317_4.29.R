library(POE5Rdata)
data("cex5_small")

install.packages("tseries")
library(tseries)

#4.29(a)
food_mean <- mean(cex5_small$food)
food_median <- median(cex5_small$food)
food_minimum <- min(cex5_small$food)
food_maximum <- max(cex5_small$food)
food_standard_deviation <- sd(cex5_small$food)

income_mean <- mean(cex5_small$income)
income_median <- median(cex5_small$income)
income_minimum <- min(cex5_small$income)
income_maximum <- max(cex5_small$income)
income_standard_deviation <- sd(cex5_small$income)

cat("FOOD summary:\n")
cat("Mean: ", food_mean, "\n")
cat("Median: ", food_median, "\n")
cat("Min: ", food_minimum, "\n")
cat("Max: ", food_maximum, "\n")
cat("Standard Deviation: ", food_standard_deviation, "\n\n")

cat("INCOME summary:\n")
cat("Mean: ", income_mean, "\n")
cat("Median: ", income_median, "\n")
cat("Min: ", income_minimum, "\n")
cat("Max: ", income_maximum, "\n")
cat("Standard Deviation: ", income_standard_deviation, "\n\n")

par(mfrow = c(1, 2))  # 設定畫面為 1x2 格局
hist(cex5_small$food, main = "Histogram of FOOD", xlab = "FOOD", col = "pink", border = "black")
abline(v = food_mean, col = "orange", lwd = 3, lty = 2.5)
abline(v = food_median, col = "lightblue", lwd = 3, lty = 2.5)
hist(cex5_small$income, main = "Histogram of INCOME", xlab = "INCOME", col = "purple", border = "black")
abline(v = income_mean, col = "orange", lwd = 3, lty = 2.5)
abline(v = income_median, col = "lightblue", lwd = 3, lty = 2.5)

print(jarque.bera.test(cex5_small$food))
print(jarque.bera.test(cex5_small$income))

#4.29(b)
model_linear <- lm(food ~ income, data = cex5_small)
summary(model_linear)

plot(cex5_small$income, cex5_small$food,
     main = "FOOD vs INCOME (Linear Model)",
     xlab = "INCOME",
     ylab = "FOOD",
     pch = 16,
     col = "purple")
abline(model_linear, col = "orange", lwd = 2)

confint(model_linear, level = 0.95)

#4.29(c)
residuals_linear <- resid(model_linear)

plot(cex5_small$income, residuals_linear,
     main = "Residuals vs INCOME (Linear Model)",
     xlab = "INCOME",
     ylab = "Residuals",
     pch = 16,
     col = "purple")
abline(h = 0, col = "orange", lwd = 2, lty = 2)

hist(residuals_linear,
     main = "Histogram of Residuals (Linear Model)",
     xlab = "Residuals",
     col = "purple",
     border = "black")
abline(v = 0, col = "orange", lwd = 2, lty = 2)

print(jarque.bera.test(residuals_linear))

#4.29(d)
income_values <- c(19, 65, 160)
predicted_food <- predict(model_linear, newdata = data.frame(income = income_values))
predicted_food

beta2 <- coef(model_linear)[2]
elasticity <- beta2 * (income_values / predicted_food)
elasticity

confint_beta2 <- confint(model_linear)["income", ]
elasticity_lower <- confint_beta2[1] * (income_values / predicted_food)
elasticity_upper <- confint_beta2[2] * (income_values / predicted_food)

elasticity_table <- data.frame(
  INCOME = income_values,
  FOOD_Predicted = round(predicted_food, 2),
  Elasticity = round(elasticity, 4),
  Lower_95_CI = round(elasticity_lower, 4),
  Upper_95_CI = round(elasticity_upper, 4)
)
elasticity_table

#4.29(e)
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)
model_loglog <- lm(log_food ~ log_income, data = cex5_small)
summary(model_loglog)

plot(cex5_small$log_income, cex5_small$log_food,
     main = "log(FOOD) vs log(INCOME) (Log-Log Model)",
     xlab = "log(INCOME)",
     ylab = "log(FOOD)",
     pch = 16,
     col = "purple")
abline(model_loglog, col = "orange", lwd = 2)

r2_loglog <- summary(model_loglog)$r.squared
r2_linear <- summary(model_linear)$r.squared

cat("R-squared of Linear Model: ", r2_linear, "\n")
cat("R-squared of Log-Log Model: ", r2_loglog, "\n")

#4.29(f)
confint(model_loglog, level = 0.95)

#4.29(g)
residuals_loglog <- resid(model_loglog)

par(mfrow = c(1, 2))
plot(cex5_small$log_income, residuals_loglog,
     main = "Residuals vs log(INCOME) (Log-Log Model)",
     xlab = "log(INCOME)",
     ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "orange", lwd = 2)

hist(residuals_loglog,
     main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals",
     col = "purple",
     breaks = 20)

print(jarque.bera.test(residuals_loglog))

#4.29(h)
cex5_small$log_income <- log(cex5_small$income)
model_linear_log <- lm(food ~ log_income, data = cex5_small)
summary(model_linear_log)

plot(cex5_small$log_income, cex5_small$food,
     main = "FOOD vs log(INCOME) (Linear Model)",
     xlab = "log(INCOME)",
     ylab = "FOOD",
     pch = 16,
     col = "purple")
abline(model_linear_log, col = "orange", lwd = 2)

r2_linear_log <- summary(model_linear_log)$r.squared
cat("R-squared of Linear Model with log(INCOME): ", r2_linear_log, "\n")
cat("R-squared of Log-Log Model: ", r2_loglog, "\n")

#4.29(i)
income_values <- c(19, 65, 160)
log_income_values <- log(income_values)
predicted_logfood <- predict(model_linear_log, newdata = data.frame(log_income = log_income_values))
predicted_logfood

logbeta2 <- coef(model_linear_log)[2]
logelasticity <- logbeta2 / predicted_logfood
logelasticity

confint_logbeta2 <- confint(model_linear_log)["log_income", ]
elasticity_lower_log <- confint_logbeta2[1] / predicted_logfood
elasticity_upper_log <- confint_logbeta2[2] / predicted_logfood

logelasticity_table <- data.frame(
  INCOME = income_values,
  FOOD_Predicted = round(predicted_logfood, 2),
  Elasticity = round(logelasticity, 4),
  Lower_95_CI = round(elasticity_lower_log, 4),
  Upper_95_CI = round(elasticity_upper_log, 4)
)
logelasticity_table

#4.29(j)
residuals_linearlog <- resid(model_linear_log)

par(mfrow = c(1, 2))
plot(cex5_small$log_income, residuals_linearlog,
     main = "Residuals vs log(INCOME) (Linear-Log Model)",
     xlab = "log(INCOME)",
     ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "orange", lwd = 2)

hist(residuals_linearlog,
     main = "Histogram of Residuals (Linear-Log Model)",
     xlab = "Residuals",
     col = "purple",
     breaks = 20)

print(jarque.bera.test(residuals_linearlog))
