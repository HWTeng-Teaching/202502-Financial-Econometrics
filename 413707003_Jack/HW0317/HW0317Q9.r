# 4.29
# i

alpha2 <- coef(linear_log_model)["log(income)"]
se_alpha2 <- summary(linear_log_model)$coefficients["log(income)", "Std. Error"]

income_values <- c(19, 65, 160)
food_preds <- predict(linear_log_model, newdata = data.frame(income = income_vals))
elasticities <- alpha2 / food_preds
se_elasticities <- se_alpha2 / food_preds

lower_bound <- elasticities - 1.96 * se_elasticities
upper_bound <- elasticities + 1.96 * se_elasticities


elasticity_results <- data.frame(
  INCOME = income_vals,
  Fitted_Point = food_preds,
  Elasticity = elasticities,
  Lower_95_CI = lower_bound,
  Upper_95_CI = upper_bound
)


print(elasticity_results)
 # INCOME Fitted_Point Elasticity Lower_95_CI Upper_95_CI
 #     19     88.89788  0.2495828   0.1784715   0.3206942
 #     65    116.18722  0.1909624   0.1365532   0.2453715
 #    160    136.17332  0.1629349   0.1165113   0.2093585


# It is dissimilar from the other models because it has a significant difference in elasticity and confidence intervals compared to the previous linear model.


# j

residuals_linear_log <- residuals(linear_log_model)

plot(log(cex5_small$income), residuals_linear_log, main = "Residuals vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "Residuals", pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

hist(residuals_linear_log, main = "Histogram of Residuals (Linear-Log model)",
     xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_linear_log), col = "red", lwd = 2)

# As ln(INCOME) increases, the residuals also increase.
# The p-value is less than 0.05, so we reject the null hypothesis, indicating that the regression errors are not normally distributed.

jb_test_linear_log <- jarque.test(residuals_linear_log)
print(jb_test_linear_log)
# data:  residuals_linear_log
# JB = 628.07, p-value < 2.2e-16
# alternative hypothesis: greater


# k

# I prefer the log-log model because its residuals are the closest to a normal distribution among the three models.


