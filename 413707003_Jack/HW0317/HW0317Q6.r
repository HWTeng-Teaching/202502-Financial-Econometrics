# 4.29
# c

residuals_lm <- resid(lm_model)

ggplot(cex5_small, aes(x = income, y = residuals_lm)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs INCOME", x = "INCOME", y = "Residuals")

# The residuals increase as income increases.

par(mfrow = c(1, 1))
hist(residuals_lm, breaks = 30, col = "lightblue", main = "Residuals Histogram", probability = TRUE)
abline(v = mean(residuals_lm, na.rm = TRUE), col = "red", lwd = 2, lty = 2)

jarque.test(residuals_lm)

# Jarque-Bera Normality Test
# 
# data:  residuals_lm
# JB = 624.19, p-value < 2.2e-16
# alternative hypothesis: greater

# I believe that the random error being normally distributed is more important.


# d

beta2 <- coef(lm_model)["income"]
model_summary <- summary(lm_model)
se_beta2 <- model_summary$coefficients["income", "Std. Error"]

income_vals <- c(19, 65, 160)
food_preds <- predict(lm_model, newdata = data.frame(income = income_vals))

elasticity_linear <- beta2 * income_vals / food_preds
se_elasticity_linear <- se_beta2 * income_vals / food_preds

lower_ci_linear <- elasticity_linear - 1.96 * se_elasticity_linear
upper_ci_linear <- elasticity_linear + 1.96 * se_elasticity_linear

elasticity_results_linear <- data.frame(
  INCOME = income_vals,
  Fitted_Point = food_preds,
  Elasticity = elasticity_linear,
  Lower_95_CI = lower_ci_linear,
  Upper_95_CI = upper_ci_linear
)

print(elasticity_results_linear)
#   INCOME Fitted_Point Elasticity Lower_95_CI Upper_95_CI
# 1     19     95.38155 0.07145038  0.05219387  0.09070689
# 2     65    111.88114 0.20838756  0.15222527  0.26454986
# 3    160    145.95638 0.39319883  0.28722827  0.49916940

# The estimated elasticities are dissimilar, as the confidence intervals for the three models do not overlap. 
# I believe the elasticity initially increases with income but then decreases after reaching a certain income level. At higher income levels, food expenditure no longer increases significantly.

