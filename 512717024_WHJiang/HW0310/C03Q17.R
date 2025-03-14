# ============================================
# C03Q17 Analysis in R
# ============================================

# -----------------------------
# (a) Urban Regression – Test of Slope
# -----------------------------
# Given urban regression:
#   WAGE = -10.76 + 2.46 * EDUC, N = 986
#   Standard error for slope: 0.16
# Test H0: beta2 = 1.80 vs. H1: beta2 > 1.80 at alpha = 0.05

urban_slope <- 2.46
urban_se_slope <- 0.16
n_urban <- 986
df_urban <- n_urban - 2  # degrees of freedom

# Calculate the t-statistic:
t_stat_urban <- (urban_slope - 1.80) / urban_se_slope
cat("Urban regression: t-statistic for slope test =", round(t_stat_urban, 3), "\n")

# For a one-tailed test at alpha=0.05, the critical t-value:
alpha <- 0.05
t_crit_urban <- qt(1 - alpha, df_urban)
cat("Critical t-value (one-tailed, alpha=0.05) =", round(t_crit_urban, 3), "\n")

if (t_stat_urban > t_crit_urban) {
  cat("Conclusion: Reject H0. The urban slope is significantly greater than 1.80.\n\n")
} else {
  cat("Conclusion: Fail to reject H0. The urban slope is not significantly greater than 1.80.\n\n")
}

# Plot the t-distribution and highlight the rejection region:
library(ggplot2)
t_values <- seq(-5, 10, length.out = 1000)
t_density <- dt(t_values, df_urban)
df_plot <- data.frame(t_values = t_values, density = t_density)

ggplot(df_plot, aes(x = t_values, y = density)) +
  geom_line() +
  # 塗上右尾拒絕區域 (t > critical value)
  geom_area(data = subset(df_plot, t_values >= t_crit_urban), aes(y = density),
            fill = "red", alpha = 0.5) +
  # 標示觀察到的 t 值
  geom_vline(xintercept = t_stat_urban, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "t-Distribution for Urban Slope Test",
       subtitle = paste("Observed t =", round(t_stat_urban, 3),
                        "| Critical t =", round(t_crit_urban, 3)),
       x = "t value", y = "Density") +
  theme_minimal()

# -----------------------------
# (b) Rural Regression – 95% CI for Expected WAGE at EDUC = 16
# -----------------------------
# Given rural regression:
#   WAGE = -4.88 + 1.80 * EDUC, N = 214
#   se(intercept) = 3.29, se(slope) = 0.24, covariance = -0.761
# Given: The required standard error for predicting E(WAGE) at EDUC = 16 is 0.833.

rural_intercept <- -4.88
rural_slope <- 1.80
rural_se_intercept <- 3.29
rural_se_slope <- 0.24
cov_rural <- -0.761
educ_val <- 16

# Compute predicted WAGE:
wage_pred_rural <- rural_intercept + rural_slope * educ_val
cat("Rural predicted WAGE at EDUC = 16:", wage_pred_rural, "\n")

# 計算預測標準誤：
# SE(pred) = sqrt( Var(beta1) + (EDUC)^2 * Var(beta2) + 2*EDUC*Cov(beta1, beta2) )
var_intercept_rural <- rural_se_intercept^2
var_slope_rural <- rural_se_slope^2
se_pred_rural_calc <- sqrt(var_intercept_rural + educ_val^2 * var_slope_rural + 2 * educ_val * cov_rural)
cat("Calculated SE for rural prediction =", round(se_pred_rural_calc, 3), "\n")
cat("Given SE for rural prediction = 0.833\n")

# 採用題目給定的 SE = 0.833，構造95%信賴區間：
df_rural <- 214 - 2
t_crit_rural <- qt(0.975, df_rural)  # 雙尾檢定的臨界值
margin_error_rural <- t_crit_rural * 0.833
ci_lower_rural <- wage_pred_rural - margin_error_rural
ci_upper_rural <- wage_pred_rural + margin_error_rural
cat("Rural 95% CI for expected WAGE at EDUC = 16: (", 
    round(ci_lower_rural, 2), ",", round(ci_upper_rural, 2), ")\n\n")

# -----------------------------
# (c) Urban Regression – 95% CI for Expected WAGE at EDUC = 16 and Comparison
# -----------------------------
# Given urban regression:
#   WAGE = -10.76 + 2.46 * EDUC, N = 986
#   se(intercept) = 2.27, se(slope) = 0.16, covariance = -0.345

urban_intercept <- -10.76
urban_slope <- 2.46
urban_se_intercept <- 2.27
urban_se_slope <- 0.16
cov_urban <- -0.345

# Prediction:
wage_pred_urban <- urban_intercept + urban_slope * educ_val
cat("Urban predicted WAGE at EDUC = 16:", wage_pred_urban, "\n")

# 預測標準誤計算：
var_intercept_urban <- urban_se_intercept^2
var_slope_urban <- urban_se_slope^2
se_pred_urban <- sqrt(var_intercept_urban + educ_val^2 * var_slope_urban + 2 * educ_val * cov_urban)
cat("Calculated SE for urban prediction =", round(se_pred_urban, 3), "\n")

# 採用近似標準正態臨界值（由於自由度非常大，df ≈ 984）
margin_error_urban <- 1.96 * se_pred_urban
ci_lower_urban <- wage_pred_urban - margin_error_urban
ci_upper_urban <- wage_pred_urban + margin_error_urban
cat("Urban 95% CI for expected WAGE at EDUC = 16: (", 
    round(ci_lower_urban, 2), ",", round(ci_upper_urban, 2), ")\n")

# 比較兩區間寬度：
width_rural <- ci_upper_rural - ci_lower_rural
width_urban <- ci_upper_urban - ci_lower_urban
cat("Width of rural CI =", round(width_rural, 3), "\n")
cat("Width of urban CI =", round(width_urban, 3), "\n")
if(width_urban < width_rural){
  cat("The urban CI is slightly narrower than the rural CI, which is plausible given the larger sample size in the urban data.\n\n")
} else {
  cat("The urban CI is wider than the rural CI.\n\n")
}

# -----------------------------
# (d) Rural Regression – Test of Intercept
# -----------------------------
# For the rural regression, test:
# H0: beta1 >= 4  vs.  H1: beta1 < 4 at alpha = 0.01.
# Given: rural intercept = -4.88, se(intercept) = 3.29, df = 214 - 2 = 212.

t_stat_intercept_rural <- (rural_intercept - 4) / rural_se_intercept
cat("t-statistic for rural intercept test =", round(t_stat_intercept_rural, 3), "\n")

# For a one-tailed test (left tail) at the 1% significance level:
alpha_int <- 0.01
df_rural <- 214 - 2
t_crit_intercept_rural <- qt(alpha_int, df_rural)  # lower tail critical value
cat("Critical t-value for intercept test (alpha = 0.01) =", round(t_crit_intercept_rural, 3), "\n")

if(t_stat_intercept_rural < t_crit_intercept_rural) {
  cat("Conclusion: Reject H0. There is sufficient evidence that the rural intercept is less than 4 at the 1% level.\n")
} else {
  cat("Conclusion: Fail to reject H0. There is insufficient evidence that the rural intercept is less than 4.\n")
}
