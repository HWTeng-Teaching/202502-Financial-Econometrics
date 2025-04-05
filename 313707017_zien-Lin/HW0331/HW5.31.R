library(POE5Rdata)
data("commute5")
# ------------a.各係數
# Fit the multiple linear regression model
model <- lm(time ~ depart + reds + trains, data = commute5)

# Display the summary of the regression model to get the coefficients
summary(model)

#----------------b.
# Compute the 95% confidence intervals for the coefficients
conf_intervals <- confint(model, level = 0.95)

# Display the confidence intervals
print(conf_intervals)

#---------------c.
# Extract the coefficient and standard error for REDS
coef_summary <- summary(model)$coefficients
beta_3 <- coef_summary["reds", "Estimate"]
se_beta_3 <- coef_summary["reds", "Std. Error"]

# Calculate the t-statistic for H0: beta_3 = 2
t_stat <- (beta_3 - 2) / se_beta_3

# Degrees of freedom (n - number of predictors - 1)
n <- nrow(commute5)  # Number of observations (249)
k <- 3  # Number of predictors (DEPART, REDS, TRAINS)
df <- n - k - 1  # Degrees of freedom = 249 - 3 - 1 = 245

# Calculate the p-value for a one-sided test (H1: beta_3 < 2)
p_value <- pt(t_stat, df, lower.tail = TRUE)

# Print the t-statistic and p-value
cat("t-statistic:", t_stat, "\n")
cat("p-value (one-sided):", p_value, "\n")

# Decision at 5% significance level
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis at the 5% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at the 5% significance level.\n")
}

#------------------------d.
# Extract the coefficient and standard error for trains
coef_summary <- summary(model)$coefficients
beta_4 <- coef_summary["trains", "Estimate"]
se_beta_4 <- coef_summary["trains", "Std. Error"]

# Calculate the t-statistic for H0: beta_4 = 3
t_stat <- (beta_4 - 3) / se_beta_4

# Degrees of freedom (n - number of predictors - 1)
n <- nrow(commute5)  # Number of observations (249)
k <- 3  # Number of predictors (DEPART, REDS, TRAINS)
df <- n - k - 1  # Degrees of freedom = 249 - 3 - 1 = 245

# 雙尾
two_tail_p_value <- 2 * pt(-abs(t_stat), df, lower.tail = TRUE)

# Print the t-statistic and p-value
cat("t-statistic:", t_stat, "\n")
cat("p-value (one-sided):", two_tail_p_value, "\n")

# Decision at 10% significance level
alpha <- 0.10
if (two_tail_p_value < alpha) {
  cat("Reject the null hypothesis at the 10% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at the 10% significance level.\n")
}

#----------------------e.
coef_summary <- summary(model)$coefficients
beta_2 <- coef_summary["depart", "Estimate"]
se_beta_2 <- coef_summary["depart", "Std. Error"]

# Calculate the t-statistic for H0: beta_2 = 1/3
null_value <- 1/3  # 10 minutes / 30 minutes = 1/3
t_stat <- (beta_2 - null_value) / se_beta_2

# Degrees of freedom (n - number of predictors - 1)
n <- nrow(commute5)  # Number of observations (249)
k <- 3  # Number of predictors (DEPART, REDS, TRAINS)
df <- n - k - 1  # Degrees of freedom = 249 - 3 - 1 = 245

# Calculate the p-value for a one-sided test (H1: beta_2 < 1/3)  右尾的話是lower.tail = false
p_value <- pt(t_stat, df, lower.tail = TRUE)

# Print the t-statistic and p-value
cat("t-statistic:", t_stat, "\n")
cat("p-value (one-sided):", p_value, "\n")

# Decision at 5% significance level
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis at the 5% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at the 5% significance level.\n")
}

#--------------------------f.
# Extract the coefficients and their variance-covariance matrix
coef_summary <- summary(model)$coefficients
beta_3 <- coef_summary["reds", "Estimate"]
beta_4 <- coef_summary["trains", "Estimate"]
vcov_matrix <- vcov(model)  # Variance-covariance matrix of the coefficients

# Compute the linear combination: beta_4 - 3 * beta_3
linear_comb <- beta_4 - 3 * beta_3

# Compute the standard error of the linear combination beta_4 - 3 * beta_3
# Var(beta_4 - 3 * beta_3) = Var(beta_4) + 9 * Var(beta_3) - 2 * 3 * Cov(beta_3, beta_4)
var_beta_3 <- vcov_matrix["reds", "reds"]
var_beta_4 <- vcov_matrix["trains", "trains"]
cov_beta_3_beta_4 <- vcov_matrix["reds", "trains"]
var_linear_comb <- var_beta_4 + 9 * var_beta_3 - 2 * 3 * cov_beta_3_beta_4
se_linear_comb <- sqrt(var_linear_comb)

# Calculate the t-statistic for H0: beta_4 - 3 * beta_3 = 0
t_stat <- (linear_comb - 0) / se_linear_comb

# Degrees of freedom (n - number of predictors - 1)
n <- nrow(commute5)  # Number of observations (249)
k <- 3  # Number of predictors (DEPART, REDS, TRAINS)
df <- n - k - 1  # Degrees of freedom = 249 - 3 - 1 = 245

# Calculate the p-value for a one-sided test (H1: beta_4 - 3 * beta_3 < 0)
p_value <- pt(t_stat, df, lower.tail = TRUE)

# Print the t-statistic and p-value
cat("Linear combination (beta_4 - 3 * beta_3):", linear_comb, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value (left-tailed):", p_value, "\n")

# Decision at 5% significance level
alpha <- 0.05
if (p_value < alpha) {
  cat("Reject the null hypothesis at the 5% significance level.\n")
} else {
  cat("Fail to reject the null hypothesis at the 5% significance level.\n")
}

#-----------------------g.
# 載入所需的套件和數據集（如果尚未載入）
library(POE5Rdata)
data("commute5")

# 擬合多重線性回歸模型（如果尚未完成）
model <- lm(time ~ depart + reds + trains, data = commute5)

# 提取係數及其變異數-共變異數矩陣
coef_summary <- summary(model)$coefficients
beta_1 <- coef_summary["(Intercept)", "Estimate"]
beta_2 <- coef_summary["depart", "Estimate"]
beta_3 <- coef_summary["reds", "Estimate"]
beta_4 <- coef_summary["trains", "Estimate"]
vcov_matrix <- vcov(model)  # 係數的變異數-共變異數矩陣

# 計算 E(TIME|X) = beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4
expected_time <- beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4

# 計算線性組合 beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4 的標準誤
# Var(beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4) = Var(beta_1) + 900 * Var(beta_2) + 36 * Var(beta_3) + Var(beta_4)
# + 2 * (30 * Cov(beta_1, beta_2) + 6 * Cov(beta_1, beta_3) + Cov(beta_1, beta_4)
# + 30 * 6 * Cov(beta_2, beta_3) + 30 * Cov(beta_2, beta_4) + 6 * Cov(beta_3, beta_4))
var_beta_1 <- vcov_matrix["(Intercept)", "(Intercept)"]
var_beta_2 <- vcov_matrix["depart", "depart"]
var_beta_3 <- vcov_matrix["reds", "reds"]
var_beta_4 <- vcov_matrix["trains", "trains"]
cov_beta_1_beta_2 <- vcov_matrix["(Intercept)", "depart"]
cov_beta_1_beta_3 <- vcov_matrix["(Intercept)", "reds"]
cov_beta_1_beta_4 <- vcov_matrix["(Intercept)", "trains"]
cov_beta_2_beta_3 <- vcov_matrix["depart", "reds"]
cov_beta_2_beta_4 <- vcov_matrix["depart", "trains"]
cov_beta_3_beta_4 <- vcov_matrix["reds", "trains"]

var_linear_comb <- (var_beta_1 + 900 * var_beta_2 + 36 * var_beta_3 + var_beta_4 +
                      2 * (30 * cov_beta_1_beta_2 + 6 * cov_beta_1_beta_3 + cov_beta_1_beta_4 +
                             30 * 6 * cov_beta_2_beta_3 + 30 * cov_beta_2_beta_4 + 6 * cov_beta_3_beta_4))
se_linear_comb <- sqrt(var_linear_comb)

# 計算 H0: beta_1 + 30 * beta_2 + 6 * beta_3 + beta_4 = 45 的 t 統計量
t_stat <- (expected_time - 45) / se_linear_comb

# 自由度 (n - 預測變量數 - 1)
n <- nrow(commute5)  # 觀測值數量 (249)
k <- 3  # 預測變量數 (DEPART, REDS, TRAINS)
df <- n - k - 1  # 自由度 = 249 - 3 - 1 = 245

# 計算單尾檢驗的 p 值 (H1: E(TIME|X) > 45)
p_value <- pt(t_stat, df, lower.tail = FALSE)

# 輸出結果
cat("預期通勤時間 E(TIME|X):", expected_time, "分鐘\n")
cat("t 統計量:", t_stat, "\n")
cat("p 值 (右尾):", p_value, "\n")

# 在 5% 顯著性水平下做出決策
alpha <- 0.05
if (p_value < alpha) {
  cat("在 5% 顯著性水平下拒絕原假設。\n")
} else {
  cat("在 5% 顯著性水平下無法拒絕原假設。\n")
}

