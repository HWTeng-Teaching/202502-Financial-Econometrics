# 清除環境變數（可選）
rm(list=ls())

# 安裝並載入所需套件
#nstall.packages("tseries")  # 如果尚未安裝 tseries，取消註解此行
library(POE5Rdata)
library(dplyr)
library(ggplot2)
library(tseries)  # 用於 Jarque-Bera 檢驗

# 載入資料
data("cex5_small")

# 篩選三人家庭（count == 3）
cex <- cex <- cex5_small[,c(6,9)]

# a. 描述性統計與分佈分析
# 計算描述性統計
cat("\nSummary Statistics for FOOD and INCOME:\n")
summary(cex)
cat("\nStandard Deviation for FOOD and INCOME:\n")
sapply(cex, sd)

# 繪製直方圖並標記均值和中位數
p1 <- ggplot(cex, aes(x = food)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(food)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(food)), color = "blue", linetype = "dotted", size = 1) +
  labs(title = "Histogram of FOOD", x = "Food Expenditure", y = "Density") +
  theme_minimal() +
  annotate("text", x = mean(cex$food), y = 0.002, label = "Mean", color = "red", vjust = -1) +
  annotate("text", x = median(cex$food), y = 0.002, label = "Median", color = "blue", vjust = 1)

p2 <- ggplot(cex, aes(x = income)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgreen", color = "black") +
  geom_vline(aes(xintercept = mean(income)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(income)), color = "blue", linetype = "dotted", size = 1) +
  labs(title = "Histogram of INCOME", x = "Income", y = "Density") +
  theme_minimal() +
  annotate("text", x = mean(cex$income), y = 0.002, label = "Mean", color = "red", vjust = -1) +
  annotate("text", x = median(cex$income), y = 0.002, label = "Median", color = "blue", vjust = 1)

print(p1)
print(p2)

# Jarque-Bera 檢驗
cat("\nJarque-Bera Test for FOOD:\n")
print(jarque.bera.test(cex$food))
cat("\nJarque-Bera Test for INCOME:\n")
print(jarque.bera.test(cex$income))

# b. 估計線性模型並繪製散點圖
model_linear <- lm(food ~ income, data = cex)
summary(model_linear)

confint_beta2 <- confint(model_linear, "income", level = 0.95)
cat("\n95% Confidence Interval for beta_2:\n")
print(confint_beta2)

p3 <- ggplot(cex, aes(x = income, y = food)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of FOOD vs INCOME with Fitted Line",
       x = "Income", y = "Food Expenditure") +
  theme_minimal()

print(p3)

# c. 殘差分析
cex <- cex %>%
  mutate(resid_linear = resid(model_linear))

p4 <- ggplot(cex, aes(x = income, y = resid_linear)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs INCOME (Linear Model)", x = "Income", y = "Residuals") +
  theme_minimal()

print(p4)

p4_hist <- ggplot(cex, aes(x = resid_linear)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals (Linear Model)", x = "Residuals", y = "Density") +
  theme_minimal()

print(p4_hist)

cat("\nJarque-Bera Test for Residuals (Linear Model):\n")
print(jarque.bera.test(cex$resid_linear))

cat("\nDiscussion on Normality:\n")
cat("It is more important for the random error (residuals) to be normally distributed than for FOOD and INCOME to be normally distributed. This is because the least squares estimation assumes that the errors are normally distributed for valid inference (e.g., confidence intervals, hypothesis tests). If the residuals are not normally distributed, the statistical tests may be unreliable. However, FOOD and INCOME themselves do not need to be normally distributed, as the model focuses on the relationship between them, not their marginal distributions.\n")

# d. 點估計與收入彈性
new_data <- data.frame(income = c(19, 65, 160))
pred_intervals <- predict(model_linear, newdata = new_data, interval = "confidence", level = 0.95)

# 點估計
point_estimates <- pred_intervals[, "fit"]

# 計算彈性
beta_2 <- coef(model_linear)["income"]
elasticity_linear <- beta_2 * new_data$income / point_estimates

# 計算彈性的標準誤和信賴區間（使用 Delta 方法）
# 彈性為 beta_2 * (INCOME / FOOD)，其中 FOOD = beta_1 + beta_2 * INCOME
# 我們需要 beta_2 的標準誤
se_beta_2 <- summary(model_linear)$coefficients["income", "Std. Error"]

# 對於每個 INCOME，計算彈性的標準誤
# 彈性的標準誤近似為：se(epsilon) = (INCOME / FOOD) * se(beta_2)
se_elasticity <- (new_data$income / point_estimates) * se_beta_2

# 計算 95% 信賴區間（假設彈性近似正態分佈）
z <- qnorm(0.975)  # 95% 信賴區間的 z 值
elasticity_lb <- elasticity_linear - z * se_elasticity
elasticity_ub <- elasticity_linear + z * se_elasticity

# 創建表格
results_table <- data.frame(
  INCOME = new_data$income,
  Point_Estimate = point_estimates,
  Elasticity = elasticity_linear,
  SE_Elasticity = se_elasticity,
  LB = elasticity_lb,
  UB = elasticity_ub
)

# 格式化表格，保留小數點後 4 位
results_table$Point_Estimate <- round(results_table$Point_Estimate, 4)
results_table$Elasticity <- round(results_table$Elasticity, 4)
results_table$SE_Elasticity <- round(results_table$SE_Elasticity, 4)
results_table$LB <- round(results_table$LB, 4)
results_table$UB <- round(results_table$UB, 4)

# 重新命名列名以匹配表格
colnames(results_table) <- c("INCOME", "b_1 + b_2*INCOME", "ε", "se(ε)", "LB", "UB")

# 輸出表格
cat("\nPoint Estimates and Elasticity with 95% Confidence Intervals:\n")
print(results_table)

# e. 估計對數-對數模型並比較
model_loglog <- lm(log(food) ~ log(income), data = cex)
summary(model_loglog)

p5 <- ggplot(cex, aes(x = log(income), y = log(food))) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of ln(FOOD) vs ln(INCOME) with Fitted Line",
       x = "ln(Income)", y = "ln(Food Expenditure)") +
  theme_minimal()

print(p5)

# f. 對數-對數模型的進一步分析
beta0_hat2 <- coef(model_loglog)[1]
beta1_hat2 <- coef(model_loglog)[2]
beta1_CI_loglog <- confint(model_loglog, level = 0.95)[2,]

cat("Point Estimate of Elasticity:", beta1_hat2, "\n")
cat("95% Confidence Interval for Elasticity: (", beta1_CI_loglog[1], ",", beta1_CI_loglog[2], ")\n")


# g. 對數-對數模型的殘差分析
cex <- cex %>%
  mutate(resid_loglog = resid(model_loglog))

p6 <- ggplot(cex, aes(x = log(income), y = resid_loglog)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) (Log-Log Model)", x = "ln(Income)", y = "Residuals") +
  theme_minimal()

print(p6)

p6_hist <- ggplot(cex, aes(x = resid_loglog)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals (Log-Log Model)", x = "Residuals", y = "Density") +
  theme_minimal()

print(p6_hist)

cat("\nJarque-Bera Test for Residuals (Log-Log Model):\n")
print(jarque.bera.test(cex$resid_loglog))

# h. 估計線性-對數模型
model_linearlog <- lm(food ~ log(income), data = cex)
summary(model_linearlog)

p7 <- ggplot(cex, aes(x = log(income), y = food)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of FOOD vs ln(INCOME) with Fitted Line",
       x = "ln(Income)", y = "Food Expenditure") +
  theme_minimal()

print(p7)

cat("\nComparison of Plots (Linear, Log-Log, Linear-Log):\n")
cat("The linear-log model may better capture the relationship if the scatter plot of FOOD vs ln(INCOME) shows a more linear pattern compared to FOOD vs INCOME (linear model) or ln(FOOD) vs ln(INCOME) (log-log model).\n")

cat("\nR-squared for Linear-Log Model:\n")
print(summary(model_linearlog)$r.squared)

# i. 線性-對數模型的彈性
new_data <- data.frame(income = c(19, 65, 160))
pred_intervals <- predict(model_linearlog, newdata = new_data, interval = "confidence", level = 0.95)

# 點估計
point_estimates <- pred_intervals[, "fit"]

# 計算彈性
alpha_2 <- coef(model_linearlog)["log(income)"]
elasticity_linearlog <- alpha_2 / point_estimates

# 計算彈性的標準誤和信賴區間（使用 Delta 方法）
# 彈性為 alpha_2 / FOOD，其中 FOOD = alpha_1 + alpha_2 * ln(INCOME)
# 我們需要 alpha_2 的標準誤
se_alpha_2 <- summary(model_linearlog)$coefficients["log(income)", "Std. Error"]

# 對於每個 INCOME，計算彈性的標準誤
# 彈性的標準誤近似為：se(epsilon) = (1 / FOOD) * se(alpha_2)
se_elasticity_linearlog <- (1 / point_estimates) * se_alpha_2

# 計算 95% 信賴區間
elasticity_linearlog_lb <- elasticity_linearlog - z * se_elasticity_linearlog
elasticity_linearlog_ub <- elasticity_linearlog + z * se_elasticity_linearlog

# 創建表格
results_table_linearlog <- data.frame(
  INCOME = new_data$income,
  Point_Estimate = point_estimates,
  Elasticity = elasticity_linearlog,
  SE_Elasticity = se_elasticity_linearlog,
  LB = elasticity_linearlog_lb,
  UB = elasticity_linearlog_ub
)

# 格式化表格，保留小數點後 4 位
results_table_linearlog$Point_Estimate <- round(results_table_linearlog$Point_Estimate, 4)
results_table_linearlog$Elasticity <- round(results_table_linearlog$Elasticity, 4)
results_table_linearlog$SE_Elasticity <- round(results_table_linearlog$SE_Elasticity, 4)
results_table_linearlog$LB <- round(results_table_linearlog$LB, 4)
results_table_linearlog$UB <- round(results_table_linearlog$UB, 4)

# 重新命名列名以匹配表格
colnames(results_table_linearlog) <- c("INCOME", "α_1 + α_2*ln(INCOME)", "ε", "se(ε)", "LB", "UB")

# 輸出表格
cat("\nPoint and 95% Interval Estimate of Elasticity (Linear-Log Model):\n")
print(results_table_linearlog)


# j. 線性-對數模型的殘差分析
cex <- cex %>%
  mutate(resid_linearlog = resid(model_linearlog))

p8 <- ggplot(cex, aes(x = log(income), y = resid_linearlog)) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) (Linear-Log Model)", x = "ln(Income)", y = "Residuals") +
  theme_minimal()

print(p8)

p8_hist <- ggplot(cex, aes(x = resid_linearlog)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals (Linear-Log Model)", x = "Residuals", y = "Density") +
  theme_minimal()

print(p8_hist)

cat("\nJarque-Bera Test for Residuals (Linear-Log Model):\n")
print(jarque.bera.test(cex$resid_linearlog))

cat("\nDiscussion on Normality (Linear-Log Model):\n")
cat("If the Jarque-Bera test p-value is less than 0.05, the residuals are not normally distributed, which may affect the reliability of statistical inference. The residual plot and histogram can help identify patterns, such as heteroskedasticity or non-normality.\n")

# k. 模型選擇
cat("\nModel Selection:\n")
cat("Based on the R-squared values, residual analysis, and economic interpretation:\n")
cat("- The linear model has an R-squared of ", summary(model_linear)$r.squared, ".\n")
cat("- The log-log model has a generalized R-squared of ", r2_generalized_loglog, ".\n")
cat("- The linear-log model has an R-squared of ", summary(model_linearlog)$r.squared, ".\n")
cat("The model with the highest R-squared (or generalized R-squared) fits the data best in terms of explained variance. Additionally, the log-log and linear-log models better align with Engel's Law, as their elasticities are either constant (log-log) or decrease with income (linear-log), unlike the linear model where elasticity increases with income. The residual analysis also helps determine if the model assumptions (e.g., normality, homoskedasticity) are better satisfied. Based on these criteria, the linear-log model is often preferred for food expenditure data, as it balances fit and economic interpretability.\n")
