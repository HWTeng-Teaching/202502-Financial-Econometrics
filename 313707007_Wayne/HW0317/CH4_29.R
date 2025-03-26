#(a)
library(ggplot2)
library(tseries)
library(dplyr)

# 讀取資料
df <- read.csv("cex5_small.csv")

# 確保 food 和 income 是數值型態
df$food <- as.numeric(df$food)
df$income <- as.numeric(df$income)

# 計算基本統計量
summary_stats <- data.frame(
  變數 = c("food", "income"),
  平均數 = c(mean(df$food, na.rm = TRUE), mean(df$income, na.rm = TRUE)),
  中位數 = c(median(df$food, na.rm = TRUE), median(df$income, na.rm = TRUE)),
  最小值 = c(min(df$food, na.rm = TRUE), min(df$income, na.rm = TRUE)),
  最大值 = c(max(df$food, na.rm = TRUE), max(df$income, na.rm = TRUE)),
  標準差 = c(sd(df$food, na.rm = TRUE), sd(df$income, na.rm = TRUE))
)
print(summary_stats)

# 繪製 food 直方圖
ggplot(df, aes(x = food)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(food, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median(food, na.rm = TRUE)), color = "green", linetype = "solid", size = 1.2) +
  labs(title = "Histogram of FOOD", x = "food", y = "Frequency") +
  theme_minimal()

# 繪製 income 直方圖
ggplot(df, aes(x = income)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(income, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median(income, na.rm = TRUE)), color = "green", linetype = "solid", size = 1.2) +
  labs(title = "Histogram of INCOME", x = "income", y = "Frequency") +
  theme_minimal()

# Jarque–Bera 常態性檢定
jb_food <- jarque.bera.test(df$food)
jb_income <- jarque.bera.test(df$income)

cat("FOOD 的 Jarque–Bera 檢定結果：\n")
print(jb_food)
cat("\nINCOME 的 Jarque–Bera 檢定結果：\n")
print(jb_income)

#(b)
model <- lm(food ~ income,data=df)
summary(model)
ggplot(df, aes(x = income, y = food)) +
  geom_point(color = "gray50", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "FOOD vs INCOME 散佈圖與回歸線",
       x = "INCOME（家庭收入）",
       y = "FOOD（食物支出）") +
  theme_minimal()
confint(model, level = 0.95)
#(c)
# 取得殘差
df$residuals <- resid(model)  # model 是之前建立的 lm(food ~ income)

# 殘差對 income 的散佈圖
ggplot(df, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "residual INCOME scatter plot", x = "INCOME", y = "residual") +
  theme_minimal()

# 殘差直方圖
ggplot(df, aes(x = residuals)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "black") +
  geom_vline(aes(xintercept = mean(residuals)), color = "red", linetype = "dashed") +
  labs(title = "residual histogram", x = "Residuals", y = "frequency") +
  theme_minimal()

# Jarque–Bera 常態性檢定
library(tseries)
jarque.bera.test(df$residuals)

#(d)
# 指定要分析的收入水準
income_values <- c(19, 65, 160)

# 取得斜率（β2）與其標準誤
beta2 <- coef(model)["income"]
se_beta2 <- summary(model)$coefficients["income", "Std. Error"]

# 預測 food 的點估計值（fitted values）
fitted_food <- predict(model, newdata = data.frame(income = income_values))

# 計算彈性點估計值：ε = β2 * income / fitted_food
elasticity_point <- beta2 * income_values / fitted_food

# 彈性信賴區間的上下限（只考慮 β2 的不確定性，fitted 值視為非隨機）
# 使用 delta method 的簡化版
upper_beta2 <- beta2 + 1.96 * se_beta2
lower_beta2 <- beta2 - 1.96 * se_beta2
elasticity_upper <- upper_beta2 * income_values / fitted_food
elasticity_lower <- lower_beta2 * income_values / fitted_food

# 整理成表格
elasticity_table <- data.frame(
  INCOME = income_values,
  Fitted_FOOD = round(fitted_food, 2),
  Elasticity = round(elasticity_point, 3),
  Lower_95_CI = round(elasticity_lower, 3),
  Upper_95_CI = round(elasticity_upper, 3)
)

elasticity_table
#(e)
# 建立 log-log 變數
df$log_food <- log(df$food)
df$log_income <- log(df$income)

# 建立 log-log 模型
loglog_model <- lm(log_food ~ log_income, data = df)

# 檢視模型摘要
summary(loglog_model)

# 繪製 ln(FOOD) vs ln(INCOME) 散佈圖與回歸線
library(ggplot2)
ggplot(df, aes(x = log_income, y = log_food)) +
  geom_point(alpha = 0.5, color = "darkgray") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "log(FOOD) vs log(INCOME)",
       x = "log(INCOME)", y = "log(FOOD)") +
  theme_minimal()

# Generalized R² 計算（與 linear R² 比較）
# 原始線性模型與 log-log 模型
linear_model <- lm(food ~ income, data = df)

# Generalized R² = 1 - exp((RSS_loglog - RSS_null)/n)
rss_loglog <- sum(resid(loglog_model)^2)
rss_null_loglog <- sum((df$log_food - mean(df$log_food))^2)
generalized_r2 <- 1 - exp(-(rss_null_loglog - rss_loglog) / nrow(df))

# 收集 R²
linear_r2 <- summary(linear_model)$r.squared
loglog_r2 <- summary(loglog_model)$r.squared

# 結果比較表
data.frame(
  Model = c("Linear", "Log-Log"),
  R2 = c(linear_r2, loglog_r2),
  Generalized_R2 = c(NA, generalized_r2)
)
#(f)
# 建立 log-log 模型（如果還沒建立）
loglog_model <- lm(log(food) ~ log(income), data = df)

# 顯示模型摘要，取出 log(INCOME) 的係數（即彈性）與信賴區間
summary(loglog_model)
confint(loglog_model, level = 0.95)
#(g)
# Residuals from log-log model
df$loglog_resid <- resid(loglog_model)

# Scatter plot: residuals vs ln(INCOME)
library(ggplot2)
ggplot(df, aes(x = log(income), y = loglog_resid)) +
  geom_point(alpha = 0.5, color = "tomato") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals vs ln(INCOME) - Log-Log Model",
       x = "ln(INCOME)",
       y = "Residuals") +
  theme_minimal()

# Histogram of residuals
ggplot(df, aes(x = loglog_resid)) +
  geom_histogram(binwidth = 0.2, fill = "gray", color = "black") +
  geom_vline(xintercept = mean(df$loglog_resid), color = "red", linetype = "dashed") +
  labs(title = "Histogram of Residuals - Log-Log Model",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# Jarque–Bera normality test
library(tseries)
jarque.bera.test(df$loglog_resid)

#(h)
# 建立 Linear-Log 模型：FOOD = α1 + α2 * ln(INCOME) + e
linlog_model <- lm(food ~ log(income), data = df)

# 顯示模型摘要
summary(linlog_model)

# 畫出 FOOD vs ln(INCOME) 的散佈圖與回歸線
library(ggplot2)
ggplot(df, aes(x = log(income), y = food)) +
  geom_point(alpha = 0.5, color = "gray50") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "FOOD vs ln(INCOME) - Linear-Log Model",
       x = "ln(INCOME)",
       y = "FOOD") +
  theme_minimal()

# 收集三個模型的 R²
linear_r2 <- summary(lm(food ~ income, data = df))$r.squared
loglog_r2 <- summary(lm(log(food) ~ log(income), data = df))$r.squared
linlog_r2 <- summary(linlog_model)$r.squared

# 整理成表格
data.frame(
  Model = c("Linear", "Log-Log", "Linear-Log"),
  R2 = c(linear_r2, loglog_r2, linlog_r2)
)

#(i)
df <- read.csv("cex5_small.csv")
# 確保資料與模型已建立
df$log_income <- log(df$income)
linlog_model <- lm(food ~ log_income, data = df)

# 設定三個收入水準
income_vals <- c(19, 65, 160)
log_income_vals <- log(income_vals)

# 預測 food（對應 log(INCOME)）
new_data <- data.frame(log_income = log_income_vals)
predicted_food <- predict(linlog_model, newdata = new_data)

# 取出 α2（log_income 的迴歸係數）與其標準誤
alpha2 <- coef(linlog_model)["log_income"]
se_alpha2 <- summary(linlog_model)$coefficients["log_income", "Std. Error"]

# 計算 α2 的 95% 信賴區間
alpha2_lower <- alpha2 - 1.96 * se_alpha2
alpha2_upper <- alpha2 + 1.96 * se_alpha2

# 彈性與其信賴區間計算（ε = α2 / FOOD）
elasticity <- alpha2 / predicted_food
elasticity_lower <- alpha2_lower / predicted_food
elasticity_upper <- alpha2_upper / predicted_food

# 建立並輸出結果表格
elasticity_result <- data.frame(
  INCOME = income_vals,
  Predicted_FOOD = round(predicted_food, 2),
  Elasticity = round(elasticity, 4),
  Lower_95_CI = round(elasticity_lower, 4),
  Upper_95_CI = round(elasticity_upper, 4)
)

print(elasticity_result)

#(j)
# 建立 log(income) 變數（如果尚未建立）
df$log_income <- log(df$income)

# 建立 Linear-Log 模型
linlog_model <- lm(food ~ log_income, data = df)

# 取得殘差
df$linlog_resid <- resid(linlog_model)

# 殘差對 ln(INCOME) 的散佈圖
library(ggplot2)
ggplot(df, aes(x = log_income, y = linlog_resid)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs ln(INCOME) - Linear-Log Model",
       x = "ln(INCOME)", y = "Residuals") +
  theme_minimal()

# 殘差直方圖
ggplot(df, aes(x = linlog_resid)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "black") +
  geom_vline(xintercept = mean(df$linlog_resid), color = "red", linetype = "dashed") +
  labs(title = "Histogram of Residuals - Linear-Log Model",
       x = "Residuals", y = "Frequency") +
  theme_minimal()

# Jarque–Bera 常態性檢定
library(tseries)
jarque.bera.test(df$linlog_resid)

