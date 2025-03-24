library(POE5Rdata)
data("cex5_small")

# a.
# 計算 FOOD 的統計描述
food_stats <- c(
  mean = mean(cex5_small$food, na.rm = TRUE),
  median = median(cex5_small$food, na.rm = TRUE),
  min = min(cex5_small$food, na.rm = TRUE),
  max = max(cex5_small$food, na.rm = TRUE),
  sd = sd(cex5_small$food, na.rm = TRUE)
)

# 計算 INCOME 的統計描述
income_stats <- c(
  mean = mean(cex5_small$income, na.rm = TRUE),
  median = median(cex5_small$income, na.rm = TRUE),
  min = min(cex5_small$income, na.rm = TRUE),
  max = max(cex5_small$income, na.rm = TRUE),
  sd = sd(cex5_small$income, na.rm = TRUE)
)

# 顯示統計描述
cat("FOOD 的統計描述：\n")
print(food_stats)
cat("\nINCOME 的統計描述：\n")
print(income_stats)

# 載入 ggplot2 繪圖套件
library(ggplot2)

# 繪製 food 的直方圖
p1 <- ggplot(cex5_small, aes(x = food)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean(food, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(food, na.rm = TRUE)), color = "blue", linetype = "dotted", size = 1) +
  labs(title = "Histogram of food", x = "food (USD)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = mean(cex5_small$food, na.rm = TRUE) + 10, y = 50, label = "Mean", color = "red") +
  annotate("text", x = median(cex5_small$food, na.rm = TRUE) + 10, y = 45, label = "Median", color = "blue")

# 繪製 income 的直方圖
p2 <- ggplot(cex5_small, aes(x = income)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  geom_vline(aes(xintercept = mean(income, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(income, na.rm = TRUE)), color = "blue", linetype = "dotted", size = 1) +
  labs(title = "Histogram of income", x = "income (USD)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = mean(cex5_small$income, na.rm = TRUE) + 10, y = 50, label = "Mean", color = "red") +
  annotate("text", x = median(cex5_small$income, na.rm = TRUE) + 10, y = 45, label = "Median", color = "blue")

# 排列圖形
par(mfrow = c(2, 1))
print(p1)
print(p2)

# 手動計算 Jarque-Bera 統計量
# 計算 food 的偏度和峰度
n_food <- sum(!is.na(cex5_small$food))  # 樣本大小（忽略 NA）
skewness_food <- sum((cex5_small$food - mean(cex5_small$food, na.rm = TRUE))^3, na.rm = TRUE) / 
  (n_food * sd(cex5_small$food, na.rm = TRUE)^3)
kurtosis_food <- sum((cex5_small$food - mean(cex5_small$food, na.rm = TRUE))^4, na.rm = TRUE) / 
  (n_food * sd(cex5_small$food, na.rm = TRUE)^4)

# 計算 food 的 JB 統計量
jb_stat_food <- (n_food / 6) * (skewness_food^2 + (kurtosis_food - 3)^2 / 4)
p_value_food <- 1 - pchisq(jb_stat_food, df = 2)

# 計算 income 的偏度和峰度
n_income <- sum(!is.na(cex5_small$income))
skewness_income <- sum((cex5_small$income - mean(cex5_small$income, na.rm = TRUE))^3, na.rm = TRUE) / 
  (n_income * sd(cex5_small$income, na.rm = TRUE)^3)
kurtosis_income <- sum((cex5_small$income - mean(cex5_small$income, na.rm = TRUE))^4, na.rm = TRUE) / 
  (n_income * sd(cex5_small$income, na.rm = TRUE)^4)

# 計算 income 的 JB 統計量
jb_stat_income <- (n_income / 6) * (skewness_income^2 + (kurtosis_income - 3)^2 / 4)
p_value_income <- 1 - pchisq(jb_stat_income, df = 2)

# 顯示手動計算的結果
cat("手動計算的 Jarque-Bera 檢驗結果（food）:\n")
cat("JB 統計量:", jb_stat_food, "\np 值:", p_value_food, "\n")
cat("\n手動計算的 Jarque-Bera 檢驗結果（income）:\n")
cat("JB 統計量:", jb_stat_income, "\np 值:", p_value_income, "\n")

# b.
# 估計線性回歸模型
model_linear <- lm(food ~ income, data = cex5_small)

# 顯示回歸結果
summary(model_linear)

# 繪製散點圖並加入擬合直線
scatter_plot <- ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 擬合直線
  labs(title = "Scatter Plot of food vs income with Fitted Line",
       x = "income (USD)",
       y = "food (USD)") +
  theme_minimal()

# 顯示圖形
print(scatter_plot)

# 提取回歸係數和置信區間
conf_int <- confint(model_linear, level = 0.95)

# 顯示置信區間
print(conf_int)

# c.----------------
# 提取殘差
residuals <- resid(model_linear)

# 將殘差和 income 組成數據框
residual_data <- data.frame(income = cex5_small$income, residuals = residuals)

# 繪製殘差對 income 的散點圖
residual_plot <- ggplot(residual_data, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 添加 y=0 的參考線
  labs(title = "Residuals vs income",
       x = "income (USD)",
       y = "Residuals") +
  theme_minimal()

# 顯示圖形
print(residual_plot)

# 繪製殘差的直方圖
residual_hist <- ggplot(residual_data, aes(x = residuals)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# 顯示圖形
print(residual_hist)

# 手動計算 Jarque-Bera 統計量
n_resid <- length(residuals)  # 樣本大小
skewness_resid <- sum((residuals - mean(residuals))^3) / (n_resid * sd(residuals)^3)  # 偏度
kurtosis_resid <- sum((residuals - mean(residuals))^4) / (n_resid * sd(residuals)^4)  # 峰度
jb_stat_resid <- (n_resid / 6) * (skewness_resid^2 + (kurtosis_resid - 3)^2 / 4)  # JB 統計量
p_value_resid <- 1 - pchisq(jb_stat_resid, df = 2)  # p 值

# 顯示 Jarque-Bera 檢驗結果
cat("Jarque-Bera 檢驗結果（殘差）:\n")
cat("JB 統計量:", jb_stat_resid, "\np 值:", p_value_resid, "\n")

# e.------------
# 對 food 和 income 取自然對數
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

# 估計對數-對數模型
model_loglog <- lm(log_food ~ log_income, data = cex5_small)

# 顯示回歸結果
summary(model_loglog)

# 繪製 ln(food) 對 ln(income) 的散點圖並加入擬合直線
scatter_loglog <- ggplot(cex5_small, aes(x = log_income, y = log_food)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 擬合直線
  labs(title = "Scatter Plot of ln(food) vs ln(income) with Fitted Line",
       x = "ln(income)",
       y = "ln(food)") +
  theme_minimal()

# 顯示圖形
print(scatter_loglog)

# 計算對數-對數模型的預測值（對數尺度）
cex5_small$log_food_pred <- predict(model_loglog)

# 轉換回原始尺度
cex5_small$food_pred <- exp(cex5_small$log_food_pred)

# 計算廣義 R^2
ss_total <- sum((cex5_small$food - mean(cex5_small$food))^2)  # 總平方和
ss_residual <- sum((cex5_small$food - cex5_small$food_pred)^2)  # 殘差平方和
r2_generalized <- 1 - ss_residual / ss_total

# 顯示廣義 R^2
cat("對數-對數模型的廣義 R^2:", r2_generalized, "\n")

# g.-------------------
# 對 food 和 income 取自然對數
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

# 重新運行對數-對數模型
model_loglog <- lm(log_food ~ log_income, data = cex5_small)

# 提取殘差
residuals <- resid(model_loglog)

# 將殘差和 log_income 組成數據框
residual_data <- data.frame(log_income = cex5_small$log_income, residuals = residuals)

# 繪製殘差對 log_income 的散點圖
residual_plot <- ggplot(residual_data, aes(x = log_income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 添加 y=0 的參考線
  labs(title = "Residuals vs ln(income)",
       x = "ln(income)",
       y = "Residuals") +
  theme_minimal()

# 顯示圖形
print(residual_plot)

# 繪製殘差的直方圖
residual_hist <- ggplot(residual_data, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Residuals (Log-Log Model)",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

# 顯示圖形
print(residual_hist)

# 手動計算 Jarque-Bera 統計量
n_resid <- length(residuals)  # 樣本大小
skewness_resid <- sum((residuals - mean(residuals))^3) / (n_resid * sd(residuals)^3)  # 偏度
kurtosis_resid <- sum((residuals - mean(residuals))^4) / (n_resid * sd(residuals)^4)  # 峰度
jb_stat_resid <- (n_resid / 6) * (skewness_resid^2 + (kurtosis_resid - 3)^2 / 4)  # JB 統計量
p_value_resid <- 1 - pchisq(jb_stat_resid, df = 2)  # p 值

# 顯示 Jarque-Bera 檢驗結果
cat("Jarque-Bera 檢驗結果（殘差）:\n")
cat("JB 統計量:", jb_stat_resid, "\np 值:", p_value_resid, "\n")

# h.----------------
# 對 income 取自然對數
cex5_small$log_income <- log(cex5_small$income)

# 估計線性-對數模型
model_linearlog <- lm(food ~ log_income, data = cex5_small)

# 顯示回歸結果
summary(model_linearlog)

# 繪製 food 對 log_income 的散點圖並加入擬合直線
scatter_linearlog <- ggplot(cex5_small, aes(x = log_income, y = food)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 擬合直線
  labs(title = "Scatter Plot of food vs ln(income) with Fitted Line",
       x = "ln(income)",
       y = "food") +
  theme_minimal()

# 顯示圖形
print(scatter_linearlog)