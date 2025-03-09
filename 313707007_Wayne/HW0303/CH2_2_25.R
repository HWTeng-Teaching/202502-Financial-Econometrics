#(a)

# 計算統計數據
mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)  # 平均值
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)  # 中位數
percentile_25 <- quantile(cex5_small$foodaway, 0.25, na.rm = TRUE)  # 25th 百分位數
percentile_75 <- quantile(cex5_small$foodaway, 0.75, na.rm = TRUE)  # 75th 百分位數

# 顯示統計結果
cat("Mean:", mean_foodaway, "\n")
cat("Median:", median_foodaway, "\n")
cat("25th Percentile:", percentile_25, "\n")
cat("75th Percentile:", percentile_75, "\n")

# 繪製 FOODAWAY 直方圖
library(ggplot2)

ggplot(cex5_small, aes(x = foodaway, y = ..density.. * 100)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, max(cex5_small$foodaway, na.rm = TRUE), 100), expand = c(0, 0)) +  # 設定 X 軸刻度為 100
  labs(title = "Histogram of FOODAWAY",
       x = "FOODAWAY (Monthly Food Away Expenditure per Person)",
       y = "Percentage") +
  theme_minimal()
#(b)
# 讀取數據
cex5_small <- read.csv("cex5_small.csv")

# 計算有高等學位 (advanced == 1) 的家庭的 FOODAWAY 平均值與中位數
mean_advanced <- mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
median_advanced <- median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)

# 計算有大學學位 (college == 1) 的家庭的 FOODAWAY 平均值與中位數
mean_college <- mean(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)
median_college <- median(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)

# 計算無高等或大學學位 (advanced == 0 且 college == 0) 的家庭的 FOODAWAY 平均值與中位數
mean_no_degree <- mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)
median_no_degree <- median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)

# 顯示結果
cat("FOODAWAY Mean & Median by Education Level:\n")
cat("Advanced Degree - Mean:", mean_advanced, "Median:", median_advanced, "\n")
cat("College Degree - Mean:", mean_college, "Median:", median_college, "\n")
cat("No Degree - Mean:", mean_no_degree, "Median:", median_no_degree, "\n")

#(c)
# 計算 ln(FOODAWAY)，排除非正值（FOODAWAY <= 0 的情況）
cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)

# 計算 ln(FOODAWAY) 的統計數據
mean_ln_foodaway <- mean(cex5_small$ln_foodaway, na.rm = TRUE)
median_ln_foodaway <- median(cex5_small$ln_foodaway, na.rm = TRUE)
percentile_25_ln <- quantile(cex5_small$ln_foodaway, 0.25, na.rm = TRUE)
percentile_75_ln <- quantile(cex5_small$ln_foodaway, 0.75, na.rm = TRUE)

# 顯示統計結果
cat("Summary Statistics for ln(FOODAWAY):\n")
cat("Mean:", mean_ln_foodaway, "\n")
cat("Median:", median_ln_foodaway, "\n")
cat("25th Percentile:", percentile_25_ln, "\n")
cat("75th Percentile:", percentile_75_ln, "\n")

# 繪製 ln(FOODAWAY) 直方圖
ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(FOODAWAY) (Log of Monthly Food Away Expenditure per Person)",
       y = "Frequency") +
  theme_minimal()

(d)
# 創建 ln(FOODAWAY)，排除非正值（FOODAWAY <= 0）
cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)

# 執行線性回歸 ln(FOODAWAY) ~ INCOME
model <- lm(ln_foodaway ~ income, data = cex5_small, na.action = na.omit)

# 顯示回歸結果
summary(model)

#(e)

cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)

# 執行線性回歸 ln(FOODAWAY) ~ INCOME
model <- lm(ln_foodaway ~ income, data = cex5_small, na.action = na.omit)

# 繪製散點圖 + 擬合回歸線

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖（藍色點）
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid", size = 1.2) +  # 加入回歸線
  labs(title = "Scatter Plot of ln(FOODAWAY) vs. INCOME",
       x = "Household Monthly Income (100s of USD)",
       y = "ln(FOODAWAY) (Log of Monthly Food Away Expenditure per Person)") +
  theme_minimal()

#(f)
# 去除 NA 行，確保 `model` 和 `residuals` 數量匹配
cex5_small_clean <- na.omit(cex5_small[, c("ln_foodaway", "income")]) 

# 執行線性回歸
model <- lm(ln_foodaway ~ income, data = cex5_small_clean)

# 計算殘差
cex5_small_clean$residuals <- resid(model)

ggplot(cex5_small_clean, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1.2) +  # 0 水平線
  labs(title = "Residual Plot: ln(FOODAWAY) vs. INCOME",
       x = "Household Monthly Income (100s of USD)",
       y = "Residuals") +
  theme_minimal()


