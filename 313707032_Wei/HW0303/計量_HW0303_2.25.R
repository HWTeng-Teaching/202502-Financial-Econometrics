# 先安裝並載入必要的套件
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# 讀取數據
load("cex5_small.rda")

# 檢查變數名稱
print(names(cex5_small))

# 確保 foodaway 是數值類型
cex5_small$foodaway <- as.numeric(cex5_small$foodaway)
cex5_small$income <- as.numeric(cex5_small$income)

# (a) FOODAWAY 直方圖與統計數據
hist(cex5_small$foodaway, main = "Histogram of FOODAWAY",
     xlab = "Food Away from Home Expenditure",
     col = "lightblue", border = "black")

# 計算平均數, 中位數, 以及 25th & 75th 百分位數
mean_value <- mean(cex5_small$foodaway, na.rm = TRUE)
median_value <- median(cex5_small$foodaway, na.rm = TRUE)
quantiles <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

# 輸出結果
print(paste("Mean of FOODAWAY:", round(mean_value, 2)))
print(paste("Median of FOODAWAY:", round(median_value, 2)))
print("25th & 75th Percentiles:")
print(round(quantiles, 2))


# 2.25(b) 計算不同教育程度家庭的 FOODAWAY 平均值與中位數
# 確保變數 foodaway 是數值
cex5_small$foodaway <- as.numeric(cex5_small$foodaway)

# 計算不同教育程度家庭的 FOODAWAY 統計值
if(all(c("advanced", "college", "foodaway") %in% names(cex5_small))) {
  
  # 有高等學位的家庭
  advanced_mean <- round(mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE), 2)
  advanced_median <- round(median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE), 2)
  
  # 有大學學位的家庭
  college_mean <- round(mean(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE), 2)
  college_median <- round(median(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE), 2)
  
  # **無大學學位的家庭 (college = 0 且 advanced = 0)**
  no_college_data <- cex5_small[cex5_small$college == 0 & cex5_small$advanced == 0, ]
  no_college_mean <- round(mean(no_college_data$foodaway, na.rm = TRUE), 2)
  no_college_median <- round(median(no_college_data$foodaway, na.rm = TRUE), 2)
  
  # 計算觀察數量 N
  n_advanced <- sum(cex5_small$advanced == 1, na.rm = TRUE)
  n_college <- sum(cex5_small$college == 1, na.rm = TRUE)
  n_no_college <- nrow(no_college_data)
  
  # 輸出結果
  print("--- FOODAWAY Statistics by Education Level ---")
  print(paste("Advanced Degree (N =", n_advanced, "): Mean =", advanced_mean, " Median =", advanced_median))
  print(paste("College Degree (N =", n_college, "): Mean =", college_mean, " Median =", college_median))
  print(paste("No College (N =", n_no_college, "): Mean =", no_college_mean, " Median =", no_college_median))
  
} else {
  print("Error: Required variables not found in dataset.")
}

# 2.25(c) 繪製 ln(FOODAWAY) 直方圖
library(ggplot2)

# 確保 FOODAWAY > 0，避免 log(0) 無效
filtered_data <- cex5_small[cex5_small$foodaway > 0, ]

# 計算 ln(FOODAWAY)
filtered_data$ln_foodaway <- log(filtered_data$foodaway)

# 繪製直方圖 (y 軸為百分比)
ggplot(filtered_data, aes(x = ln_foodaway)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
                 binwidth = 0.3, fill = "lightblue", color = "black") +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "Log of Food Away from Home Expenditure",
       y = "Percent") +
  theme_minimal()
library(ggplot2)

# 確保 FOODAWAY > 0，避免 log(0) 無效
filtered_data <- cex5_small[cex5_small$foodaway > 0, ]

# 計算 ln(FOODAWAY)
filtered_data$ln_foodaway <- log(filtered_data$foodaway)

# 繪製直方圖 (y 軸為百分比)
ggplot(filtered_data, aes(x = ln_foodaway)) +
  geom_histogram(aes(y = after_stat(count) / sum(after_stat(count)) * 100), 
                 binwidth = 0.3, fill = "lightblue", color = "black") +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "Log of Food Away from Home Expenditure",
       y = "Percent") +
  theme_minimal()

# 2.25 (d) 估計線性回歸 ln(FOODAWAY) ~ INCOME
# 確保 FOODAWAY > 0，避免 log(0) 無效
filtered_data <- cex5_small[cex5_small$foodaway > 0, ]

# 計算 ln(FOODAWAY)
filtered_data$ln_foodaway <- log(filtered_data$foodaway)

# 確保 ln_foodaway 和 income 變數沒有 NA
filtered_data <- na.omit(filtered_data[, c("ln_foodaway", "income")])

# 執行線性回歸
lm_model <- lm(ln_foodaway ~ income, data = filtered_data)

# 顯示回歸結果
summary(lm_model)

# 輸出回歸係數
print("Regression Coefficients:")
print(coef(lm_model))

#2.25 (e) 繪製 ln(FOODAWAY) vs. INCOME 並加入回歸線
# 過濾掉 foodaway == 0，因為 log(0) 會變成 -Inf
filtered_data <- subset(cex5_small, foodaway > 0)

# 計算 ln(FOODAWAY)
filtered_data$ln_foodaway <- log(filtered_data$foodaway)

# 繪製 ln(FOODAWAY) vs. INCOME 並加上回歸線
ggplot(filtered_data, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "ln(FOODAWAY) vs. INCOME",
       x = "Household Income (in $100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

# 2.25(f) 計算殘差並繪製殘差圖
install.packages("dplyr")  # 如果尚未安裝
library(dplyr)  # 確保載入 dplyr

# 過濾掉 foodaway = 0 的數據
filtered_data <- subset(cex5_small, foodaway > 0)  # 或者使用 dplyr::filter()

#過濾後的數據 建立回歸模型
lm_model <- lm(log(foodaway) ~ income, data = filtered_data)

#計算殘差
filtered_data$residuals <- resid(lm_model)

#繪製殘差圖
ggplot(filtered_data, aes(x = income, y = residuals)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residual Plot",
       x = "Household Income (in $100 units)",
       y = "Residuals") +
  theme_minimal()