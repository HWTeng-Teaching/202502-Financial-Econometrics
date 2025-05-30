# 2.25
(a)
# 安裝並載入 POE5Rdata 資料庫
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)

# 載入 cex5_small 資料集
data("cex5_small")

# 繪製 FOODAWAY 的直方圖
hist(cex5_small$foodaway, 
     main = "Histogram of FOODAWAY", 
     xlab = "FOODAWAY (Monthly Food Away Expenditure per Person)", 
     col = "lightblue", 
     border = "black")

# 計算統計數據
mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)
percentile_25 <- quantile(cex5_small$foodaway, 0.25, na.rm = TRUE)
percentile_75 <- quantile(cex5_small$foodaway, 0.75, na.rm = TRUE)

# 顯示結果
cat("Mean of FOODAWAY:", mean_foodaway, "\n")
cat("Median of FOODAWAY:", median_foodaway, "\n")
cat("25th Percentile of FOODAWAY:", percentile_25, "\n")
cat("75th Percentile of FOODAWAY:", percentile_75, "\n")

# 繪製 FOODAWAY 直方圖，增加 bins 的數量
hist_data <- hist(cex5_small$foodaway, 
                  main = "Histogram of FOODAWAY (Frequency)", 
                  xlab = "FOODAWAY (Monthly Food Away Expenditure per Person)", 
                  ylab = "Frequency", 
                  col = "lightblue", 
                  border = "black",
                  freq = TRUE,  # Y 軸維持頻率（Frequency）
                  breaks = 30)  # 增加 bins 的數量，讓直方條更細緻

# 設定 Y 軸刻度為 0, 400, 800, ...
y_max <- max(hist_data$counts)  # 取得最大頻率值
y_ticks <- seq(0, ceiling(y_max / 400) * 400, by = 400)  # 確保最大值是 400 的倍數

# 重新繪製 Y 軸標籤
axis(2, at = y_ticks, labels = y_ticks)

(b)
# 計算擁有高級學位的家庭的 FOODAWAY 統計數據
foodaway_advanced <- cex5_small$foodaway[cex5_small$advanced == 1]
mean_advanced <- mean(foodaway_advanced, na.rm = TRUE)
median_advanced <- median(foodaway_advanced, na.rm = TRUE)
n_advanced <- sum(cex5_small$advanced == 1, na.rm = TRUE)  # 計算個數 N

# 計算至少有大學學位的家庭的 FOODAWAY 統計數據
foodaway_college <- cex5_small$foodaway[cex5_small$college == 1]
mean_college <- mean(foodaway_college, na.rm = TRUE)
median_college <- median(foodaway_college, na.rm = TRUE)
n_college <- sum(cex5_small$college == 1, na.rm = TRUE)  # 計算個數 N

# 計算沒有高級學位或大學學位的家庭的 FOODAWAY 統計數據
foodaway_no_degree <- cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0]
mean_no_degree <- mean(foodaway_no_degree, na.rm = TRUE)
median_no_degree <- median(foodaway_no_degree, na.rm = TRUE)
n_no_degree <- sum(cex5_small$advanced == 0 & cex5_small$college == 0, na.rm = TRUE)  # 計算個數 N

# 顯示結果
cat("FOODAWAY Statistics by Education Level:\n")
cat("1. Households with an Advanced Degree (N =", n_advanced, "):\n")
cat("   Mean:", mean_advanced, " Median:", median_advanced, "\n")

cat("2. Households with a College Degree (N =", n_college, "):\n")
cat("   Mean:", mean_college, " Median:", median_college, "\n")

cat("3. Households with No Advanced or College Degree (N =", n_no_degree, "):\n")
cat("   Mean:", mean_no_degree, " Median:", median_no_degree, "\n")

(c)
# 確保 FOODAWAY 沒有 NA
cex5_small <- cex5_small[!is.na(cex5_small$foodaway), ]

# 計算 FOODAWAY 的有效觀測數
n_foodaway <- sum(!is.na(cex5_small$foodaway))

# 計算 ln(FOODAWAY)，並處理 FOODAWAY = 0 的數據
cex5_small$log_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)

# 計算 ln(FOODAWAY) 的有效觀測數
n_log_foodaway <- sum(!is.na(cex5_small$log_foodaway))

# 計算 ln(FOODAWAY) 的統計摘要（包含 25th、75th 百分位數）
mean_log_foodaway <- mean(cex5_small$log_foodaway, na.rm = TRUE)
median_log_foodaway <- median(cex5_small$log_foodaway, na.rm = TRUE)
min_log_foodaway <- min(cex5_small$log_foodaway, na.rm = TRUE)
max_log_foodaway <- max(cex5_small$log_foodaway, na.rm = TRUE)
q1_log_foodaway <- quantile(cex5_small$log_foodaway, 0.25, na.rm = TRUE)  # 25th 百分位數
q3_log_foodaway <- quantile(cex5_small$log_foodaway, 0.75, na.rm = TRUE)  # 75th 百分位數

# 顯示統計摘要
cat("Summary Statistics of ln(FOODAWAY):\n")
cat("Mean:", mean_log_foodaway, "\n")
cat("Median:", median_log_foodaway, "\n")
cat("Min:", min_log_foodaway, "\n")
cat("Max:", max_log_foodaway, "\n")
cat("25th Percentile:", q1_log_foodaway, "\n")
cat("75th Percentile:", q3_log_foodaway, "\n")
cat("Number of Observations (ln(FOODAWAY)):", n_log_foodaway, "\n")
cat("Number of Observations (FOODAWAY):", n_foodaway, "\n")

# 繪製 ln(FOODAWAY) 的直方圖（Y 軸維持 Frequency）
hist(cex5_small$log_foodaway, 
     main = "Histogram of ln(FOODAWAY)", 
     xlab = "ln(FOODAWAY)", 
     ylab = "Frequency", 
     col = "lightblue", 
     border = "black", 
     breaks = 30)  # 設定較多的分組數，讓圖形更平滑

(d)
# 確保 FOODAWAY > 0，避免 log(0) 產生 -Inf
cex5_small$log_foodaway <- log(cex5_small$foodaway)

# 檢查是否有 NA 或 Inf
summary(cex5_small$log_foodaway)

# 移除 NA 或 -Inf 的觀測值
clean_data <- subset(cex5_small, !is.na(log_foodaway) & is.finite(log_foodaway))

# 再次執行回歸分析
reg_model <- lm(log_foodaway ~ income, data = clean_data)

# 顯示回歸結果
summary(reg_model)

(e)
# 繪製 ln(FOODAWAY) 對 INCOME 的散佈圖，並加入回歸擬合線
plot(clean_data$income, clean_data$log_foodaway,
     main = "Scatter Plot of ln(FOODAWAY) vs INCOME",
     xlab = "INCOME (Household Monthly Income in $100 Units)",
     ylab = "ln(FOODAWAY)",
     col = "blue", 
     pch = 16,   # 使用實心圓點
     cex = 0.7)  # 調整點的大小

# 加入回歸擬合線（fitted line）
abline(reg_model, col = "red", lwd = 2)

# 加入圖例
legend("topleft", legend = c("Data Points", "Fitted Line"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

(f)
# 計算 ln(FOODAWAY)
cex5_small$log_foodaway <- log(cex5_small$foodaway)

# 移除 NA 或 Inf，確保回歸不會出錯
clean_data <- subset(cex5_small, !is.na(log_foodaway) & is.finite(log_foodaway))

# 執行 OLS 回歸（來自 part d）
reg_model <- lm(log_foodaway ~ income, data = clean_data)

# 計算殘差（Residuals）
clean_data$residuals <- residuals(reg_model)

# **繪製殘差對 INCOME 的散佈圖**
plot(clean_data$income, clean_data$residuals,
     main = "Residuals vs. INCOME",
     xlab = "INCOME (Household Monthly Income in $100 Units)",
     ylab = "Residuals",
     col = "blue",
     pch = 16,   # 使用實心圓點
     cex = 0.7)  # 調整點的大小

# 加入 y = 0 的參考線
abline(h = 0, col = "red", lwd = 2)

# 加入圖例
legend("topright", legend = c("Residuals", "Zero Reference Line"), 
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

