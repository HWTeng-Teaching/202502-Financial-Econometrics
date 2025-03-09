# 安裝並載入必要的套件
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入數據集
library(POE5Rdata)
data("cex5_small")

# 提取 FOODAWAY 數據
foodaway_data <- cex5_small$foodaway

# 繪製 FOODAWAY 的直方圖
library(ggplot2)

# ## a.長條圖
# print(
# ggplot(cex5_small, aes(x = foodaway)) +
#   geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
#   labs(title = "Distribution of FOODAWAY",
#        x = "FOODAWAY (Spending on Food Away from Home)",
#        y = "Frequency") +
#   theme_minimal()
# )
# 
# # 計算摘要統計量
# foodaway_summary <- summary(foodaway_data)
# mean_value <- mean(foodaway_data, na.rm = TRUE)  # 均值
# median_value <- median(foodaway_data, na.rm = TRUE)  # 中位數
# quantiles <- quantile(foodaway_data, probs = c(0.25, 0.75), na.rm = TRUE)  # 25% 和 75% 百分位數
# 
# # 顯示結果
# cat("Mean (均值):", mean_value, "\n")
# cat("Median (中位數):", median_value, "\n")
# cat("25th Percentile:", quantiles[1], "\n")
# cat("75th Percentile:", quantiles[2], "\n")

# ## b. 
# # 設定不同教育程度的條件
# advanced_degree <- cex5_small$advanced == 1  # 有高等學位
# college_degree <- (cex5_small$college == 1)  # 只有學士學位
# no_degree <- (cex5_small$college == 0 & cex5_small$advanced == 0)  # 無學士或高等學位
# 
# # 計算各組的均值與中位數
# foodaway_advanced <- cex5_small$foodaway[advanced_degree]
# foodaway_college <- cex5_small$foodaway[college_degree]
# foodaway_none <- cex5_small$foodaway[no_degree]
# 
# # 進行統計計算
# results <- data.frame(
#   Category = c("Advanced Degree", "College Degree", "No Degree"),
#   Mean = c(mean(foodaway_advanced, na.rm = TRUE),
#            mean(foodaway_college, na.rm = TRUE),
#            mean(foodaway_none, na.rm = TRUE)),
#   Median = c(median(foodaway_advanced, na.rm = TRUE),
#              median(foodaway_college, na.rm = TRUE),
#              median(foodaway_none, na.rm = TRUE))
# )
# 
# # 顯示結果
# print(results)


# ##c.
# 計算 ln(FOODAWAY)，避免 log(0) 問題，過濾 NA 和 0
# cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
# 
# # 繪製直方圖
# print(
# ggplot(cex5_small, aes(x = ln_foodaway)) +
#   geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
#   labs(title = "Histogram of ln(FOODAWAY)",
#        x = "ln(FOODAWAY)",
#        y = "Frequency") +
#   theme_minimal()
# )
# # 計算摘要統計
# summary_stats <- data.frame(
#   Statistic = c("Mean", "Median", "25th Percentile", "75th Percentile"),
#   Value = c(
#     mean(cex5_small$ln_foodaway, na.rm = TRUE),
#     median(cex5_small$ln_foodaway, na.rm = TRUE),
#     quantile(cex5_small$ln_foodaway, 0.25, na.rm = TRUE),
#     quantile(cex5_small$ln_foodaway, 0.75, na.rm = TRUE)
#   )
# )
# 
# # 顯示摘要統計
# print(summary_stats)

## d.
# 先取 ln(FOODAWAY)，過濾掉非正數的值（因為 log 不能取負數或零）
cex5_small <- subset(cex5_small, foodaway > 0)  # 過濾掉 foodaway <= 0
cex5_small$ln_foodaway <- log(cex5_small$foodaway)  # 取 ln(FOODAWAY)

# 執行線性回歸
model <- lm(ln_foodaway ~ income, data = cex5_small)

# 顯示回歸結果
summary(model)
beta_2 <- coef(model)[2]

cat("The slope is:", beta_2, "\n")

# ## e.
# # 繪製散點圖並加上回歸線
# print(
# ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
#   geom_point(color = "blue", alpha = 0.6) +  # 散點圖
#   geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +  # 回歸線
#   labs(title = "ln(FOODAWAY) vs INCOME",
#        x = "Income",
#        y = "ln(FOODAWAY)") +
#   theme_minimal()
# )

## f.
# 計算最小平方殘差
residuals <- model$residuals

# 繪製殘差與 INCOME 的關係圖
print(
ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 加入 0 線，便於檢視殘差的偏離
  labs(title = "Residuals vs INCOME",
       x = "Income",
       y = "Residuals") +
  theme_minimal()
)






