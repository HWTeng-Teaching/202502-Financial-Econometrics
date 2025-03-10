library(POE5Rdata)
install.packages("ggplot2")  # 安裝 ggplot2
library(ggplot2)
(a) 
data(cex5_small)
cex5_small
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5) +labs(title = "Histogram of FOODAWAY",
       x = "FOODAWAY (dollars)",
       y = "Income")

summary(cex5_small$foodaway)
quantile(cex5_small$foodaway, probs = c(0.25,0.5, 0.75))




# 確保數據名稱正確（應使用 cex5_small）
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +  # 加入邊框顏色
  labs(title = "Histogram of FOODAWAY",
       x = "FOODAWAY (dollars)",
       y = "Count") +  # 更適合的 Y 軸標籤
  theme_minimal()  # 使用較美觀的主題

#(b)

# 計算有高等學位的家庭
mean_advanced <- mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
median_advanced <- median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)

# 計算有大學學位的家庭
mean_college <- mean(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)
median_college <- median(cex5_small$foodaway[cex5_small$college == 1], na.rm = TRUE)

# 計算沒有高等或大學學位的家庭
mean_no_degree <- mean(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)
median_no_degree <- median(cex5_small$foodaway[cex5_small$advanced == 0 & cex5_small$college == 0], na.rm = TRUE)

# 顯示結果
mean_advanced
median_advanced
mean_college
median_college
mean_no_degree
median_no_degree




#(d)

# 取對數但排除 FOODAWAY = 0 的情況
cex5_small <- cex5_small[cex5_small$foodaway > 0, ]
cex5_small$log_foodaway <- log(cex5_small$foodaway)

# 重新執行回歸
model_log <- lm(log_foodaway ~ income, data = cex5_small)

# 顯示結果
summary(model_log)


#(e)

library(ggplot2)

ggplot(cex5_small, aes(x = income, y = log_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 添加回歸線
  labs(title = "Scatterplot of ln(FOODAWAY) vs. INCOME",
       x = "Household Income (in $100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()




#(f)
# 計算殘差
cex5_small$residuals_log <- residuals(model_log)

# 繪製殘差圖
library(ggplot2)

ggplot(cex5_small, aes(x = income, y = residuals_log)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 水平基準線
  labs(title = "Residual Plot: ln(FOODAWAY) vs. INCOME",
       x = "Household Income (INCOME)",
       y = "Residuals") +
  theme_minimal()
