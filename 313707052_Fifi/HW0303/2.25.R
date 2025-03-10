library(ggplot2)

data("cex5_small", package = "POE5Rdata")
names(cex5_small)

#a.

# 繪製 FOODAWAY 直方圖
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(color = "black", fill = "blue", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of FOODAWAY",
       x = "Food Away Expenditure (dollars per month per person)",
       y = "Frequency") +
  theme_minimal()

# 計算描述統計
summary(cex5_small$foodaway)
quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

#b.

# 載入必要套件
install.packages("dplyr")
library(dplyr)

# 創建教育程度分組
cex5_small <- cex5_small %>%
  mutate(education_group = case_when(
    advanced == 1 ~ "Advanced Degree",
    college == 1 & advanced == 0 ~ "College Degree",
    college == 0 & advanced == 0 ~ "No College Degree"
  ))

# 計算人數、均值和中位數
summary_stats <- cex5_small %>%
  group_by(education_group) %>%
  summarise(
    count = n(),
    mean_foodaway = mean(foodaway, na.rm = TRUE),
    median_foodaway = median(foodaway, na.rm = TRUE)
  )

# 顯示結果
print(summary_stats)

#c.

# (1) 先檢查 foodaway 是否有 0 或負數
summary(cex5_small$foodaway)

# (2) 過濾掉 foodaway <= 0，避免 log(0) 或 log(負數)
cex5_small <- cex5_small %>%
  filter(foodaway > 0) %>%
  mutate(ln_foodaway = log(foodaway))

# (3) 繪製 ln(foodaway) 直方圖
ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(color = "black", fill = "red", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "Log of Food Away Expenditure",
       y = "Frequency") +
  theme_minimal()

# (4) 計算描述統計
summary(cex5_small$ln_foodaway)

#d.

# 擬合線性回歸模型
linear_model <- lm(ln_foodaway ~ income, data = cex5_small)

# 顯示回歸結果
summary(linear_model)

#e.

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 加入回歸線
  labs(title = "Scatter Plot of ln(FOODAWAY) vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "Log of Food Away Expenditure") +
  theme_minimal()

#f.

# 計算殘差
residuals_linear <- resid(linear_model)

# 繪製殘差 vs. income
ggplot(cex5_small, aes(x = income, y = residuals_linear)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # y=0 參考線
  labs(title = "Residuals Plot: ln(FOODAWAY) vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "Residuals") +
  theme_minimal()


