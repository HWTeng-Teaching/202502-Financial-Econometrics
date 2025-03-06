#A.
# 🌟 清空環境
rm(list = ls())

# 🌟 載入必要套件
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 🔗 下載並載入 cex5_small 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cex5_small)

# 檢查變數名稱，確保 foodaway 存在
colnames(cex5_small)

# 計算 foodaway 的統計數據
summary_stats <- cex5_small %>%
  summarise(
    Mean = mean(foodaway, na.rm = TRUE),
    Median = median(foodaway, na.rm = TRUE),
    Q25 = quantile(foodaway, 0.25, na.rm = TRUE),
    Q75 = quantile(foodaway, 0.75, na.rm = TRUE)
  )

# 顯示統計結果
print(summary_stats)

# 繪製 foodaway 的直方圖
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(color = "black", fill = "blue", bins = 30, alpha = 0.7) +
  labs(
    title = "Histogram of Foodaway Expenditure",
    x = "Food Away from Home ($ per month per person)",
    y = "Frequency"
  ) +
  theme_minimal()

#B.
# Compute summary statistics for each group
summary_stats <- cex5_small %>%
  mutate(
    category = case_when(
      advanced == 1 ~ "Advanced Degree",
      college == 1 & advanced == 0 ~ "College Degree",
      college == 0 & advanced == 0 ~ "No College/Advanced Degree"
    )
  ) %>%
  group_by(category) %>%
  summarise(
    Mean = mean(foodaway, na.rm = TRUE),
    Median = median(foodaway, na.rm = TRUE)
  )

# Display results
print(summary_stats)

#C.
# Compute summary statistics for log(foodaway)
# Compute log(foodaway), ensuring we avoid log(0) or negative values
cex5_small <- cex5_small %>%
  mutate(log_foodaway = ifelse(foodaway > 0, log(foodaway), NA))

summary_log <- cex5_small %>%
  summarise(
    Mean = mean(log_foodaway, na.rm = TRUE),
    Median = median(log_foodaway, na.rm = TRUE),
    Q25 = quantile(log_foodaway, 0.25, na.rm = TRUE),
    Q75 = quantile(log_foodaway, 0.75, na.rm = TRUE),
    Count_Log = sum(!is.na(log_foodaway)),  # Count of valid log values
    Count_Original = sum(!is.na(foodaway))  # Count of original foodaway values
  )

# Print summary statistics
print(summary_log)

# Plot histogram of log(foodaway)
ggplot(cex5_small, aes(x = log_foodaway)) +
  geom_histogram(color = "black", fill = "blue", bins = 30, alpha = 0.7) +
  labs(
    title = "Histogram of ln(foodaway) Expenditure",
    x = "ln(Food Away from Home)",
    y = "Frequency"
  ) +
  theme_minimal()

#D.E.
# 確保 foodaway > 0，計算 ln(foodaway)，並處理 NA
cex5_small <- cex5_small %>%
  mutate(log_foodaway = ifelse(foodaway > 0, log(foodaway), NA))

# 過濾掉 NA 值
cex5_clean <- cex5_small %>%
  filter(!is.na(log_foodaway), !is.na(income))  # 確保 income 也沒有 NA

# **檢查數據是否正確**
print(head(cex5_clean))  # 確認 log_foodaway 和 income 存在

# **執行線性回歸**
model <- lm(log_foodaway ~ income, data = cex5_clean)

# **檢查回歸模型是否成功**
summary(model)  # 如果這裡報錯，表示回歸沒有成功

# 取得回歸係數
intercept <- coef(model)[1]  # 截距 β1
slope <- coef(model)[2]       # 斜率 β2

# 產生回歸方程式的文字
regression_eq <- paste0("ln(foodaway) = ", round(intercept, 2), 
                        " + ", round(slope, 4), " * income")

# 設定標示回歸公式的位置
x_pos <- quantile(cex5_clean$income, 0.7, na.rm = TRUE)
y_pos <- quantile(cex5_clean$log_foodaway, 0.9, na.rm = TRUE)

# 繪製散點圖與回歸線
ggplot(cex5_clean, aes(x = income, y = log_foodaway)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # 回歸線
  annotate("text", x = x_pos, y = y_pos, label = regression_eq, 
           size = 5, color = "black", hjust = 0) +  # 正確標示回歸公式
  labs(
    title = "Linear Regression: ln(Foodaway) vs. Income",
    x = "Income (Hundreds of Dollars)",
    y = "ln(Foodaway)"
  ) +
  theme_minimal()

#F.
#處理數值
cex5_small <- cex5_small %>%
  mutate(log_foodaway = ifelse(foodaway > 0 ,log(foodaway),NA))

cex5_clean <- cex5_small %>%
  filter(!is.na(log_foodaway), !is.na(income))

#執行線性迴歸
log_model <- lm(log_foodaway ~ income, data = cex5_clean)

#計算殘差
cex5_clean <- cex5_clean %>%
  mutate(residual = resid(log_model))

#繪製殘差圖
ggplot(cex5_clean, aes(y = residual, x = income)) +
  geom_point(colour = "blue", alpha = 0.6) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1.5 ) +
  labs(
    title = "Residual vs income",
    x = "Income(Hundreds of dollars)",
    y = "residual"
  ) +
  theme_minimal()
# From the residual plot, several groups of residual points are aligned along
# a distinct straight line, indicating that the residuals are not completely random.
    
