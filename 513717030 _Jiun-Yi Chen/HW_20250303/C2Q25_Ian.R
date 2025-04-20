#a

install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(cex5_small)
data <- cex5_small

str(data)  # 檢查數據結構
summary(data)  # 檢視數據摘要

#繪製直方圖
library(ggplot2)

ggplot(data, aes(x = foodaway)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "FOODAWAY 的直方圖", x = "每人每月外食支出 (美元)", y = "頻數") +
  theme_minimal()

#計算統計指標
mean_foodaway <- mean(data$foodaway, na.rm = TRUE)  # 平均值
median_foodaway <- median(data$foodaway, na.rm = TRUE)  # 中位數
percentiles <- quantile(data$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)  # 25th 和 75th 百分位數

# 顯示結果
mean_foodaway
median_foodaway
percentiles


#b.
install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(cex5_small)
data <- cex5_small

# 建立 education 變數
data$education <- ifelse(data$advanced == 1, "Advanced Degree",
                  ifelse(data$college == 1, "College Degree", "No Degree"))

# 檢查新變數
table(data$education)

# 計算不同教育程度的平均值、中位數、樣本數
mean_foodaway <- aggregate(foodaway ~ education, data, mean, na.rm = TRUE)
median_foodaway <- aggregate(foodaway ~ education, data, median, na.rm = TRUE)
count_foodaway <- aggregate(foodaway ~ education, data, length)

# 合併結果
summary_stats <- Reduce(function(x, y) merge(x, y, by = "education"), 
                        list(mean_foodaway, median_foodaway, count_foodaway))

# 重新命名欄位
colnames(summary_stats) <- c("Education", "Mean_FOODAWAY", "Median_FOODAWAY", "Sample_Size")

# 顯示結果
print(summary_stats)


#C
# 檢查是否有 0 或負值
sum(data$foodaway <= 0, na.rm = TRUE)

# 取自然對數，並排除 FOODAWAY ≤ 0 的情況
data <- data %>%
  filter(foodaway > 0) %>%
  mutate(ln_foodaway = log(foodaway))

# 檢查新變數
summary(data$ln_foodaway)

#繪製直方圖

library(ggplot2)

ggplot(data, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  labs(title = "ln(FOODAWAY) 的直方圖", x = "ln(FOODAWAY)", y = "頻數") +
  theme_minimal()


#計算統計數據
mean_ln_foodaway <- mean(data$ln_foodaway, na.rm = TRUE)
median_ln_foodaway <- median(data$ln_foodaway, na.rm = TRUE)
percentiles_ln <- quantile(data$ln_foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

# 顯示結果
mean_ln_foodaway
median_ln_foodaway
percentiles_ln

# 計算 FOODAWAY 的缺失值數量
missing_foodaway <- sum(is.na(data$foodaway))

# 計算 ln(FOODAWAY) 的缺失值數量
missing_ln_foodaway <- sum(is.na(data$ln_foodaway))

# 顯示結果
missing_foodaway
missing_ln_foodaway

#d.
data <- data[data$foodaway > 0, ]  # 移除 foodaway = 0 的觀測值
data$ln_foodaway <- log(data$foodaway)  # 取自然對數

# 建立線性回歸模型
model <- lm(ln_foodaway ~ income, data = data)

# 顯示回歸結果
summary(model)

#e.

data <- data[data$foodaway > 0, ]  # 移除 foodaway = 0 的觀測值
data$ln_foodaway <- log(data$foodaway)  # 取自然對數

# 建立線性回歸模型
model <- lm(ln_foodaway ~ income, data = data)

# 顯示回歸結果
summary(model)

#繪製 ln(FOODAWAY) 與 INCOME 散佈圖，並加入回歸線
library(ggplot2)

ggplot(data, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 添加回歸線
  labs(title = "ln(FOODAWAY) 與 INCOME 的回歸分析",
       x = "家庭月收入（$100 美元）",
       y = "ln(FOODAWAY)") +
  theme_minimal()

#f.
# 計算殘差
data$residuals <- residuals(model)

#繪製殘差圖
library(ggplot2)

ggplot(data, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  # 繪製散點圖
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 添加 y=0 參考線
  labs(title = "殘差與 INCOME 的關係",
       x = "家庭月收入（$100 美元）",
       y = "殘差 (ln(FOODAWAY))") +
  theme_minimal()

