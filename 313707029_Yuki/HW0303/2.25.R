# 安裝必要的套件
#install.packages("devtools")  # 安裝 devtools
#library(devtools)             # 載入 devtools
#devtools::install_github("ccolonescu/POE5Rdata")#下載資料
#install.packages("dplyr")
#install.packages("ggplot2")

# 清除環境變數（可選）
rm(list=ls())

# 安裝與載入必要套件
library(dplyr)
library(ggplot2)
library(POE5Rdata)

# 讀取 cex5_small 數據集
data("cex5_small") 

# 計算相對頻率（百分比）
hist_data <- hist(cex5_small$foodaway, breaks = 20, plot = FALSE)  # 不繪圖，只計算數據
relative_freq <- hist_data$counts / sum(hist_data$counts) * 100  # 將頻率轉為百分比

# 繪製直方圖
barplot(
  relative_freq, 
  names.arg = hist_data$mids,  # 使用分組的中點作為 X 軸標籤
  main = "Figure xr2-25a Histogram of FOODAWAY",  # 標題
  xlab = "food away from home expenditure per month per person past quarter, $",  # X 軸標籤
  ylab = "Percent",  # Y 軸標籤
  ylim = c(0, 60)    # 調整 Y 軸範圍
)

# 計算描述統計
summary(cex5_small$foodaway)

#----------------------------------------------------------(a)

# 計算不同學歷群體的 FOODAWAY 平均值與中位數
summary_foodaway <- cex5_small %>%
  mutate(education_category = case_when(
    advanced == 1 ~ "advanced = 1",
    college == 1 ~ "college = 1",
    advanced == 0 & college == 0 ~ "None"
  )) %>%
  group_by(education_category) %>%
  summarise(
    N = n(),  # 將 count 改為 N
    MEAN = mean(foodaway, na.rm = TRUE),  # 將 mean_foodaway 改為 MEAN
    MEDIAN = median(foodaway, na.rm = TRUE)  # 將 median_foodaway 改為 MEDIAN
  )

# 顯示結果
print(summary_foodaway)

#------------------------------------------------------------------(b)

# 創建 ln_foodaway 變數 (避免對0或負值取對數)
cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, 
                                 log(cex5_small$foodaway), 
                                 NA)

# 計算有效觀察值數量
n_foodaway <- sum(!is.na(cex5_small$foodaway))
n_ln_foodaway <- sum(!is.na(cex5_small$ln_foodaway))
n_zeros <- sum(cex5_small$foodaway == 0, na.rm = TRUE)

# 輸出觀察值數量
cat("foodaway 觀察值數量:", n_foodaway, "\n")
cat("ln_foodaway 觀察值數量:", n_ln_foodaway, "\n")
cat("foodaway 為零的觀察值數量:", n_zeros, "\n")
cat("因取對數而移除的觀察值數量:", n_foodaway - n_ln_foodaway, "\n\n")

#-------------------------------------------------------------------(c)

# 估計線性回歸模型
model <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model)

#-------------------------------------------------------------------(d)

# 繪製 ln(FOODAWAY) 與 ln(INCOME) 的散點圖，並加上擬合線
ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Figure xr2.25(d) Scatter plot of ln(FOODAWAY) vs. ln(INCOME)",
       x = "ln(INCOME)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

# 計算殘差
residuals <- model$residuals

# 清理缺失值
cex5_small <- cex5_small %>%
  filter(!is.na(ln_foodaway), !is.na(income))

# 繪製殘差圖
ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Income",
       x = "Income",
       y = "Residuals") +
  theme_minimal()

#----------------------------------------------------------------(e)



