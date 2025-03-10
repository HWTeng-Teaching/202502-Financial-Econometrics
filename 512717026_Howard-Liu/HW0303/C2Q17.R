#a
#install.packages("remotes")  # 確保 remotes 套件已安裝
#remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
library(ggplot2)

data(collegetown)
data <- collegetown
str(data)


ggplot(collegetown, aes(x = sqft, y = price)) + 
  geom_point(color = "green", alpha = 0.8) +
  labs(title = "房屋價格v.s.房屋面積",
       x = "房面積（百平方英尺）",
       y = "房價（千美元）") +
  theme_minimal() +
  theme(text=element_text(family = "黑體-繁 中黑"))


#b.
# 執行線性回歸
model <- lm(price ~ sqft, data = collegetown)

# 顯示回歸結果
summary(model)

# 繪製散佈圖與迴歸線
#library(ggplot2) #載入ggplot2

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "red", alpha = 0.6) +  
  geom_smooth(method = "lm", color = "green", se = FALSE) + 
  labs(title = "房屋價格 v.s.房屋面積之回歸分析",
       x = "房面積（百平方英尺）",
       y = "房價（千美元）") +
  theme_minimal() +
  theme(text=element_text(family = "黑體-繁 中黑"))

#c
quad_model <- lm(price ~ I(sqft^2), data = collegetown)
summary(quad_model)

alpha2 <- coef(quad_model)[2]  # 提取 α2 係數
sqft_value <- 20  # 2000 平方英尺，因為單位是百平方英尺
marginal_effect <- 2 * alpha2 * sqft_value
marginal_effect  # 顯示邊際影響

ggplot(data, aes(x = sqft, y = price)) +
  geom_point(alpha = 0.5) +  # 繪製散點圖
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # 繪製回歸曲線
  ggtitle("Quadratic Regression: 房屋價格 v.s.房屋面積") +
  xlab("房面積（百平方英尺）") +
  ylab("房價（千美元）") +
  theme_minimal() +
  theme(text=element_text(family = "黑體-繁 中黑"))

#d 繪製 sqrt = 20 的切線
sqft_target <- 20
price_target <- coef(quad_model)[1] + coef(quad_model)[2] * (sqft_target^2)

# 計算該點的切線斜率
slope_tangent <- 2 * alpha2 * sqft_value
slope_tangent  # 顯示斜率

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(alpha = 0.5) +  # 繪製散點圖
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +  # 二次回歸曲線
  geom_abline(intercept = price_target - slope_tangent * sqft_target, 
              slope = slope_tangent, color = "red", linetype = "dashed") +  # 切線
  geom_point(aes(x = sqft_target, y = price_target), color = "red", size = 3) +  # 標示 2000 平方英尺點
  ggtitle("Quadratic Regression with Tangent Line at SQFT=20") +
  xlab("房面積（百平方英尺）") +
  ylab("房價（千美元）") +
  theme_minimal() +
  theme(text=element_text(family = "黑體-繁 中黑"))

#e
# 取得平方項係數 α2
alpha2 <- coef(quad_model)[2]  
# 設定 2000 平方英尺 (20 百平方英尺)
sqft_value <- 20  
# 計算該點的邊際影響 (dPRICE/dSQFT)
marginal_effect <- 2 * alpha2 * sqft_value
# 計算該點的房價
price_target <- coef(quad_model)[1] + coef(quad_model)[2] * (sqft_value^2)
# 計算彈性
elasticity <- (marginal_effect * sqft_value) / price_target
elasticity  # 顯示彈性值
