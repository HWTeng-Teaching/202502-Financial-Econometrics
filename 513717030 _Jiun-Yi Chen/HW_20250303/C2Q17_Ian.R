#a
install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(collegetown)

class(collegetown)  # 確認變數類型

data <- collegetown
str(data)

library(ggplot2)
ggplot(collegetown, aes(x = sqft, y = price)) +  # 改用小寫變數名稱
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "房屋價格與房屋面積的關係",
       x = "房屋面積（百平方英尺）",
       y = "房價（千美元）") +
  theme_minimal()


#b.
# 執行線性回歸
model <- lm(price ~ sqft, data = collegetown)

# 顯示回歸結果
summary(model)

# 繪製散佈圖與迴歸線
library(ggplot2) #載入ggplot2

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  # 繪製散點
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 移除灰色置信區間
  labs(title = "房屋價格與房屋面積的回歸分析",
       x = "房屋面積（百平方英尺）",
       y = "房價（千美元）") +
  theme_minimal()

#geom_smooth會有


