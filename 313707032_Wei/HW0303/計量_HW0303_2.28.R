# 安裝並載入必要的套件
install.packages("ggplot2")
install.packages("dplyr")
install.packages("psych")

library(ggplot2)
library(dplyr)
library(psych)

# 讀取數據
load("cps5_small.rda")

# 檢查數據集
print(names(cps5_small))  # 確認變數名稱
str(cps5_small)           # 檢視數據結構
summary(cps5_small)       # 顯示摘要統計

#2.28 (a) 獲取 WAGE 和 EDUC 的統計摘要與直方圖
# 確保 WAGE 和 EDUC 是數值型
cps5_small$wage <- as.numeric(cps5_small$wage)
cps5_small$educ <- as.numeric(cps5_small$educ)

# 描述性統計
describe(cps5_small[, c("wage", "educ")])

# WAGE 直方圖
ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Histogram of wage",
       x = "Hourly Wage ($)",
       y = "Count") +
  theme_minimal()

# EDUC 直方圖
ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of EDUC",
       x = "Years of Education",
       y = "Count") +
  theme_minimal()

# 2.28 (b) 估計線性回歸 WAGE = β1 + β2 * EDUC + e
# 執行線性回歸
model <- lm(wage ~ educ, data = cps5_small)

# 顯示回歸結果
summary(model)

# 2.28c
# 計算 OLS 殘差
cps5_small$residuals <- residuals(model)

ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Education",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()

# 2.28d
# 針對男性（male = 1）
model_male <- lm(wage ~ educ, data = cps5_small, subset = (female == 0))
summary(model_male)

# 針對女性（female = 1）
model_female <- lm(wage ~ educ, data = cps5_small, subset = (female == 1))
summary(model_female)

# 針對黑人（black = 1）
model_black <- lm(wage ~ educ, data = cps5_small, subset = (black == 1))
summary(model_black)

# 針對白人（black = 0）
model_white <- lm(wage ~ educ, data = cps5_small, subset = (black == 0))
summary(model_white)


# 2.28e
# 估計二次回歸
model_quad <- lm(wage ~ I(educ^2), data = cps5_small)

# 顯示回歸結果
summary(model_quad)


# 2.28f
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5, color = "black") +  # 加入散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE, formula = y ~ x) +  # 線性回歸
  geom_smooth(method = "lm", color = "blue", se = FALSE, formula = y ~ poly(x, 2)) +  # 二次回歸
  labs(title = "Comparison of Linear and Quadratic Models",
       x = "Years of Education",
       y = "Hourly Wage (USD)") +
  theme_minimal()


