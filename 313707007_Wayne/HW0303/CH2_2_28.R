#(a)

library(ggplot2)

# Wage 直方圖
ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Wage", x = "wage", y = "freq") +
  theme_minimal()

# Education 直方圖
ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "EDUC", x = "educ", y = "freq") +
  theme_minimal()

#(b)
# 執行線性回歸
model <- lm(wage ~ educ, data = cps5_small)

# 顯示回歸結果
summary(model)

#(c)

# 計算殘差
cps5_small$residuals <- residuals(model)
cps5_small$residuals

ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "relationships between residuals and EDUC", x = "EDUC", y = "Residuals") +
  theme_minimal()

#(d)
# 對男性進行回歸
male_model <- lm(wage ~ educ, data = subset(cps5_small, female == 0))
summary(male_model)

# 對女性進行回歸
female_model <- lm(wage ~ educ, data = subset(cps5_small, female == 1))
summary(female_model)

# 對黑人進行回歸
black_model <- lm(wage ~ educ, data = subset(cps5_small, black == 1))
summary(black_model)

# 對白人進行回歸
white_model <- lm(wage ~ educ, data = subset(cps5_small, black == 0))
summary(white_model)

#(e)
# 進行二次回歸
quad_model <- lm(wage ~ I(educ^2), data = cps5_small)

# 顯示回歸結果
summary(quad_model)

#(f)
# 執行線性回歸
linear_model <- lm(wage ~ educ, data = cps5_small)

# 執行二次回歸 (只有 educ^2，無 educ)
quad_model <- lm(wage ~ I(educ^2), data = cps5_small)
# 建立數據框，包含教育年數、實際薪資、線性預測值與二次回歸預測值
cps5_small$fitted_linear <- predict(linear_model)
cps5_small$fitted_quad <- predict(quad_model)

# 繪製散點圖 + 擬合線
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5, color = "gray") +  # 實際數據點
  geom_line(aes(y = fitted_linear), color = "blue", size = 1, linetype = "dashed") + # 線性回歸
  geom_line(aes(y = fitted_quad), color = "red", size = 1) +  # 二次回歸
  labs(title = "linear V.S. quadratic",
       x = "EDUC", y = "WAGE") +
  scale_color_manual(values = c("blue", "red"), labels = c("線性回歸", "二次回歸")) +
  theme_minimal()
