library(ggplot2)

data("cps5_small", package = "POE5Rdata")
names(cps5_small)

#a.

# 檢查數據結構
summary(cps5_small$wage)
summary(cps5_small$educ)

# 直方圖: wage
ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(color = "black", fill = "blue", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of Wage",
       x = "Hourly Wage",
       y = "Frequency") +
  theme_minimal()

# 直方圖: educ
ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(color = "black", fill = "red", bins = 30, alpha = 0.7) +
  labs(title = "Histogram of Education",
       x = "Years of Education",
       y = "Frequency") +
  theme_minimal()

#b.

# 擬合線性回歸模型
linear_model <- lm(wage ~ educ, data = cps5_small)

# 顯示回歸結果
summary(linear_model)

#c.

# 計算殘差
residuals_linear <- resid(linear_model)

# 繪製殘差 vs. educ
ggplot(cps5_small, aes(x = educ, y = residuals_linear)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +  # y=0 參考線
  labs(title = "Residuals Plot: Wage vs. Education",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()

#d.

# 添加男性變數 (male = 1 - female)
cps5_small$male <- 1 - cps5_small$female

# 擬合回歸模型 (男性)
linear_model_male <- lm(wage ~ educ, data = subset(cps5_small, male == 1))
summary(linear_model_male)

# 擬合回歸模型 (女性)
linear_model_female <- lm(wage ~ educ, data = subset(cps5_small, female == 1))
summary(linear_model_female)

# 擬合回歸模型 (黑人)
linear_model_black <- lm(wage ~ educ, data = subset(cps5_small, black == 1))
summary(linear_model_black)

# 擬合回歸模型 (白人)
linear_model_white <- lm(wage ~ educ, data = subset(cps5_small, black == 0))
summary(linear_model_white)

#e.

# 擬合二次回歸模型 (僅二次項)
quad_model <- lm(wage ~ I(educ^2), data = cps5_small)

# 顯示回歸結果
summary(quad_model)

# 計算 12 年和 16 年教育的邊際影響 (微分)
edu_12 <- 12
edu_16 <- 16
coef_quad <- coef(quad_model)

# 邊際影響 = 對 wage 取偏導數，即 2 * alpha_2 * educ
marginal_effect_12 <- 2 * coef_quad[2] * edu_12
marginal_effect_16 <- 2 * coef_quad[2] * edu_16

print(paste("Marginal effect at 12 years:", marginal_effect_12))
print(paste("Marginal effect at 16 years:", marginal_effect_16))

#f.

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue", alpha = 0.6) +  # 原始數據
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  # 線性回歸
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "green", se = FALSE) +  # 二次回歸
  labs(title = "Wage vs. Education: Linear and Quadratic Fit",
       x = "Years of Education",
       y = "Hourly Wage") +
  theme_minimal()

