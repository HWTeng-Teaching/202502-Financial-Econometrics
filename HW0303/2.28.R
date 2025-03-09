# 清除環境變數（可選）
rm(list=ls())

library(POE5Rdata)
data("cps5_small")

# 計算 WAGE 的摘要統計量
summary_wage <- cps5_small %>%
  summarise(
    N = n(),
    Mean = mean(wage, na.rm = TRUE),
    Median = median(wage, na.rm = TRUE),
    SD = sd(wage, na.rm = TRUE),
    Min = min(wage, na.rm = TRUE),
    Max = max(wage, na.rm = TRUE)
  )

# 計算 EDUC 的摘要統計量  
summary_educ <- cps5_small %>%
  summarise(
    N = n(),
    Mean = mean(educ, na.rm = TRUE),
    Median = median(educ, na.rm = TRUE),
    SD = sd(educ, na.rm = TRUE),
    Min = min(educ, na.rm = TRUE),
    Max = max(educ, na.rm = TRUE)
  )

# 顯示結果
print(summary_wage)
print(summary_educ)

#------------------------------------------------(a)

mod1 <- lm(wage ~ educ, data = cps5_small)
summary(mod1)
cat(b1 <- coef(mod1)[[1]])
cat(b2 <- coef(mod1)[[2]])

#-------------------------------------------------(b)

# 計算最小平方殘差
residuals <- resid(mod1)

# 將殘差添加到原始數據框中
cps5_small$residuals <- residuals

# 繪製殘差與 EDUC 的散點圖
plot(cps5_small$educ, residuals,
     main = "Residuals vs. Education",
     xlab = "Education (years)",
     ylab = "Residuals",
     pch = 20,
     col = "blue")

#---------------------------------------------------(C)

# 按性別分組
cps5_male <- cps5_small[cps5_small$female == 0, ]
cps5_female <- cps5_small[cps5_small$female == 1, ]

# 按種族分組
cps5_white <- cps5_small[cps5_small$black == 0, ]
cps5_black <- cps5_small[cps5_small$black == 1, ]

# 男性、女性、白人、黑人
mod1_male <- lm(wage ~ educ, data = cps5_male)
mod1_female <- lm(wage ~ educ, data = cps5_female)
mod1_white <- lm(wage ~ educ, data = cps5_white)
mod1_black <- lm(wage ~ educ, data = cps5_black)
summary(mod1_male)
summary(mod1_female)
summary(mod1_white)
summary(mod1_black)

#---------------------------------------------------(d)

# 只包含 EDUC^2 項的二次回歸模型
mod2 <- lm(wage ~ I(educ^2), data = cps5_small)
summary(mod2)

# 計算邊際效應
# 在 wage = β₀ + β₁(educ²) 模型中，邊際效應 = 2*β₁*educ
# 獲取係數
a2 <- coef(mod2)["I(educ^2)"]
a2

# 計算特定教育年數的邊際效應
me_12 <- 2 * a2 * 12
me_16 <- 2 * a2 * 16

# 從線性模型獲取邊際效應 (在線性模型中，邊際效應就是係數)
me_linear <- coef(mod1)["educ"]
me_linear

# 顯示結果
cat("只有平方項的模型中，12年教育的邊際效應:", round(me_12, 4), "\n")
cat("只有平方項的模型中，16年教育的邊際效應:", round(me_16, 4), "\n")
cat("線性模型中的邊際效應:", round(me_linear, 4), "\n")

#-------------------------------------------------------------------(e)

# 繪製線性模型和二次模型擬合值與原始數據的散點圖

# 創建用於預測的教育年數序列
educ_seq <- seq(min(cps5_small$educ), max(cps5_small$educ), by = 0.1)

# 預測線性模型的擬合值
linear_pred <- predict(mod1, newdata = data.frame(educ = educ_seq))

# 預測二次模型的擬合值
quad_pred <- predict(mod2, newdata = data.frame(educ = educ_seq))

# 使用基本繪圖
plot(cps5_small$educ, cps5_small$wage, 
     main = "工資與教育關係：線性模型 vs. 二次模型", 
     xlab = "教育年數", 
     ylab = "工資率",
     pch = 20, 
     col = "gray")

# 添加線性模型擬合線
lines(educ_seq, linear_pred, col = "blue", lwd = 2)

# 添加二次模型擬合線
lines(educ_seq, quad_pred, col = "red", lwd = 2)

# 添加圖例
legend("topleft", 
       legend = c("原始數據", "線性模型", "二次模型"), 
       col = c("gray", "blue", "red"), 
       pch = c(20, NA, NA), 
       lty = c(NA, 1, 1),
       lwd = c(NA, 2, 2))

