# 清除環境變數（可選）
rm(list=ls())

# 安裝並載入所需套件
library(POE5Rdata)
library(dplyr)
library(ggplot2)

# 載入資料
data("commute5")
#a
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)
#b
confint(model, level = 0.95)
#c
b_red <- summary(model)$coefficients["reds", "Estimate"]
se_red <- summary(model)$coefficients["reds", "Std. Error"]
t_red <- (b_red - 2) / se_red
df <- df.residual(model)
p_red <- pt(t_red, df)
cat("t =", t_red, ", p-value =", p_red)
#d
b_train <- summary(model)$coefficients["trains", "Estimate"]
se_train <- summary(model)$coefficients["trains", "Std. Error"]
t_train <- (b_train - 3) / se_train
p_train <- 2 * pt(-abs(t_train), df)
cat("t =", t_train, ", p-value =", p_train)
#e
b_depart <- summary(model)$coefficients["depart", "Estimate"]
se_depart <- summary(model)$coefficients["depart", "Std. Error"]
delta <- 30
t_depart <- (delta * b_depart - 10) / (delta * se_depart)
p_depart <- pt(t_depart, df)
cat("t =", t_depart, ", p-value =", p_depart)
#f
beta_diff <- b_train - 3 * b_red
se_diff <- sqrt(se_train^2 + 9 * se_red^2)  # 忽略共變異數（保守作法）
t_diff <- beta_diff / se_diff
p_diff <- pt(t_diff, df)
cat("t =", t_diff, ", p-value =", p_diff)
