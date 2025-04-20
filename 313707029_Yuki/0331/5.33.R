# 清除環境變數（可選）
rm(list=ls())

# 安裝並載入所需套件
library(POE5Rdata)
library(dplyr)
library(ggplot2)

# 載入資料
data("cps5_small")
#a
# 建立模型
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = cps5_small)
summary(model)
#c
b <- coef(model)
cps5_small$me_educ <- b[2] + 2 * b[3] * cps5_small$educ + b[6] * cps5_small$exper
quantiles <- quantile(cps5_small$me_educ, probs = c(0.05, 0.5, 0.95))
cat("第 5 百分位數:", quantiles[1], "\n")
cat("中位數:", quantiles[2], "\n")
cat("第 95 百分位數:", quantiles[3], "\n")

# Histogram
hist(cps5_small$me_educ, main = "Marginal Effect of EDUC", xlab = "∂ln(wage)/∂educ")
#e
# 建立 exper 的邊際效果
cps5_small$me_exper <- b[4] + 2 * b[5] * cps5_small$exper + b[6] * cps5_small$educ

# Histogram
hist(cps5_small$me_exper, main = "Marginal Effect of EXPER", xlab = "∂ln(wage)/∂exper")

# 分位數
quantile(cps5_small$me_exper, probs = c(0.05, 0.5, 0.95))

# 建立兩人特徵向量（截距、educ, educ^2, exper, exper^2, educ*exper）
x_david <- c(1, 17, 17^2, 8, 8^2, 17*8)
x_svet  <- c(1, 16, 16^2, 18, 18^2, 16*18)
#f
# 預測差異
y_diff <- sum(x_david * b) - sum(x_svet * b)

# 標準誤（需用共變異數矩陣）
vcv <- vcov(model)
x_delta <- x_david - x_svet
se_diff <- sqrt(t(x_delta) %*% vcv %*% x_delta)

# 檢定統計量（單尾）
t_val <- y_diff / se_diff
p_val <- pt(t_val, df = df.residual(model), lower.tail = FALSE)

cat("差異（David - Svetlana）:", y_diff, "\n")
cat("t 值:", round(t_val, 4), "；p 值:", round(p_val, 4), "\n")

# g
# 更新經驗值（educ 不變）
x_david2 <- c(1, 17, 17^2, 16, 16^2, 17*16)
x_svet2  <- c(1, 16, 16^2, 26, 26^2, 16*26)

# 預測差異與標準誤
y_diff2 <- sum(x_david2 * b) - sum(x_svet2 * b)
x_delta2 <- x_david2 - x_svet2
se_diff2 <- sqrt(t(x_delta2) %*% vcv %*% x_delta2)

# t 與 p 值
t_val2 <- y_diff2 / se_diff2
p_val2 <- pt(t_val2, df = df.residual(model), lower.tail = FALSE)

cat("8 年後差異（David - Svetlana）:", y_diff2, "\n")
cat("t 值:", round(t_val2, 4), "；p 值:", round(p_val2, 4), "\n")


# h
# 建立特徵向量（針對 exper 邊際效果）
# me_exper = β4 + 2β5 * exper + β6 * educ

me_wendy <- b[4] + 2 * b[5] * 17 + b[6] * 12
me_jill  <- b[4] + 2 * b[5] * 11 + b[6] * 16
me_diff  <- me_wendy - me_jill

# 對應導數向量（β4, β5, β6 位置為 4, 5, 6）
grad_wendy <- c(0, 0, 0, 1, 2*17, 12)
grad_jill  <- c(0, 0, 0, 1, 2*11, 16)
grad_diff  <- grad_wendy - grad_jill

# 標準誤
se_me_diff <- sqrt(t(grad_diff) %*% vcv %*% grad_diff)

# t 與 p 值（雙尾）
t_val_h <- me_diff / se_me_diff
p_val_h <- 2 * pt(abs(t_val_h), df = df.residual(model), lower.tail = FALSE)

cat("邊際效果差（Wendy - Jill）:", me_diff, "\n")
cat("t 值:", round(t_val_h, 4), "；p 值:", round(p_val_h, 4), "\n")


# i
# 計算 exper 的轉折點（使 me = 0）
numerator <- -(b[4] + b[6] * 16)
denominator <- 2 * b[5]
x_jill_zero <- numerator / denominator
cat("邊際效果變負的 exper 年數:", x_jill_zero, "\n")

# Delta method：對參數做微分
# f(β) = -(β4 + 16*β6) / (2*β5)

# 對 β4, β5, β6 的偏微分
# df/dβ4 = -1 / (2β5), df/dβ5 = (β4 + 16β6) / (2β5^2), df/dβ6 = -16 / (2β5)

grad_i <- c(
  0, 0, 0,
  -1 / (2 * b[5]),
  (b[4] + 16 * b[6]) / (2 * b[5]^2),
  -16 / (2 * b[5])
)

# 信賴區間
se_i <- sqrt(t(grad_i) %*% vcv %*% grad_i)
ci_i <- x_jill_zero + c(-1.96, 1.96) * se_i

cat("95% 信賴區間為:", round(ci_i[1], 4), "~", round(ci_i[2], 4), "\n")

