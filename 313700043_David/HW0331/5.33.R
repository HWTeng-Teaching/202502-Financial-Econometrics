rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cps5_small

# a.

model1 <- lm(log(wage)~educ+I(educ^2)+exper+I(exper^2)+I(educ*exper), cps5_small)
smodel1 <- summary(model1)
smodel1

# c.

b2 <- coef(model1)[2]
b3 <- coef(model1)[3]
b6 <- coef(model1)[6]

marginal_educ <- b2 + 2 * b3 * cps5_small$educ + b6 * cps5_small$exper
marginal_educ

median_effect <- median(marginal_educ, TRUE)
p5_effect <- quantile(marginal_educ, 0.05, TRUE)
p95_effect <- quantile(marginal_educ, 0.95, TRUE)

# Step 4: 印出結果
cat("median marginal effect of educ:", round(median_effect, 4))
cat("5th percentile:", round(p5_effect, 4))
cat("95th percentile:", round(p95_effect, 4))

# （選用）Step 5: 畫出直方圖
hist(marginal_educ, 
     main = "Histogram of Marginal Effect of EDUC on ln(WAGE)", 
     xlab = "Marginal Effect", 
     col = "skyblue", 
     border = "white")

# e.

b4 <- coef(model1)[4]
b5 <- coef(model1)[5]
b6 <- coef(model1)[6]

marginal_exper <- b4 + 2 * b5 * cps5_small$exper + b6 * cps5_small$educ
marginal_exper

median_effect_exper <- median(marginal_exper, TRUE)
p5_effect_exper <- quantile(marginal_exper, 0.05, TRUE)
p95_effect_exper <- quantile(marginal_exper, 0.95, TRUE)

cat("median marginal effect of exper:", round(median_effect_exper, 4))
cat("5th percentile of exper:", round(p5_effect_exper, 4))
cat("95th percentile of exper:", round(p95_effect_exper, 4))

hist(marginal_exper, 
     main = "Histogram of Marginal Effect of EXPER on ln(WAGE)", 
     xlab = "Marginal Effect", 
     col = "skyblue", 
     border = "white")

# f.

# H0：Svetlana 的期望 log-wage 大於等於 David
# H1：David 的期望 log-wage 更高
#model:ln(WAGE)=β1+β2*⋅EDUC+β3*EDUC^2+β4*EXPER+β5*EXPER^2+β6*(EDUC⋅EXPER)

# 建立模型向量 (intercept, EDUC, EDUC^2, EXPER, EXPER^2, EDUC*EXPER)
x_david <- c(1, 17, 17^2, 8, 8^2, 17*8)
x_svet  <- c(1, 16, 16^2, 18, 18^2, 16*18)

# 差值向量（x_d - x_s）
x_diff <- x_david - x_svet

# 提取模型係數與共變異數矩陣
b <- coef(model1)
vcov_mat <- vcov(model1)

# 計算期望 log-wage 差異（估計值）
delta_hat <- sum(x_diff * b)

# 計算標準誤
se_delta <- sqrt(t(x_diff) %*% vcov_mat %*% x_diff)

# 計算 t 統計量
t_stat <- delta_hat / se_delta

# 印出檢定統計量與結論
cat("Estimated difference (David - Svetlana):", round(delta_hat, 4), "\n")
cat("Standard error of difference:", round(se_delta, 4), "\n")
cat("t-statistic:", round(t_stat, 4), "\n")

# 臨界值 (one-tailed test, 5%)
crit_value <- qt(0.95, df = model1$df.residual)
cat("Critical value (5% level):", round(crit_value, 4), "\n")

if (t_stat > crit_value) {
  cat("Conclusion: Reject H0. David is expected to earn more.\n")
} else {
  cat("Conclusion: Fail to reject H0. Not enough evidence that David earns more.\n")
}

# g.
x_david <- c(1, 17, 17^2, 16, 16^2, 17*16)
x_svet  <- c(1, 16, 16^2, 26, 26^2, 16*26)

# 差值向量（x_d - x_s）
x_diff <- x_david - x_svet

# 提取模型係數與共變異數矩陣
b <- coef(model1)
vcov_mat <- vcov(model1)

# 計算期望 log-wage 差異（估計值）
delta_hat <- sum(x_diff * b)

# 計算標準誤
se_delta <- sqrt(t(x_diff) %*% vcov_mat %*% x_diff)

# 計算 t 統計量
t_stat <- delta_hat / se_delta

# 印出檢定統計量與結論
cat("Estimated difference (David - Svetlana):", round(delta_hat, 4), "\n")
cat("Standard error of difference:", round(se_delta, 4), "\n")
cat("t-statistic:", round(t_stat, 4), "\n")

# 臨界值 (one-tailed test, 5%)
crit_value <- qt(0.95, df = model1$df.residual)
cat("Critical value (5% level):", round(crit_value, 4), "\n")

if (t_stat > crit_value) {
  cat("Conclusion: Reject H0. David is expected to earn more.\n")
} else {
  cat("Conclusion: Fail to reject H0. Not enough evidence that David earns more.\n")
}

# h.
#對 EXPER 的偏導數為：β4+2*β5*exper+β6*educ
#邊際效果：M_w = β4+2*β5*17+β6*12
#邊際效果：M_j = β4+2*β5*1+β6*16
# H0: M_w = M_j vs. H1: M_w != M_j

# Marginal effect for: beta_4 + 2*beta_5*EXPER + beta_6*EDUC

# Wendy: 12 educ, 17 exper → 0*intercept + 0*educ + 0*educ2 + 1*exper + 2*17*exper2 + 12*educ*exper
c_wendy <- c(0, 0, 0, 1, 2*17, 12)

# Jill: 16 educ, 1 exper → 0*intercept + 0*educ + 0*educ2 + 1*exper + 2*1*exper2 + 16*educ*exper
c_jill  <- c(0, 0, 0, 1, 2*1, 16)

# 差值（H0: 差 = 0）
c_diff <- c_wendy - c_jill

# 模型係數與共變異數矩陣
b <- coef(model1)
vcov_mat <- vcov(model1)

# 差異估計值
delta_hat <- sum(c_diff * b)

# 標準誤
se_delta <- sqrt(t(c_diff) %*% vcov_mat %*% c_diff)

# t 統計量
t_stat <- delta_hat / se_delta

# 臨界值
crit_val <- qt(0.975, df = model1$df.residual)  

# 輸出
cat("Estimated difference in marginal effects:", round(delta_hat, 4), "\n")
cat("Standard error:", round(se_delta, 4), "\n")
cat("t-statistic:", round(t_stat, 4), "\n")
cat("Critical value (5% level): ±", round(crit_val, 4), "\n")

if (abs(t_stat) > crit_val) {
  cat("Conclusion: Reject H0. Marginal effects are significantly different.\n")
} else {
  cat("Conclusion: Fail to reject H0. No significant difference in marginal effects.\n")
}

# i

b4 <- coef(model1)[4]
b5 <- coef(model1)[5]
b6 <- coef(model1)[6]

# 計算臨界年數（使 Jill 的 marginal effect = 0）
x_crit <- - (b4 + b6 * 16) / (2 * b5)

cat("Point estimate of experience when marginal effect becomes negative:", round(x_crit, 2), "years\n")

# 對應參數向量：theta = [b4, b5, b6]
# 函數 g(theta) = - (b4 + 16*b6) / (2*b5)
# 偏微分：∂g/∂b4 = -1/(2*b5), ∂g/∂b5 = (b4 + 16*b6)/(2*b5^2), ∂g/∂b6 = -16/(2*b5)

d_b4 <- -1 / (2 * b5)
d_b5 <- (b4 + 16 * b6) / (2 * b5^2)
d_b6 <- -16 / (2 * b5)

# 將這三個導數組成梯度向量
grad <- c(d_b4, d_b5, d_b6)

# 取出這三個參數的變異數–共變異數矩陣
vcov_sub <- vcov(model1)[c("exper", "I(exper^2)", "I(educ * exper)"),c("exper", "I(exper^2)", "I(educ * exper)") ]
vcov_sub
# delta method 計算標準誤
se_xcrit <- sqrt(t(grad) %*% vcov_sub %*% grad)

# 95% 信賴區間
crit_val <- qt(0.975, df = model1$df.residual)
lower_bound <- x_crit - crit_val * se_xcrit
upper_bound <- x_crit + crit_val * se_xcrit

cat("95% confidence interval for turning point: [", round(lower_bound, 2), ",", round(upper_bound, 2), "] years\n")

