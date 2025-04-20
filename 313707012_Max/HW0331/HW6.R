library(POE5Rdata)
data("commute5")

model <- lm(time ~ depart + reds + trains, data = commute5)

# 顯示回歸結果
summary(model)

# 計算 95% 信賴區間
conf_intervals <- confint(model, level = 0.95)
# 顯示信賴區間
print(conf_intervals)
cov_matrix <- vcov(model)
print(cov_matrix)

# 5.33
#a.
library(POE5Rdata)
data("cps5_small")

model_1 <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = cps5_small)

# 查看回歸結果
summary(model_1)

#c.
# 提取係數
beta2 <- coef(model_1)["educ"]
beta3 <- coef(model_1)["I(educ^2)"]
beta6 <- coef(model_1)["I(educ * exper)"]

# 計算每個觀測值的邊際效應
marginal_effect_educ <- beta2 + 2 * beta3 * cps5_small$educ + beta6 * cps5_small$exper
# 繪製直方圖
hist(marginal_effect_educ, breaks = 30, main = "邊際效應直方圖", xlab = "∂ln(WAGE)/∂EDUC", col = "lightblue")
# 計算中位數、第 5 百分位數和第 95 百分位數
quantiles <- quantile(marginal_effect_educ, probs = c(0.05, 0.5, 0.95))
cat("第 5 百分位數:", quantiles[1], "\n")
cat("中位數:", quantiles[2], "\n")
cat("第 95 百分位數:", quantiles[3], "\n")

#e.
# 提取係數
beta4 <- coef(model_1)["exper"]
beta5 <- coef(model_1)["I(exper^2)"]
beta6 <- coef(model_1)["I(educ * exper)"]

# 計算每個觀測值的邊際效應
marginal_effect_educ_e <- beta4 + 2 * beta5 * cps5_small$exper + beta6 * cps5_small$educ
# 繪製直方圖
hist(marginal_effect_educ_e, breaks = 30, main = "邊際效應直方圖", xlab = "∂ln(WAGE)/∂EDUC", col = "lightgreen")
# 計算中位數、第 5 百分位數和第 95 百分位數
quantiles <- quantile(marginal_effect_educ_e, probs = c(0.05, 0.5, 0.95))
cat("第 5 百分位數:", quantiles[1], "\n")
cat("中位數:", quantiles[2], "\n")
cat("第 95 百分位數:", quantiles[3], "\n")

#f.
beta1 <- coef(model_1)["(Intercept)"]
#計算差值 Delta
# Delta = -beta2 - 33*beta3 + 10*beta4 + 260*beta5 + 152*beta6
delta <- beta2 + 33 * beta3 - 10 * beta4 - 260 * beta5 - 152 * beta6

# 定義線性組合向量 c
c_vec <- c(0, 1, 33, -10, -260, -152)  
# 提取回歸係數的協方差矩陣
vcov_matrix <- vcov(model_1)
# 計算 Delta 的方差
var_delta <- t(c_vec) %*% vcov_matrix %*% c_vec
# 計算 Delta 的標準誤
se_delta <- sqrt(var_delta)
# 計算 t 統計量
t_stat <- delta / se_delta

# 計算 p 值（右尾檢驗）
p_value <- pnorm(t_stat, lower.tail = FALSE)

# 輸出結果
cat("差值:", delta, "\n")
cat("標準誤:", se_delta, "\n")
cat("t 統計量:", t_stat, "\n")
cat("p 值:", p_value, "\n")


#g.
#計算差值 Delta
# Delta = -beta2 - 33*beta3 + 10*beta4 + 260*beta5 + 152*beta6
delta_g <- beta2 + 33 * beta3 - 10 * beta4 - 420 * beta5 - 144 * beta6

# 定義線性組合向量 c
c_vec_g <- c(0, 1, 33, -10, -420, -144)  
# 提取回歸係數的協方差矩陣
vcov_matrix <- vcov(model_1)
# 計算 Delta 的方差
var_delta_g <- t(c_vec_g) %*% vcov_matrix %*% c_vec_g
# 計算 Delta 的標準誤
se_delta_g <- sqrt(var_delta_g)

# 計算 t 統計量
t_stat_g <- delta_g / se_delta_g

# 計算 p 值（右尾檢驗）
p_value_g <- pnorm(t_stat_g, lower.tail = FALSE)

# 輸出結果
cat("差值:", delta_g, "\n")
cat("標準誤:", se_delta_g, "\n")
cat("t 統計量:", t_stat_g, "\n")
cat("p 值:", p_value_g, "\n")

#h.
#計算差值 Delta
# Delta = -beta2 - 33*beta3 + 10*beta4 + 260*beta5 + 152*beta6
delta_h <- 12 * beta5 - 4 * beta6

# 定義線性組合向量 c
c_vec_h <- c(0, 0, 0, 0, 12, -4)  
# 提取回歸係數的協方差矩陣
vcov_matrix <- vcov(model_1)
# 計算 Delta 的方差
var_delta_h <- t(c_vec_h) %*% vcov_matrix %*% c_vec_h
# 計算 Delta 的標準誤
se_delta_h <- sqrt(var_delta_h)

# 計算 t 統計量
t_stat_h <- delta_h / se_delta_h

# 計算 p 值（右尾檢驗）
p_value_h <- pnorm(t_stat_h, lower.tail = FALSE)

# 輸出結果
cat("差值:", delta_h, "\n")
cat("標準誤:", se_delta_h, "\n")
cat("t 統計量:", t_stat_h, "\n")
cat("p 值:", p_value_h, "\n")

#i
# 計算 theta
theta <- -(beta4 + 16 * beta6) / (2 * beta5)-11

# 使用 Delta 方法計算 theta 的標準誤
grad <- c(0, 0, 0, -1 / (2 * beta5), (beta4 + 16 * beta6) / (2 * beta5^2), -8 / beta5)

# 計算 theta 的方差
var_theta <- t(grad) %*% vcov_matrix %*% grad

# 計算標準誤
se_theta <- sqrt(var_theta)

# 計算 95% 置信區間
ci_lower <- theta - 1.96 * se_theta
ci_upper <- theta + 1.96 * se_theta

# 輸出結果
cat("再過多久經驗邊際效應變為負值:", theta, "\n")
cat("95% 置信區間: [", ci_lower, ", ", ci_upper, "]\n")
