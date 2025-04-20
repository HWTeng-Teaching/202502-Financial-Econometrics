#CH5Q31
library(POE5Rdata)
data(commute5)
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

#(b)
confint(model, level = 0.95)
#(c)
# 提取係數與標準誤
b3 <- coef(summary(model))["reds", "Estimate"]
se3 <- coef(summary(model))["reds", "Std. Error"]
t_stat <- (b3 - 2) / se3
print(t_stat)
# 計算 p-value
df <- df.residual(model)
pval <- pt(t_stat, df = df)  # 單尾左側檢定
print(pval)

#(d)
b4 <- coef(summary(model))["trains", "Estimate"]
se4 <- coef(summary(model))["trains", "Std. Error"]
t_stat <- (b4 - 3) / se4
pval <- 2 * pt(-abs(t_stat), df = df)  # 雙尾檢定
print(t_stat)
#(e)
b2 <- coef(summary(model))["depart", "Estimate"]
se2 <- coef(summary(model))["depart", "Std. Error"]
t_stat <- (30 * b2 - 10) / (30 * se2)
pval <- pt(t_stat, df = df)  # 單尾左側檢定
print(t_stat)

#(f)
# beta4 - 3*beta3
theta <- b4 - 3 * b3

# 標準誤
var_b3 <- vcov(model)["reds", "reds"]
var_b4 <- vcov(model)["trains", "trains"]
cov_b3b4 <- vcov(model)["reds", "trains"]
se_theta <- sqrt(var_b4 + 9 * var_b3 - 6 * cov_b3b4)
print(se_theta)
t_stat <- theta / se_theta
pval <- pt(t_stat, df = df)  # 單尾左側
print(t_stat)

#(e)
x <- c(1, 30, 6, 1)  # 截距 + DEPART + REDS + TRAINS
beta_hat <- coef(model)
y_hat <- sum(beta_hat * x)

# 計算預測值的標準誤
se_pred <- sqrt(t(x) %*% vcov(model) %*% x)
t_stat <- (y_hat - 45) / se_pred
pval <- pt(t_stat, df = df)
print(t_stat)

#CH5Q33
data(cps5_small)
# 估計模型
# log(WAGE) = B1 + B2*EDUC + B3*EDUC^2 + B4*EXPER + B5*EXPER^2 + B6*(EDUC*EXPER) + e
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + educ:exper, data = cps5_small)

# 顯示模型摘要
model_summary <- summary(model)
print(model_summary)

#(a)
# 提取係數、標準誤、t 值和 p 值
coefficients_summary <- coef(model_summary)

# 顯示每個係數的 p 值
p_values <- coefficients_summary[, "Pr(>|t|)"]
cat("各係數估計值的 p 值:\n")
print(p_values)

cat("\n解釋:\n")
cat("例如，如果一個係數的 p 值為 0.001，則它在 0.01 (1%)、0.05 (5%) 和 0.1 (10%) 的顯著水準下都顯著不為零。\n")
cat("如果一個係數的 p 值為 0.07，則它在 0.1 (10%) 的水準下顯著，但在 0.05 (5%) 或 0.01 (1%) 的水準下不顯著。\n")
cat("截距項 (Intercept) 的 p 值是 ", sprintf("%.4f", p_values["(Intercept)"]), "\n")
cat("educ 的 p 值是 ", sprintf("%.4f", p_values["educ"]), "\n")
cat("I(educ^2) 的 p 值是 ", sprintf("%.4f", p_values["I(educ^2)"]), "\n")
cat("exper 的 p 值是 ", sprintf("%.4f", p_values["exper"]), "\n")
cat("I(exper^2) 的 p 值是 ", sprintf("%.4f", p_values["I(exper^2)"]), "\n")
cat("educ:exper 的 p 值是 ", sprintf("%.4f", p_values["educ:exper"]), "\n")


#(b)
# 提取估計係數
beta_hat <- coef(model)
b2 <- beta_hat["educ"]
b3 <- beta_hat["I(educ^2)"]
b6 <- beta_hat["educ:exper"]

cat("邊際效應 ∂E[ln(WAGE)|EDUC, EXPER] / ∂EDUC 的表達式為:\n")
cat("ME_EDUC = β₂ + 2 * β₃ * EDUC + β₆ * EXPER\n")

cat("\n使用估計係數:\n")
cat("ME_EDUC_hat = ", sprintf("%.5f", b2), " + 2 * (", sprintf("%.5f", b3), ") * EDUC + (", sprintf("%.5f", b6), ") * EXPER\n")

cat("\n評論:\n")
# 檢查 b3 和 b6 的符號和大小
if (b3 < 0) {
  cat("因為 β₃ 的估計值 (", sprintf("%.5f", b3), ") 為負，所以隨著 EDUC 的增加，EDUC 對 log(WAGE) 的邊際效應會遞減 (假設 EXPER 不變)。\n")
} else if (b3 > 0) {
  cat("因為 β₃ 的估計值 (", sprintf("%.5f", b3), ") 為正，所以隨著 EDUC 的增加，EDUC 對 log(WAGE) 的邊際效應會遞增 (假設 EXPER 不變)。\n")
} else {
  cat("β₃ 的估計值接近零，EDUC 的變化對邊際效應的影響主要是線性的 (透過 β₂ 和 β₆)。\n")
}

if (b6 > 0) {
  cat("因為 β₆ 的估計值 (", sprintf("%.5f", b6), ") 為正，所以隨著 EXPER 的增加，EDUC 對 log(WAGE) 的邊際效應會增加 (假設 EDUC 不變)。教育和經驗在影響工資方面具有互補性。\n")
} else if (b6 < 0) {
  cat("因為 β₆ 的估計值 (", sprintf("%.5f", b6), ") 為負，所以隨著 EXPER 的增加，EDUC 對 log(WAGE) 的邊際效應會減少 (假設 EDUC 不變)。教育和經驗在影響工資方面具有替代性。\n")
} else {
  cat("β₆ 的估計值接近零，EXPER 的變化對 EDUC 的邊際效應影響很小。\n")
}

#(c)
library(dplyr)
library(ggplot2)
library(car)
# 計算每個觀察值的邊際效應 (EDUC)
cps5_small <- cps5_small %>%
  mutate(ME_EDUC = b2 + 2 * b3 * educ + b6 * exper)

# 繪製直方圖
hist_me_educ <- ggplot(cps5_small, aes(x = ME_EDUC)) +
  geom_histogram(binwidth = 0.005, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Marginal Effect of Education on log(WAGE)",
       x = "Marginal Effect (∂ln(WAGE)/∂EDUC)",
       y = "Frequency") +
  theme_minimal()

print(hist_me_educ)

# 計算中位數和百分位數
quantiles_me_educ <- quantile(cps5_small$ME_EDUC, probs = c(0.05, 0.5, 0.95))

cat("\nEDUC 邊際效應的統計數據:\n")
cat("中位數 (Median):", sprintf("%.5f", quantiles_me_educ["50%"]), "\n")
cat("第 5 百分位數 (5th Percentile):", sprintf("%.5f", quantiles_me_educ["5%"]), "\n")
cat("第 95 百分位數 (95th Percentile):", sprintf("%.5f", quantiles_me_educ["95%"]), "\n")

cat("\n發現:\n")
cat("直方圖顯示了教育邊際效應在樣本中的分佈情況。我們可以觀察到效應的集中趨勢、離散程度以及是否所有效應都是正的。大多數情況下，教育的邊際回報是正的，但回報率因個體的教育程度和經驗水平而異。分佈可能呈現偏態，顯示大部分人的邊際回報集中在某個範圍，但也有一些人的回報較高或較低。\n")


#(d)
# 提取估計係數
b4 <- beta_hat["exper"]
b5 <- beta_hat["I(exper^2)"]
# b6 已經在前面提取過了

cat("邊際效應 ∂E[ln(WAGE)|EDUC, EXPER] / ∂EXPER 的表達式為:\n")
cat("ME_EXPER = β₄ + 2 * β₅ * EXPER + β₆ * EDUC\n")

cat("\n使用估計係數:\n")
cat("ME_EXPER_hat = ", sprintf("%.5f", b4), " + 2 * (", sprintf("%.5f", b5), ") * EXPER + (", sprintf("%.5f", b6), ") * EDUC\n")

cat("\n評論:\n")
# 檢查 b5 和 b6 的符號和大小
if (b5 < 0) {
  cat("因為 β₅ 的估計值 (", sprintf("%.5f", b5), ") 為負，所以隨著 EXPER 的增加，EXPER 對 log(WAGE) 的邊際效應會遞減 (假設 EDUC 不變)，這符合經驗報酬遞減的預期。\n")
} else if (b5 > 0) {
  cat("因為 β₅ 的估計值 (", sprintf("%.5f", b5), ") 為正，所以隨著 EXPER 的增加，EXPER 對 log(WAGE) 的邊際效應會遞增 (假設 EDUC 不變)。\n")
} else {
  cat("β₅ 的估計值接近零，EXPER 的變化對其自身邊際效應的影響主要是線性的 (透過 β₄ 和 β₆)。\n")
}

if (b6 > 0) {
  cat("因為 β₆ 的估計值 (", sprintf("%.5f", b6), ") 為正，所以隨著 EDUC 的增加，EXPER 對 log(WAGE) 的邊際效應會增加 (假設 EXPER 不變)。再次說明教育和經驗的互補性。\n")
} else if (b6 < 0) {
  cat("因為 β₆ 的估計值 (", sprintf("%.5f", b6), ") 為負，所以隨著 EDUC 的增加，EXPER 對 log(WAGE) 的邊際效應會減少 (假設 EXPER 不變)。\n")
} else {
  cat("β₆ 的估計值接近零，EDUC 的變化對 EXPER 的邊際效應影響很小。\n")
}

#(e)
# 計算每個觀察值的邊際效應 (EXPER)
cps5_small <- cps5_small %>%
  mutate(ME_EXPER = b4 + 2 * b5 * exper + b6 * educ)

# 繪製直方圖
hist_me_exper <- ggplot(cps5_small, aes(x = ME_EXPER)) +
  geom_histogram(binwidth = 0.002, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Marginal Effect of Experience on log(WAGE)",
       x = "Marginal Effect (∂ln(WAGE)/∂EXPER)",
       y = "Frequency") +
  theme_minimal()

print(hist_me_exper)

# 計算中位數和百分位數
quantiles_me_exper <- quantile(cps5_small$ME_EXPER, probs = c(0.05, 0.5, 0.95))

cat("\nEXPER 邊際效應的統計數據:\n")
cat("中位數 (Median):", sprintf("%.5f", quantiles_me_exper["50%"]), "\n")
cat("第 5 百分位數 (5th Percentile):", sprintf("%.5f", quantiles_me_exper["5%"]), "\n")
cat("第 95 百分位數 (95th Percentile):", sprintf("%.5f", quantiles_me_exper["95%"]), "\n")

cat("\n發現:\n")
cat("直方圖顯示了經驗邊際效應在樣本中的分佈。我們可以看到經驗回報通常是正的，但隨著經驗增加而遞減 (如果 b5 顯著為負)。教育程度較高的人可能經驗回報也更高 (如果 b6 顯著為正)。分佈的形狀、中心和範圍提供了關於經驗回報異質性的資訊。\n")


#(f)
# 定義線性組合的向量 c
# 順序對應 coef(model): (Intercept), educ, I(educ^2), exper, I(exper^2), educ:exper
linear_combination <- c(0, -1, -33, 10, 260, 152)

# 使用 linearHypothesis 進行檢定 (它會給出 F 統計量和雙尾 p 值)
# 我們需要轉換為單尾 t 檢定
lh_test <- linearHypothesis(model, linear_combination, rhs = 0)
print(lh_test)

# 手動計算 t 統計量和單尾 p 值
# 獲取線性組合的估計值
L_hat <- sum(linear_combination * beta_hat)
# 獲取線性組合的變異數 V(L_hat) = c' * Var(beta_hat) * c
vcov_matrix <- vcov(model)
var_L_hat <- t(linear_combination) %*% vcov_matrix %*% linear_combination
se_L_hat <- sqrt(var_L_hat[1, 1]) # 提取標準誤

# 計算 t 統計量 (檢定 L = 0)
t_stat <- (L_hat - 0) / se_L_hat

# 計算自由度
df_resid <- df.residual(model)

# 計算左尾 p 值 (對應 H1: L < 0)
p_value_left_tail <- pt(t_stat, df = df_resid)

cat("\n假設檢定 (H0: L >= 0 vs H1: L < 0):\n")
cat("線性組合 L = -1*β₂ - 33*β₃ + 10*β₄ + 260*β₅ + 152*β₆\n")
cat("估計值 L_hat:", sprintf("%.5f", L_hat), "\n")
cat("標準誤 SE(L_hat):", sprintf("%.5f", se_L_hat), "\n")
cat("t 統計量:", sprintf("%.4f", t_stat), "\n")
cat("自由度:", df_resid, "\n")
cat("左尾 p 值:", sprintf("%.4f", p_value_left_tail), "\n")

# 比較 p 值和顯著水準 alpha
alpha <- 0.05
cat("\n顯著水準 alpha =", alpha, "\n")
if (p_value_left_tail < alpha) {
  cat("因為 p 值 (", sprintf("%.4f", p_value_left_tail), ") 小於顯著水準 ", alpha, "，我們拒絕虛無假設 H₀。\n")
  cat("結論：有足夠證據支持對立假設，即 David 的期望對數工資大於 Svetlana 的期望對數工資。\n")
} else {
  cat("因為 p 值 (", sprintf("%.4f", p_value_left_tail), ") 不小於顯著水準 ", alpha, "，我們無法拒絕虛無假設 H₀。\n")
  cat("結論：沒有足夠證據認為 David 的期望對數工資必然大於 Svetlana 的期望對數工資 (在 5% 水準下)。\n")
}

#(g)
# 繼續使用之前定義的 model 和 beta_hat

# g. 八年後的檢定

# 定義新的線性組合向量 c_g
# 順序: (Intercept), educ, I(educ^2), exper, I(exper^2), educ:exper
linear_combination_g <- c(0, -1, -33, 10, 420, 144)

# 手動計算 t 統計量和單尾 p 值
L_hat_g <- sum(linear_combination_g * beta_hat)
vcov_matrix <- vcov(model) # 已經在 (f) 中定義
var_L_hat_g <- t(linear_combination_g) %*% vcov_matrix %*% linear_combination_g
se_L_hat_g <- sqrt(var_L_hat_g[1, 1])

t_stat_g <- (L_hat_g - 0) / se_L_hat_g
df_resid <- df.residual(model) # 已經在 (f) 中定義
p_value_left_tail_g <- pt(t_stat_g, df = df_resid)

cat("\n--- 問題 (g) ---\n")
cat("八年後假設檢定 (H0: L' >= 0 vs H1: L' < 0):\n")
cat("線性組合 L' = -1*β₂ - 33*β₃ + 10*β₄ + 420*β₅ + 144*β₆\n")
cat("估計值 L'_hat:", sprintf("%.5f", L_hat_g), "\n")
cat("標準誤 SE(L'_hat):", sprintf("%.5f", se_L_hat_g), "\n")
cat("t 統計量:", sprintf("%.4f", t_stat_g), "\n")
cat("自由度:", df_resid, "\n")
cat("左尾 p 值:", sprintf("%.4f", p_value_left_tail_g), "\n")

#(h)
# h. 比較 Wendy 和 Jill 的經驗邊際效應 (使用 t 檢定)

# 回顧: Wendy(E=12, X=17), Jill(E=16, X=11)
# H₀: ME_EXPER(Wendy) - ME_EXPER(Jill) = 0
# H₁: ME_EXPER(Wendy) - ME_EXPER(Jill) ≠ 0
# 線性組合 L_ME = 12*β₅ - 4*β₆

# 定義線性組合向量 c_h (與之前相同)
# 順序: (Intercept), educ, I(educ^2), exper, I(exper^2), educ:exper
linear_combination_h <- c(0, 0, 0, 0, 12, -4)

# 提取估計係數和變異數-共變異數矩陣 (如果還沒提取)
# beta_hat <- coef(model)
# vcov_matrix <- vcov(model)

# 1. 計算線性組合的估計值 L_ME_hat
L_ME_hat <- sum(linear_combination_h * beta_hat)

# 2. 計算 L_ME_hat 的標準誤 SE(L_ME_hat)
var_L_ME_hat <- t(linear_combination_h) %*% vcov_matrix %*% linear_combination_h
se_L_ME_hat <- sqrt(var_L_ME_hat[1, 1])

# 3. 計算 t 統計量 (檢定 H0: L_ME = 0)
t_stat_h <- (L_ME_hat - 0) / se_L_ME_hat

# 4. 獲取自由度
df_resid <- df.residual(model)

# 5. 計算雙尾 p 值
# P(|T| >= |t_stat_h|) = 2 * P(T <= -|t_stat_h|) for T ~ t(df_resid)
p_value_t_h <- 2 * pt(-abs(t_stat_h), df = df_resid)

cat("\n--- 問題 (h) - 使用 t 檢定 ---\n")
cat("比較 Wendy 和 Jill 的經驗邊際效應檢定:\n")
cat("H₀: ME_EXPER(Wendy) - ME_EXPER(Jill) = 0  (即 12*β₅ - 4*β₆ = 0)\n")
cat("H₁: ME_EXPER(Wendy) - ME_EXPER(Jill) ≠ 0  (即 12*β₅ - 4*β₆ ≠ 0)\n")
cat("\n線性組合估計值 L_ME_hat:", sprintf("%.5f", L_ME_hat), "\n")
cat("標準誤 SE(L_ME_hat):", sprintf("%.5f", se_L_ME_hat), "\n")
cat("t 統計量:", sprintf("%.4f", t_stat_h), "\n")
cat("自由度:", df_resid, "\n")
cat("雙尾 p 值:", sprintf("%.4f", p_value_t_h), "\n")

alpha <- 0.05
cat("顯著水準 alpha =", alpha, "\n")
if (p_value_t_h < alpha) {
  cat("因為 p 值 (", sprintf("%.4f", p_value_t_h), ") 小於顯著水準 ", alpha, "，我們拒絕虛無假設 H₀。\n")
  cat("結論：有足夠證據認為 Wendy 和 Jill 的經驗邊際效應不相等 (在 5% 水準下)。\n")
} else {
  cat("因為 p 值 (", sprintf("%.4f", p_value_t_h), ") 不小於顯著水準 ", alpha, "，我們無法拒絕虛無假設 H₀。\n")
  cat("結論：沒有足夠證據認為 Wendy 和 Jill 的經驗邊際效應不相等 (在 5% 水準下)。\n")
}

#(i)
# i. Jill 經驗邊際效應變負所需時間 y 及其信賴區間

# 提取係數
b4 <- beta_hat["exper"]
b5 <- beta_hat["I(exper^2)"]
b6 <- beta_hat["educ:exper"]

# 計算 y 的點估計
y_hat <- -(b4 + 22 * b5 + 16 * b6) / (2 * b5)

cat("\n--- 問題 (i) ---\n")
cat("Jill (EDUC=16, EXPER=11) 的經驗邊際效應變為負值所需的額外年資 (y) 的點估計:\n")
cat("y_hat = ", sprintf("%.4f", y_hat), " 年\n")

# 使用 Delta 方法計算 y_hat 的標準誤和信賴區間
# y = g(b4, b5, b6) = -(b4 + 22*b5 + 16*b6) / (2*b5)
# 梯度向量 (相對於 b1, b2, b3, b4, b5, b6)
# dg/db1=0, dg/db2=0, dg/db3=0
# dg/db4 = -1 / (2*b5)
# dg/db5 = (b4 + 16*b6) / (2 * b5^2)
# dg/db6 = -16 / (2*b5) = -8 / b5

grad_g <- c(
  0,
  0,
  0,
  -1 / (2 * b5),
  (b4 + 16 * b6) / (2 * b5^2),
  -8 / b5
)

# 計算 y_hat 的變異數估計值
vcov_matrix <- vcov(model)
var_y_hat <- t(grad_g) %*% vcov_matrix %*% grad_g
se_y_hat <- sqrt(var_y_hat[1, 1])

cat("y_hat 的標準誤 (使用 Delta 方法):", sprintf("%.4f", se_y_hat), "\n")

# 計算 95% 信賴區間
df_resid <- df.residual(model)
critical_t <- qt(0.975, df = df_resid) # 雙尾 alpha=0.05 -> 單尾 0.025
lower_bound <- y_hat - critical_t * se_y_hat
upper_bound <- y_hat + critical_t * se_y_hat

cat("自由度:", df_resid, "\n")
cat("臨界 t 值 (t_0.025):", sprintf("%.4f", critical_t), "\n")
cat("所需額外年資 (y) 的 95% 信賴區間: [", sprintf("%.4f", lower_bound), ", ", sprintf("%.4f", upper_bound), "]\n")



