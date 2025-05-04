library(POE5Rdata)
data("mroz")

#a.------------------------------------------------------
library(AER)   # 含 ivreg
library(ggplot2)
# 篩選參與勞動市場的女性（參考文獻中用 lfp == 1）
mroz_working <- subset(mroz, lfp == 1)
# 建立 exper^2 變數
mroz_working$exper2 <- mroz_working$exper^2

# 執行 IV/2SLS，educ 是內生變數，工具是 mothereduc 與 fathereduc
iv_model <- ivreg(log(wage) ~ educ + exper + exper2 | mothereduc + fathereduc + exper + exper2,
                  data = mroz_working)
summary(iv_model)
# 取出 IV 殘差
mroz_working$e_iv <- resid(iv_model)
# 畫圖：殘差對 exper
ggplot(mroz_working, aes(x = exper, y = e_iv)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "IV/2SLS 殘差對 Exper 散佈圖",
       x = "工作經驗 (EXPER)",
       y = "IV 殘差 (ê_IV)") +
  theme_minimal()

#b.------------------------------------------------------
# 殘差平方
mroz_working$e_iv2 <- mroz_working$e_iv^2
# 回歸 ê^2_IV 對常數與 exper
bp_test <- lm(e_iv2 ~ exper, data = mroz_working)
# 取得 R^2 與樣本數
R2 <- summary(bp_test)$r.squared
n <- nobs(bp_test)
NR2 <- n * R2
# 計算 p-value（自由度 = 1）
p_value <- pchisq(NR2, df = 1, lower.tail = FALSE)

# 輸出結果
cat("NR^2 統計量 =", NR2, "\n")
cat("p-value =", p_value, "\n")

#c.------------------------------------------------------
# 使用穩健標準誤進行 IV/2SLS 回歸
iv_base_model <- ivreg(log(wage) ~ educ + exper + exper2 | mothereduc + fathereduc + exper + exper2,
                       data = mroz_working)
                       
iv_robust_model <- ivreg(log(wage) ~ educ + exper + exper2 | mothereduc + fathereduc + exper + exper2,
                         data = mroz_working, 
                         vcov = function(x) vcovHC(x, type = "HC3"))

# 顯示穩健標準誤結果
summary(iv_base_model)
summary(iv_robust_model)

base_se_educ <- sqrt(vcov(iv_base_model)[2, 2])  # 取得 educ 係數的標準誤

# 穩健模型中的 educ 係數的標準誤差
robust_se_educ <- sqrt(vcovHC(iv_robust_model, type = "HC3")[2, 2])

# 顯示結果
cat("基準模型中的 EDUC 係數標準誤：", base_se_educ, "\n")
cat("穩健模型中的 EDUC 係數標準誤：", robust_se_educ, "\n")


# Step 2: 計算 95% 區間估計，這是使用穩健標準誤的結果
coefficient_educ <- coef(iv_robust_model)["educ"]
robust_se_educ <- sqrt(vcovHC(iv_robust_model, type = "HC3")[2, 2])  # 取得 educ 係數的穩健標準誤

# 95% 信賴區間
lower_bound <- coefficient_educ - 1.96 * robust_se_educ
upper_bound <- coefficient_educ + 1.96 * robust_se_educ

cat("EDUC 係數的 95% 區間估計：", lower_bound, "到", upper_bound, "\n")

#d.------------------------------------------------------
library(boot)
# 步驟 1: 定義函數來計算 IV/2SLS 模型的係數
iv_2sls_bootstrap <- function(data, indices) {
  d <- data[indices, ]  # 從 bootstrap 抽樣中獲得數據
  model <- ivreg(log(wage) ~ educ + exper + exper2 | mothereduc + fathereduc + exper + exper2, data = d)
  return(coef(model))  # 返回模型係數
}

# 步驟 2: 使用 200 次 bootstrap 重複抽樣計算 IV/2SLS 係數
set.seed(123)  # 設置隨機種子以確保結果可重現
bootstrap_results <- boot(data = mroz_working, statistic = iv_2sls_bootstrap, R = 200)

# 顯示 bootstrap 結果
print(bootstrap_results$t)

# 步驟 3: 計算 bootstrap 標準誤差
# 提取 educ 係數的標準誤差
bootstrap_se_educ <- sd(bootstrap_results$t[, 1])

# 顯示 bootstrap 標準誤差
cat("Bootstrap 標準誤差：", bootstrap_se_educ, "\n")

# 步驟 4: 計算 95% 信賴區間
coefficient_educ_bootstrap <- mean(bootstrap_results$t[, 1])
lower_bound_bootstrap <- coefficient_educ_bootstrap - 1.96 * bootstrap_se_educ
upper_bound_bootstrap <- coefficient_educ_bootstrap + 1.96 * bootstrap_se_educ

cat("EDUC 係數的 95% 信賴區間（Bootstrap）：", lower_bound_bootstrap, "到", upper_bound_bootstrap, "\n")

# 步驟 5: 比較標準誤差
# 基準模型中的 educ 係數的標準誤差
base_se_educ <- sqrt(vcov(iv_base_model)[2, 2])  # 取得 educ 係數的標準誤

# 穩健模型中的 educ 係數的標準誤差
robust_se_educ <- sqrt(vcovHC(iv_robust_model, type = "HC3")[2, 2])

# 比較標準誤差
cat("基準模型中的 EDU 係數標準誤差：", base_se_educ, "\n")
cat("穩健模型中的 EDU 係數標準誤差：", robust_se_educ, "\n")
cat("Bootstrap 模型中的 EDU 係數標準誤差：", bootstrap_se_educ, "\n")

# 比較 Bootstrap 標準誤差與基準模型及穩健模型的差異
if (bootstrap_se_educ > base_se_educ) {
  cat("Bootstrap 標準誤差較大\n")
} else {
  cat("Bootstrap 標準誤差較小\n")
}

if (bootstrap_se_educ > robust_se_educ) {
  cat("Bootstrap 標準誤差較大\n")
} else {
  cat("Bootstrap 標準誤差較小\n")
}
