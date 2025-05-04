# 載入所需的套件和數據集
library(POE5Rdata)
library(AER)  # 用於 IV 回歸
data("mroz")

# 篩選參與勞動市場的婦女（lf == 1）
mroz_lf <- subset(mroz, lfp == 1)

# 創建 exper 的平方項
mroz_lf$exper_sq <- mroz_lf$exper^2

# IV/2SLS 估計
# 模型：lwage ~ educ + exper + exper^2
# 工具變量：mothereduc 和 fathereduc
iv_model <- ivreg(wage ~ educ + exper + exper_sq | mothereduc + fathereduc + exper + exper_sq, data = mroz_lf)

# 輸出 IV 回歸結果
summary_iv <- summary(iv_model)
cat("IV/2SLS 估計結果：\n")
print(summary_iv)

# 計算殘差
mroz_lf$iv_resid <- residuals(iv_model)

# 繪製殘差對 exper 的散點圖
plot(mroz_lf$exper, mroz_lf$iv_resid, 
     xlab = "Experience (exper)", 
     ylab = "IV/2SLS Residuals (e_y)", 
     main = "Residuals vs Experience")
abline(h = 0, col = "red", lty = 2)  # 添加水平線 y=0

# 簡單判斷同方差性（需人工檢查圖形）
cat("請檢查散點圖：\n")
cat("如果殘差隨 exper 增加展現出明顯的方差變化（例如扇形分佈），則存在異方差性。\n")
cat("如果殘差分佈隨機且方差穩定，則符合同方差性。\n")

# b.--------------------
# 載入所需的套件和數據集
library(POE5Rdata)
library(AER)  # 用於 IV 回歸
library(sandwich)  # 用於計算穩健標準誤
library(lmtest)  # 用於提取穩健標準誤
data("mroz")

# 篩選參與勞動市場的婦女（lf == 1）
mroz_lf <- subset(mroz, lfp == 1)

# 創建 exper 的平方項
mroz_lf$exper_sq <- mroz_lf$exper^2

# IV/2SLS 估計
# 模型：lwage ~ educ + exper + exper^2
# 工具變量：mothereduc 和 fathereduc
iv_model <- ivreg(wage ~ educ + exper + exper_sq | mothereduc + fathereduc + exper + exper_sq, data = mroz_lf)

# 提取默認標準誤（假設同方差性）
summary_iv <- summary(iv_model)
default_se <- coef(summary_iv)[, "Std. Error"]
cat("IV/2SLS 默認標準誤（假設同方差性）：\n")
print(round(default_se, 4))

# 計算穩健標準誤（HC1 類型）
robust_se <- sqrt(diag(vcovHC(iv_model, type = "HC1")))
cat("\nIV/2SLS 穩健標準誤（HC1）：\n")
print(round(robust_se, 4))

# 比較兩種標準誤
cat("\n標準誤比較（默認 vs 穩健）：\n")
comparison <- data.frame(Default_SE = default_se, Robust_SE = robust_se, Difference = default_se - robust_se)
print(round(comparison, 4))

# 判斷同方差性假設是否一致
cat("\n判斷：\n")
if (any(abs(comparison$Difference) / default_se > 0.1)) {  # 如果差異超過 10%
  cat("穩健標準誤與默認標準誤差異較大，IV 估計的標準誤與同方差性假設不一致，存在異方差性。\n")
} else {
  cat("穩健標準誤與默認標準誤相近，IV 估計的標準誤與同方差性假設一致。\n")
}

# c.---------------
library(sandwich)
library(lmtest)

robust_se <- vcovHC(iv_model, type = "HC1")
robust_test <- coeftest(iv_model, vcov = robust_se)

# 教育變數估計值與 robust SE
b_educ <- coef(iv_model)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci <- b_educ + c(-1, 1) * 1.96 * se_educ

# baseline 與 robust 標準誤比較表
baseline_se <- coef(summary(iv_model))[, "Std. Error"]
robust_se_vec <- sqrt(diag(robust_se))
se_comparison <- data.frame(
  Estimate = round(coef(iv_model), 5),
  Baseline_SE = round(baseline_se, 5),
  Robust_SE = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)
cat("Conclusion:\nRobust SEs are larger, indicating heteroskedasticity.\n")
cat(sprintf("95%% Robust CI for EDUC: [%.4f, %.4f]\n", ci[1], ci[2]))

# d.----------------------
library(boot)
iv_formula <- lwage ~ educ + exper + I(exper^2) |
  mothereduc + fathereduc + exper + I(exper^2)
model_iv <- ivreg(iv_formula, data = mroz_sub)
iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(iv_formula, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz_sub, statistic = iv_boot, R = 200)

boot_se <- apply(boot_iv$t, 2, sd)
cat("\n(d) Bootstrap SEs (B=200):\n")
print(boot_se)

# Compare bootstrap SE on EDUC to baseline & robust:
cat(sprintf("  EDUC: baseline SE=%.4f, robust SE=%.4f, bootstrap SE=%.4f\n",
            baseline_se, robust_se, boot_se[2]))

# 95% CI for EDUC using bootstrap SE:
ci_boot <- coef(model_iv)["educ"] + c(-1,1)*qnorm(0.975)*boot_se[2]
cat("  95% CI for EDUC (bootstrap) = [",
    round(ci_boot[1],4), ", ", round(ci_boot[2],4), "]\n")
