#a
# 載入必要套件
library(AER)      # 提供 ivreg()
library(ggplot2)  # 作圖
library(dplyr)

# 載入資料
mroz <- read.csv("C:/Users/USER/Desktop/碩一下資料/計量經濟學/POE/poe5rdata/mroz.csv")

# 篩選出參與勞動力市場的人 (lfp == 1)，並排除缺失值或 wage 為 0
mroz_use <- mroz %>%
  filter(lfp == 1, wage > 0) %>%
  mutate(lwage = log(wage),
         expersq = exper^2)

# 估計 IV/2SLS：educ 是內生變數，由 mothereduc 和 fathereduc 作為工具變數
iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = mroz_use)

# 取出殘差
mroz_use$e_iv <- residuals(iv_model)

# 繪製殘差對 exper 的散佈圖
ggplot(mroz_use, aes(x = exper, y = e_iv)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "IV Residuals vs. Experience",
       x = "EXPER",
       y = "IV Residuals") +
  theme_minimal()






#b
# 計算殘差平方
mroz_use$e2_iv <- mroz_use$e_iv^2

# 輔助回歸：e2_iv ~ exper
aux_model <- lm(e2_iv ~ exper, data = mroz_use)

# 計算 nR^2
n <- nobs(aux_model)
R2 <- summary(aux_model)$r.squared
nR2 <- n * R2

# 檢定用的卡方臨界值與 p 值
df <- 1  # 只有一個解釋變數：exper
p_value <- 1 - pchisq(nR2, df)

# 輸出結果
cat("nR² statistic =", nR2, "\n")
cat("Degrees of freedom =", df, "\n")
cat("p-value =", p_value, "\n")

# 判斷
if (p_value < 0.05) {
  cat("結論：拒絕 H0，存在異質變異性。\n")
} else {
  cat("結論：無法拒絕 H0，沒有異質變異性證據。\n")
}

#c
# 重新估計 baseline IV 模型（如果還沒估過）
library(AER)
iv_model <- ivreg(lwage ~ educ + exper + expersq | mothereduc + fathereduc + exper + expersq, data = mroz_use)

# 安裝並載入 sandwich 套件（提供 robust SE）
library(sandwich)

# 計算 heteroskedasticity-robust 標準誤（Eicker-Huber-White）
robust_se <- sqrt(diag(vcovHC(iv_model, type = "HC1")))

# 比較 baseline SE 與 robust SE
baseline_se <- summary(iv_model)$coefficients[, "Std. Error"]
comparison <- data.frame(
  Coefficient = names(baseline_se),
  Baseline_SE = baseline_se,
  Robust_SE = robust_se,
  Ratio = robust_se / baseline_se
)
print(comparison)

# 抓出 EDUC 係數與其 robust SE
educ_coef <- coef(iv_model)["educ"]
educ_robust_se <- robust_se["educ"]

# 計算 95% 信賴區間
lower <- educ_coef - 1.96 * educ_robust_se
upper <- educ_coef + 1.96 * educ_robust_se

cat("95% 信賴區間 for EDUC =", "[", lower, ",", upper, "]\n")


#d
# 如尚未安裝請先執行：
# install.packages("boot")
# install.packages("AER")

library(boot)
library(AER)
# 定義 bootstrap 回傳 EDUC 係數的函數
boot_iv <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(lwage ~ educ + exper + expersq |
                   mothereduc + fathereduc + exper + expersq,
                 data = d)
  return(coef(model)["educ"])
}
set.seed(123)  # 為了結果可重現
boot_result <- boot(data = mroz_use, statistic = boot_iv, R = 200)
# 抓出 bootstrap 標準誤
bootstrap_se <- sd(boot_result$t)
cat("Bootstrap SE for EDUC =", bootstrap_se, "\n")

# 抓出 baseline 與 robust SE（已經估過）
baseline_se <- summary(iv_model)$coefficients["educ", "Std. Error"]
robust_se <- sqrt(diag(vcovHC(iv_model, type = "HC1")))["educ"]

# 比較
comparison <- data.frame(
  Type = c("Baseline", "Robust", "Bootstrap"),
  SE = c(baseline_se, robust_se, bootstrap_se)
)
print(comparison)
# EDUC 係數點估計
educ_coef <- coef(iv_model)["educ"]

# 計算信賴區間（常用：點估 ± 1.96 × bootstrap SE）
lower <- educ_coef - 1.96 * bootstrap_se
upper <- educ_coef + 1.96 * bootstrap_se
cat("95% CI for EDUC (Bootstrap SE): [", lower, ",", upper, "]\n")
