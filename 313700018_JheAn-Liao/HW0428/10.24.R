rm(list=ls())
# 🔗 下載並載入 vacation 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(mroz)

mroz_work <- subset(mroz, lfp == 1, wage > 0)
mroz_work$lnwage <- log(mroz_work$wage)
mroz_work$exper2 <- mroz_work$exper^2

# 2. IV/2SLS 估計（EDUC 以 MOTHEREDUC+FATHEREDUC 作工具）
iv_fit <- ivreg(
  lnwage ~ exper + exper2 + educ |
    exper + exper2 + mothereduc + fathereduc,
  data = mroz_work
)

# 3. 取出 2SLS 殘差 ê_y
mroz_work$e_iv <- resid(iv_fit)

# 4. 畫圖：ê_y 對 EXPER
plot(mroz_work$exper, mroz_work$e_iv,
     xlab = "EXPER (years)",
     ylab = "2SLS residuals ê_y",
     main = "Residuals vs. Experience")
abline(h = 0, lty = 2)

#明顯有異質變異數的問題

#B
# 1. 計算殘差平方
mroz_work$e2_iv <- mroz_work$e_iv^2

# 2. Aux 回歸：e_iv^2 ~ 1 + exper
aux_bp <- lm(e2_iv ~ exper, data = mroz_work)

# 3. 讀出 R^2 與樣本數 n
R2_aux <- summary(aux_bp)$r.squared
n      <- nrow(mroz_work)
R2_aux
# 4. 計算 LM 統計量：n * R^2
LM_stat <- n * R2_aux

# 5. p 值 (卡方分布，df = #explanatory = 1)
p_value <- 1 - pchisq(LM_stat, df = 1)

# 6. 輸出結果
cat(sprintf("Breusch–Pagan (LM) 統計量 = %.3f\n", LM_stat))
cat(sprintf("自由度 = 1, p-value = %.3f\n", p_value))
if (p_value < 0.05) {
  cat("在 5% 水準下拒絕同方差假設 → 存在異質變異\n")
} else {
  cat("在 5% 水準下無法拒絕同方差假設 → 無明顯異質變異\n")
}

#C
mroz_work <- subset(mroz, lfp == 1 & wage > 0)
mroz_work$lnwage <- log(mroz_work$wage)
mroz_work$exper2 <- mroz_work$exper^2

# 1. 基準 2SLS（同方差 SE）
iv_base <- ivreg(
  lnwage ~ exper + exper2 + educ |
    exper + exper2 + mothereduc + fathereduc,
  data = mroz_work
)
sum_base <- summary(iv_base)

# 2. Robust SE 估計
#    type="HC0" 為最基礎的 White SE，也可以指定 "HC1","HC2","HC3"
vcov_rob <- vcovHC(iv_base, type = "HC0")
coeft_rob <- coeftest(iv_base, vcov = vcov_rob)

# 3. 比較 EDUC 的同方差 vs. Robust SE
beta_e    <- coef(iv_base)["educ"]
se_homo   <- sum_base$coefficients["educ","Std. Error"]
se_robust <- sqrt(diag(vcov_rob))["educ"]

cat(sprintf("同方差 SE = %.4f\n", se_homo))
cat(sprintf("Robust SE    = %.4f\n", se_robust), "\n\n")

# 4. 用 Robust SE 計算 95% CI
alpha    <- 0.05
z_crit   <- qnorm(1 - alpha/2)
ci_lower <- beta_e - z_crit * se_robust
ci_upper <- beta_e + z_crit * se_robust

cat(sprintf("EDUC 係數點估計 = %.4f\n", beta_e))
cat(sprintf("95%% CI (robust) = [%.4f, %.4f]\n", ci_lower, ci_upper))


#D
data("mroz")
mroz_work <- subset(mroz, lfp == 1 & wage > 0)
mroz_work$lnwage <- log(mroz_work$wage)
mroz_work$exper2 <- mroz_work$exper^2

# 1. 基準 2SLS 估計
iv_base <- ivreg(
  lnwage ~ exper + exper2 + educ |
    exper + exper2 + mothereduc + fathereduc,
  data = mroz_work
)
sum_base    <- summary(iv_base)
beta_e      <- coef(iv_base)["educ"]
se_homo     <- sum_base$coefficients["educ","Std. Error"]
# 異質變異 Robust SE (HC0)
library(sandwich); library(lmtest)
vcov_rob    <- vcovHC(iv_base, type="HC0")
se_robust   <- sqrt(diag(vcov_rob))["educ"]

# 2. 定義 Bootstrap 統計量：回傳 EDUC 係數
boot_iv_fn <- function(data, indices) {
  d <- data[indices, ]
  fit <- ivreg(
    lnwage ~ exper + exper2 + educ |
      exper + exper2 + mothereduc + fathereduc,
    data = d
  )
  coef(fit)["educ"]
}

# 3. 執行 Bootstrap (B = 200)
set.seed(2025)  # 確保可重現
boot_iv <- boot(
  data = mroz_work,
  statistic = boot_iv_fn,
  R = 200
)

# 4. 取出 Bootstrap SE
se_boot <- sd(boot_iv$t)

# 5. 比較三種 SE
cat(sprintf("同方差 SE     = %.4f\n", se_homo))
cat(sprintf("Robust  SE     = %.4f\n", se_robust))
cat(sprintf("Bootstrap SE   = %.4f\n\n", se_boot))

# 6. 基於 Bootstrap SE 的 95% CI (normal approximation)
z_crit   <- qnorm(0.975)
ci_boot_lower <- beta_e - z_crit * se_boot
ci_boot_upper <- beta_e + z_crit * se_boot
cat(sprintf("EDUC 點估計 β̂ = %.4f\n", beta_e))
cat(sprintf("95%% CI (boot) = [%.4f, %.4f]\n",
            ci_boot_lower, ci_boot_upper))
