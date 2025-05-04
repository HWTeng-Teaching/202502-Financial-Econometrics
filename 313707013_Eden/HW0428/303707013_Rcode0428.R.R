library(POE5Rdata)
data("mroz",package="PoEdata")
install.packages("AER")
library(AER)
library(car)
#10.18.a
mroz_sub <- subset(mroz,lfp==1)
mroz_sub$mothercoll <- ifelse(mroz_sub$mothereduc > 12, 1, 0)
mroz_sub$fathercoll <- ifelse(mroz_sub$fathereduc > 12, 1, 0)
mean(mroz_sub$mothercoll)  
mean(mroz_sub$fathercoll)
#10.18.b
cor(mroz_sub[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")
cor(mroz_sub[, c("educ", "mothereduc", "fathereduc")], use = "complete.obs")
#10.18.c
mroz_sub$lnwage <- log(mroz_sub$wage)
mroz_sub$exper2 <- mroz_sub$exper^2
iv_model <- ivreg(lnwage ~ exper + exper2 + educ | exper + exper2 + mothercoll, data = mroz_sub)
summary(iv_model,diagnostics = TRUE)
confint(iv_model, level = 0.95)["educ", ]
cat('95% CI: [',confint(iv_model, level = 0.95)["educ", ],']')
#10.18.d
first_stage <- lm(educ ~ exper + exper2 + mothercoll, data = mroz_sub)
summary(first_stage)
linearHypothesis(first_stage, c("mothercoll=0"))
#10.18.e
iv_model2<- ivreg(lnwage ~ exper + exper2 + educ |
                          exper + exper2 + mothercoll + fathercoll,
                        data = mroz_sub)
summary(iv_model2, diagnostics = TRUE)
confint(iv_model2, level = 0.95)["educ", ]
#10.18.f
first_stage2 <- lm(educ ~ exper + exper2 + mothercoll + fathercoll, data = mroz_sub)
summary(first_stage2)
linearHypothesis(first_stage2, c("mothercoll = 0", "fathercoll = 0"))
#10.18.g
resid_iv <- resid(iv_model2)
sargan_test <- lm(resid_iv ~ mothercoll + fathercoll, data = mroz_sub)
summary(sargan_test)
S <- nrow(mroz_sub) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))

#10.20.a
summary(capm5)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)
#10.20.b
capm5$rank <- rank(capm5$mkt_excess)
first_stage <- lm(mkt_excess ~ rank, data = capm5)
summary(first_stage)
linearHypothesis(first_stage, c("rank=0"))
#10.20.c
capm5$v_hat <- resid(first_stage)
capm_with_v_hat <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(capm_with_v_hat)
#10.20.d
ivreg_model <- ivreg(msft_excess ~ mkt_excess | rank, data = capm5)
summary(ivreg_model)
#10.20.e
capm5$pos <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage_2 <- lm(mkt_excess ~ rank + pos, data = capm5)
summary(first_stage_2)
linearHypothesis(first_stage_2 ,c("rank = 0", "pos = 0"))
#10.20.f
capm5$v_hat2 <- resid(first_stage_2)
capm_with_residuals2 <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(capm_with_residuals2)
#10.20.g
ivreg_model2 <- ivreg(msft_excess ~ mkt_excess | rank + pos, data = capm5)
summary(ivreg_model2)
#10.20.h
capm5$resid_iv2 <- resid(ivreg_model2)
sargan_reg <- lm(resid_iv2 ~ rank+ pos, data = capm5)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
n <- nrow(capm5)
S <- n * R2
S
(p_value <- 1 - pchisq(S, df = 1))

#10.24.a
data("mroz", package = "PoEdata")
# 只保留 wage > 0
mroz2 <- subset(mroz, wage > 0)
# 建立 exper^2
mroz2$exper2 <- mroz2$exper^2
# 估計 IV/2SLS
iv_model <- ivreg(log(wage) ~ exper + exper2 + educ | exper + exper2 + mothereduc + fathereduc, data = mroz2)
iv_resid <- resid(iv_model)
plot(mroz2$exper, iv_resid, xlab = "exper", ylab = "IV/2SLS 殘差", main = "IV/2SLS 殘差與 exper 的散佈圖")
abline(h = 0, col = "red", lty = 2)
summary(iv_model)
#10.24.b
# 1. 取得殘差平方
iv_resid2 <- iv_resid^2
# 2. 輔助迴歸：殘差平方對常數和 exper
aux_model <- lm(iv_resid2 ~ exper, data = mroz2)
# 3. 取得 R^2
R2 <- summary(aux_model)$r.squared
# 4. 計算 nR^2
n <- nrow(mroz2)
nR2 <- n * R2
# 5. 檢定（自由度為自變項數，這裡是 1）
p_value <- 1 - pchisq(nR2, df = 1)
# 6. 輸出結果
cat("nR^2 =", nR2, "\np-value =", p_value, "\n")
if (p_value < 0.05) {
  cat("拒絕同方差性假設，存在異方差性。\n")
} else {
  cat("無法拒絕同方差性假設。\n")
}

#10.24.c
library(AER)
library(sandwich)
library(lmtest)
# 先用 ivreg 跑模型
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothereduc + fathereduc, data = mroz2)
robust_se <- vcovHC(iv_model, type = "HC1")
robust_test <- coeftest(iv_model, vcov = robust_se)
# 教育變數估計值與 robust SE
b_educ <- coef(iv_model)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci <- b_educ + c(-1, 1) * 1.96 * se_educ
ci
baseline_se <- coef(summary(iv_model))[, "Std. Error"]
robust_se_vec <- sqrt(diag(robust_se))
se_comparison <- data.frame(
  Estimate = round(coef(iv_model), 5),
  Baseline_SE = round(baseline_se, 5),
  Robust_SE = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)

#10.24.d
library(boot)
iv_formula <- log(wage) ~ educ + exper + I(exper^2) |
  mothereduc + fathereduc + exper + I(exper^2)
model_iv <- ivreg(iv_formula, data = mroz2)
iv_boot <- function(data, i) {
  d <- data[i, ]
  fit <- ivreg(iv_formula, data = d)
  coef(fit)
}

set.seed(123)
boot_iv <- boot(data = mroz2, statistic = iv_boot, R = 200)

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