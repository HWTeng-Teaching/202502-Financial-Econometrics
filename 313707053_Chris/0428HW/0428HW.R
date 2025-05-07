library(POE5Rdata)
data("mroz",package="PoEdata")
install.packages("AER")
library(AER)
library(car)
summary(mroz)

#10.18(a)
mroz_sub <- subset(mroz, lfp == 1)
mroz_sub$MOTHERCOLL <- ifelse(mroz_sub$mothereduc > 12, 1, 0)
mroz_sub$FATHERCOLL <- ifelse(mroz_sub$fathereduc > 12, 1, 0)
mother_coll_pct <- mean(mroz_sub$MOTHERCOLL, na.rm = TRUE)*100 
father_coll_pct <- mean(mroz_sub$FATHERCOLL, na.rm = TRUE)*100
cat("母親有部分大學教育的比例：", round(mother_coll_pct, 4), "%\n")
cat("父親有部分大學教育的比例：", round(father_coll_pct, 4), "%\n")

#(b)
cor(mroz_sub[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

#(c)
mroz_sub$lwage <- log(mroz_sub$wage)
mroz_sub$exper2 <- mroz_sub$exper^2
iv_model <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + MOTHERCOLL, data = mroz_sub)
summary(iv_model,diagnostics = TRUE)
cat('95% Confidence Interval: [',confint(iv_model, level = 0.95)["educ", ],']')

#(d)
first <- lm(educ ~ exper + exper2 + MOTHERCOLL, data = mroz_sub)
summary(first)
Anova(first, type = "II")

#(e)
iv_model2<- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + MOTHERCOLL + FATHERCOLL,data = mroz_sub)
summary(iv_model2, diagnostics = TRUE)
confint(iv_model2, level = 0.95)["educ", ]

#(f)
first_stage2 <- lm(educ ~ exper + exper2 + MOTHERCOLL + FATHERCOLL, data = mroz_sub)
summary(first_stage2)
Anova(first_stage2, type = "II")
linearHypothesis(first_stage2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#(g)
resid_iv <- resid(iv_model2)
sargan_test <- lm(resid_iv ~ MOTHERCOLL + FATHERCOLL, data = mroz_sub)
S <- nrow(mroz_sub) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))

#10.20(a)
summary(capm5)
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)

#(b)
capm5$RANK <- rank(capm5$mkt_excess)
firstStage <- lm(mkt_excess ~ RANK, data = capm5)
summary(firstStage)
Anova(firstStage, type = "II")

#(c)
capm5$v_hat <- resid(firstStage)
capm_with_residuals <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(capm_with_residuals)

#(d)
iv <- ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)
summary(iv)

#(e)
capm5$pos <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage_pos <- lm(mkt_excess ~ RANK + pos, data = capm5)
summary(first_stage_pos)
anova(first_stage_pos)
linearHypothesis(first_stage_pos, c("RANK = 0", "pos = 0"))

#(f)
capm5$v_hat2 <- resid(firststage_pos)
capm_residuals2 <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(capm_residuals2)

#(g)
library(AER)
iv_model2 <- ivreg(msft_excess ~ mkt_excess | RANK + pos, data = capm5)
summary(iv_model2)

#(h)
capm5$resid_iv2 <- resid(iv_model2)
sargan_reg <- lm(resid_iv2 ~ RANK+ pos, data = capm5)
summary(sargan_reg)
(p_value <- 1 - pchisq(S, df = 1))
cat('\np-value=',p_value,'\nConclusion:\nWe do not reject the null hypothes. Both RANK and POS are considered exogenous.' )


#10.24
#(a)
iv_model <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + mothereduc + fathereduc,data = mroz_sub)
mroz_sub$res <- resid(iv_model)
summary(iv_model)
plot(mroz_sub$exper, mroz_sub$res,col = 'red',pch = 16, xlab = "EXPER", ylab = "Residuals", main = "2SLS Residuals versus EXPER")


#(b)
# 假設你已經有 mroz_sub 和主模型的殘差 res：
mroz_sub$res2 <- mroz_sub$res^2

# 輔助回歸：檢查異質變異（以 exper 為變數）
aux_model <- lm(res2 ~ exper, data = mroz_sub)

# 抓 R-squared
R2 <- summary(aux_model)$r.squared

# 檢定統計量
n <- nrow(mroz_sub)  # ✅ 這裡改對
nR2 <- n * R2

# 計算 p-value：自由度等於輔助回歸中自變數個數（這裡只有 exper，一個）
p_value <- 1 - pchisq(nR2, df = 1)

# 輸出結果
nR2
p_value
if (p_value < 0.05) {
  cat("結論：拒絕 H0，存在異質變異性。\n")
} else {
  cat("結論：無法拒絕 H0，沒有異質變異性證據。\n")
}

#(c)
library(sandwich)
library(lmtest)

robust_se <- vcovHC(iv_model, type = "HC1")
robust_test <- coeftest(iv_model, vcov = robust_se)

b_educ <- coef(iv_model)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci <- b_educ + c(-1, 1) * 1.96 * se_educ

baseline_se <- coef(summary(iv_model))[, "Std. Error"]
robust_se_vec <- sqrt(diag(robust_se))
se_comparison <- data.frame(
  Estimate = round(coef(iv_model), 5),
  Baseline_SE = round(baseline_se, 5),
  Robust_SE = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No"))
print(se_comparison)
cat("Conclusion:\nRobust SEs are larger, indicating heteroscedasticity.\n")
cat(sprintf("95%% Robust CI for EDUC: [%.5f, %.5f]\n", ci[1], ci[2]))

#(d)

library(boot)
library(AER)
boot_iv <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(lwage ~ educ + exper + exper2 |
                   mothereduc + fathereduc + exper + exper2,
                 data = d)
  return(coef(model)["educ"])
}
set.seed(123)  
boot_result <- boot(data = mroz_use, statistic = boot_iv, R = 200)
bootstrap_se <- sd(boot_result$t)
cat("Bootstrap SE for EDUC =", bootstrap_se, "\n")

baseline_se <- summary(iv_model)$coefficients["educ", "Std. Error"]
robust_se <- sqrt(diag(vcovHC(iv_model, type = "HC1")))["educ"]


comparison <- data.frame(
  Type = c("Baseline", "Robust", "Bootstrap"),
  SE = c(baseline_se, robust_se, bootstrap_se)
)
print(comparison)

educ_coef <- coef(iv_model)["educ"]

lower <- educ_coef - 1.96 * bootstrap_se
upper <- educ_coef + 1.96 * bootstrap_se
cat("95% CI for EDUC (Bootstrap SE): [", 
    round(lower, 4), ", ", round(upper, 4), "]\n")
