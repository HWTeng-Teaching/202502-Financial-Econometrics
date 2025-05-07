#10.18
#(a)
library(POE5Rdata)
data(mroz)
# 篩選出參與勞動市場的 married women
mroz2 <- subset(mroz, lfp == 1)

# 建立虛擬變數：父母是否受過大學教育（>12年）
mroz2$mothercoll <- ifelse(mroz2$mothereduc > 12, 1, 0)
mroz2$fathercoll <- ifelse(mroz2$fathereduc > 12, 1, 0)

# 計算比例
mother_pct <- mean(mroz2$mothercoll) * 100
father_pct <- mean(mroz2$fathercoll) * 100

cat(round(mother_pct, 2), "%\n")
cat(round(father_pct, 2), "%\n")

#(b)
cor_data <- mroz2[, c("educ", "mothercoll", "fathercoll")]
cor_matrix <- cor(cor_data)
print(round(cor_matrix, 3))

#(c)
library(AER)

# 確保 wage 是數值且 > 0 才能取對數
mroz2 <- subset(mroz2, wage > 0)
mroz2$lnwage <- log(mroz2$wage)
mroz2$exper2 <- mroz2$exper^2

# 執行 IV/2SLS 模型：educ 為內生變數，mothercoll 為工具
iv_model <- ivreg(lnwage ~ exper + exper2 + educ |
                    exper + exper2 + mothercoll,
                  data = mroz2)

# 顯示模型與診斷檢定
summary(iv_model, diagnostics = TRUE)

# 計算 EDUC 係數的 95% 信賴區間
ci <- confint(iv_model, level = 0.95)["educ", ]
cat("95% CI for EDUC coefficient: [", round(ci[1], 4), ",", round(ci[2], 4), "]\n")

#(d)
# 第一階段回歸：以 EDUC 為因變數，MOTHERCOLL 為解釋變數（還控制 exper, exper2）
first_stage <- lm(educ ~ exper + exper2 + mothercoll, data = mroz2)

# 顯示回歸摘要（包含母親教育虛擬變數係數和整體 F 統計）
summary(first_stage)

# 抓取 F 統計量（整體回歸 F，但我們關注 mothercoll 單一變數的檢定）
anova(first_stage)

# 使用 car 套件對 mothercoll 做單獨檢定
library(car)
linearHypothesis(first_stage, "mothercoll = 0")

#(e)
library(AER)

# 確保 wage 合法且已建立衍生變數
mroz2 <- subset(mroz2, wage > 0)
mroz2$lnwage <- log(mroz2$wage)
mroz2$exper2 <- mroz2$exper^2
mroz2$mothercoll <- ifelse(mroz2$mothereduc > 12, 1, 0)
mroz2$fathercoll <- ifelse(mroz2$fathereduc > 12, 1, 0)

#(f)
# 建立第一階段回歸模型（educ 的方程式）
first_stage2 <- lm(educ ~ exper + exper2 + mothercoll + fathercoll, data = mroz2)

# 顯示回歸摘要（會看到每個變數的係數和 t 檢定）
summary(first_stage2)

# 載入 car 套件做「聯合檢定」：檢定 mothercoll = 0 與 fathercoll = 0 同時成立
library(car)
linearHypothesis(first_stage2, c("mothercoll = 0", "fathercoll = 0"))

#(g)
# 確保有 residual 和兩個工具變數（請用 iv_model2 來自 10.18.e）
resid_iv <- resid(iv_model2)

sargan_test <- lm(resid_iv ~ mothercoll + fathercoll, data = mroz2)

# 顯示 Sargan 檢定的回歸結果（只為看 R²）
summary(sargan_test)

# 計算 Sargan 統計量：n × R²
S <- nrow(mroz2) * summary(sargan_test)$r.squared

# 計算 p-value（自由度 = 超額工具個數 = 2 - 1 = 1）
(p_value <- 1 - pchisq(S, df = 1))

##10.20

library(POE5Rdata)
data("capm5")

#(a)
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)

#(b)
capm5$RANK <- rank(capm5$mkt_excess)
first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)
linearHypothesis(first_stage, c("RANK=0"))


#(c)
capm5$v_hat <- resid(first_stage)
second_stage <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(second_stage)

#(D)
library(AER)  
iv_model <- ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)
summary(iv_model)

#(e)
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage2)
library(car)
linearHypothesis(first_stage2, c("RANK = 0", "POS = 0"))

#(f)
first_stage2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
capm5$v_hat2 <- resid(first_stage2)
hausman_test <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(hausman_test)

#(g)
iv_model2 <- ivreg(msft_excess ~ mkt_excess | RANK + POS, data = capm5)
summary(iv_model2)

#(h)
iv_model2 <- ivreg(msft_excess ~ mkt_excess | RANK + POS, data = capm5)
capm5$iv_resid <- resid(iv_model2)

sargan_reg <- lm(iv_resid ~ RANK + POS, data = capm5)
summary(sargan_reg)

R2 <- summary(sargan_reg)$r.squared
n <- nrow(capm5)
S <- n * R2
S
(p_value <- 1 - pchisq(S, df = 1))

##10.24
library(POE5Rdata)
data(mroz)

#(a)
mroz2 <- subset(mroz, wage > 0)
mroz2$exper2 <- mroz2$exper^2
iv_model <- ivreg(log(wage) ~ exper + exper2 + educ | exper + exper2 + mothereduc + fathereduc, data = mroz2)
iv_resid <- resid(iv_model)
plot(mroz2$exper, iv_resid, xlab = "exper", ylab = "residuals"
     , main = "IV/2SLS residuals and exper",abline(h = 0, col = "blue",y= 2))
summary(iv_model)

#(b)
mroz2$e2 <- iv_resid^2
bp_model <- lm(e2 ~ exper, data = mroz2)
nR2 <- summary(bp_model)$r.squared * nrow(mroz2)

pval <- 1 - pchisq(nR2, df = 1)
cat("nR² =", nR2, "p-value =", pval, "\n")

#(c)
# 套件載入
library(AER)
library(sandwich)
library(lmtest)

# IV/2SLS 模型
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothereduc + fathereduc, data = mroz2)

# Robust (HC1) 標準誤
robust_se <- vcovHC(iv_model, type = "HC1")

# 使用 robust SE 的 t 檢定結果
robust_test <- coeftest(iv_model, vcov = robust_se)

# 抓取 educ 的估計值與 robust SE
b_educ <- coef(iv_model)["educ"]
se_educ_robust <- sqrt(robust_se["educ", "educ"])

# 計算 95% 信賴區間
ci_robust <- b_educ + c(-1, 1) * 1.96 * se_educ_robust
names(ci_robust) <- c("Lower", "Upper")
print(ci_robust)

# baseline 標準誤（假設同質變異）
baseline_se <- coef(summary(iv_model))[, "Std. Error"]

# robust 標準誤向量
robust_se_vec <- sqrt(diag(robust_se))

# 比較表
se_comparison <- data.frame(
  Estimate = round(coef(iv_model), 5),
  Baseline_SE = round(baseline_se, 5),
  Robust_SE = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)

#(d)
# 需要 boot 套件
library(boot)

# 設定自定義 bootstrap 函數：只回傳教育係數
boot_iv_func <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                   exper + I(exper^2) + mothereduc + fathereduc, data = d)
  return(coef(model)["educ"])
}

# 設定 seed 以利重現
set.seed(123)
boot_iv <- boot(data = mroz2, statistic = boot_iv_func, R = 200)

# Bootstrap 標準誤
boot_se <- sd(boot_iv$t)

# Bootstrap CI
boot_ci <- coef(iv_model)["educ"] + c(-1, 1) * 1.96 * boot_se
print(boot_ci)


