if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("car")  
library(car)
install.packages("AER")  
library(AER)
library(POE5Rdata)
data('klein')
klein <- subset(klein, !is.na(plag))
# (a) 11.18之OLS 
investment_model <- lm(i ~ p + plag + klag, data = klein)

summary(investment_model)
# (b) 簡化方程式及聯合檢定
# 建立 reduced-form 價格模型
profit_model <- lm(p ~ w1 + w2 + plag + klag + g + tx + elag + e , data = klein)
summary(profit_model)
library(car)
try({
  linearHypothesis(profit_model, c("w1 = 0", "w2 = 0", "g = 0", "tx = 0", "elag = 0", "e = 0"))
})
klein$vt <- residuals(profit_model)
klein$phat <- fitted(profit_model)
# (c) Hausmen Test
hausman_model <- lm(i ~ p + plag + klag + vt, data = klein)
summary(hausman_model)

# (d) 2SLS
investment_2sls <- ivreg(i ~ p + plag + klag | w1 + w2 + g + tx + elag + e + plag + klag, data = klein)
summary(investment_2sls)

# (e) 手動2SLS
first_stage <- lm(p ~ w1 + w2 + g + tx + elag + e + plag + klag, data = klein)
klein$p_hat <- predict(first_stage)

second_stage <- lm(i ~ p_hat + plag + klag, data = klein)
summary(second_stage)
# (f) Sargan Test
klein$e2t <- resid(second_stage)

sargan_model <- lm(e2t ~ w1 + w2 + g + tx + elag + e + plag + klag, data = klein)

r_squared <- summary(sargan_model)$r.squared
T <- nobs(sargan_model)

sargan_stat <- T * r_squared

df <- 5  # L - K
crit_val <- qchisq(0.95, df)

cat("Sargan test statistic (T * R²):", sargan_stat, "\n")
cat("Chi-square critical value (df = 5, 95% level):", crit_val, "\n")

if (sargan_stat < crit_val) {
  cat("✅ 結論：無法拒絕 H0，工具變數有效。\n")
} else {
  cat("❌ 結論：拒絕 H0，工具變數可能無效。\n")
}


