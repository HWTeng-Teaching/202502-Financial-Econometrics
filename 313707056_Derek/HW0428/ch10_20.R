library(POE5Rdata)
library(car)
library(AER)
library(plm)
data("capm5")
capm5$risk_premium_msft <- capm5$msft-capm5$riskfree  
capm5$risk_premium_market <- capm5$mkt - capm5$riskfree

capm_ols <- lm(risk_premium_msft~risk_premium_market,data = capm5)
summary(capm_ols)

capm5$RANK <- rank(capm5$risk_premium_market)

first_stage <- lm(risk_premium_market~RANK,data = capm5)
summary(first_stage)

# c
capm5$residuals_first_stage <- residuals(first_stage)
model_augmented <- lm(risk_premium_msft ~ risk_premium_market + residuals_first_stage, data = capm5)

summary(model_augmented)
# d 
capm_iv <- ivreg(risk_premium_msft~risk_premium_market | RANK,data = capm5)
summary(capm_iv)
# e
capm5$POS <- ifelse(capm5$risk_premium_market > 0, 1, 0)
first_stage2 <- lm(risk_premium_market~RANK+POS,data = capm5)
summary(first_stage2)
#f
capm_iv2 <- ivreg(risk_premium_msft~risk_premium_market | RANK+POS,data = capm5)
summary(capm_iv2,diagnostics = TRUE)
# 進行Hausman檢定
hausman_test <- phtest(capm_ols, capm_iv2)
summary(hausman_test)
