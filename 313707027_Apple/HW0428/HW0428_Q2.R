# 10.20
data(capm5)

# (a)
capm5$msft_rp <- capm5$msft - capm5$riskfree
capm5$mkt_rp <- capm5$mkt - capm5$riskfree
ols_model <- lm(msft_rp ~ mkt_rp, data = capm5)
summary(ols_model)

# (b)
capm5$RANK <- rank(capm5$mkt_rp)
first_stage <- lm(mkt_rp ~ RANK, data = capm5)
summary(first_stage)

# (c)
capm5$vhat <- residuals(first_stage)
augmented_model <- lm(msft_rp ~ mkt_rp + vhat, data = capm5)
summary(augmented_model)

# (d)
library(AER)
iv_model <- ivreg(msft_rp ~ mkt_rp | RANK, data = capm5)
summary(iv_model)

# (e)
capm5$POS <- ifelse(capm5$mkt_rp > 0, 1, 0)
first_stage_2IV <- lm(mkt_rp ~ RANK + POS, data = capm5)
summary(first_stage_2IV)

# (f)
capm5$v_hat <- residuals(first_stage_2IV)
hausman_model <- lm(msft_rp ~ mkt_rp + v_hat, data = capm5)
summary(hausman_model)

# (g)
iv_2sls_model_2iv <- ivreg(msft_rp ~ mkt_rp | RANK + POS, data = capm5)
summary(iv_2sls_model_2iv)

# (h)
capm5$resid_2sls <- residuals(iv_2sls_model_2iv)
sargan_aux <- lm(resid_2sls ~ RANK + POS, data = capm5)
summary(sargan_aux)

n <- nrow(capm5)
R2 <- summary(sargan_aux)$r.squared
sargan_stat <- n * R2
df <- 2 - 1 

p_value <- 1 - pchisq(sargan_stat, df)

cat("Sargan test statistic:", sargan_stat, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value, "\n")

