library(AER)
library(car)
library(POE5Rdata)
data('capm5')

# (a)
y <- capm5$msft - capm5$riskfree 
x <- capm5$mkt - capm5$riskfree

data_capm <- data.frame(y = y, x = x)
model <- lm(y ~ x, data = data_capm)
summary(model)

# (b)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$RANK <- rank(capm5$mkt_excess, ties.method = "first")

first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)

# (c)
capm5$v <- residuals(first_stage)
capm5$y <- capm5$msft - capm5$riskfree
augmented_model <- lm(y ~ mkt_excess + v, data = capm5)
summary(augmented_model)

# (d)
iv_model <- ivreg(y ~ mkt_excess | RANK, data = capm5)
summary(iv_model)

# (e)
capm5$POS <- ifelse(capm5$mkt > capm5$riskfree, 1, 0)
first_stage_2 <- lm(mkt_excess ~ RANK + POS, data = capm5)
summary(first_stage_2)

linearHypothesis(first_stage_2, c("RANK = 0", "POS = 0"))

# (f)
capm5$v_2 <- residuals(first_stage_2)
augmented_model_2 <- lm(y ~ mkt_excess + v_2, data = capm5)
summary(augmented_model_2, diagnostics = TRUE)

# (g)
iv_model <- ivreg(y ~ mkt_excess | RANK + POS, data = capm5)
summary(iv_model)

# (h)
v_hat <- residuals(iv_model)

sargan_aux <- lm(v_hat ~ RANK + POS, data = capm5)
summary(sargan_aux)

n <- nobs(sargan_aux)                   
R2 <- summary(sargan_aux)$r.squared     
J_stat <- n * R2

df <- 2 - 1
p_value <- pchisq(J_stat, df = df, lower.tail = FALSE)

cat("NR-square:", J_stat, "\n")
cat("P-value:", p_value, "\n")
