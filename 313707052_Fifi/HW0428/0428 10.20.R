library(POE5Rdata)
data("capm5", package = "POE5Rdata")

names(capm5)

#a.

# 計算超額報酬
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt <- capm5$mkt - capm5$riskfree

# OLS 回歸
ols_model <- lm(excess_msft ~ excess_mkt, data = capm5)
summary(ols_model)

#a.ans
#risky

#b.

capm5$RANK <- rank(capm5$excess_mkt)
first_stage <- lm(excess_mkt ~ RANK, data = capm5)
summary(first_stage)

#b.ans
#r square is 0.9126 and the p-value is significant.it seem to be a strong iv.

#c.

capm5$resid_first_stage <- resid(first_stage)
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt <- capm5$mkt - capm5$riskfree

hausman_test <- lm(excess_msft ~ excess_mkt + resid_first_stage, data = capm5)
summary(hausman_test)

#c.ans
#At the 1% level we cannot reject the null hypothesis that the market return is exogenous. 

#d.

capm5$excess_msft <- capm5$msft - capm5$riskfree
capm5$excess_mkt <- capm5$mkt - capm5$riskfree

library(AER)

iv_d <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm5)
summary(iv_d)

#d.ans

#ols beta might be measurement error,led to the beta is underestimated.using rank as iv could solve this problem,so this way beta should be higher than ols,the answer is agree with my expectation

#e.

capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$pos <- ifelse(capm5$msft_excess > 0, 1, 0)

first_stage <- lm(mkt_excess ~ pos + RANK, data = capm5)
summary(first_stage)


#e.ans
#the r square and f-stat is high, we can conclude that the iv is not weak.

#f.

first_stage <- lm(mkt_excess ~ pos + RANK, data = capm5)
capm5$residual_iv <- resid(first_stage)

hausman_test <- lm(msft_excess ~ mkt_excess + residual_iv, data = capm5)
summary(hausman_test)

#f.ans
# at the 1% level we cannot reject the null hypothesis that the market return is exogenous.

#g.

library(AER)

iv_both <- ivreg(excess_msft ~ excess_mkt | RANK + pos, data = capm5)
summary(iv_both)

#ols beta might be measurement error,led to the beta is underestimated.using rank as iv could solve this problem,so this way beta should be higher than ols,the answer is agree with my expectation

#H.

library(AER)
library(ivmodel)
iv_capm <- ivreg(msft_excess ~ mkt_excess | pos + RANK, data = capm5)
summary(iv_capm)
ivmod <- ivmodel(
  Y = capm5$msft_excess,
  D = capm5$mkt_excess,
  Z = capm5[, c("pos", "RANK")]
)
summary(ivmod, diagnostics = TRUE)

#Sargan Test Statistics=63.03218,greater than the critical value,so we reject h0,these are not good iv. 
