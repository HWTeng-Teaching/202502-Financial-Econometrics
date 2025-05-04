library(POE5Rdata)
data(capm5)
?data
#a
capm5$excess_mkt <- capm5$mkt - capm5$riskfree
capm5$excess_msft <- capm5$msft - capm5$riskfree
capm<-lm(excess_msft~excess_mkt,data=capm5)
summary(capm)
#b
library(AER)
capm5$rank<-rank(capm5$mkt-capm5$riskfree)
first_stqage<-lm(capm5$mkt~capm5$rank,data=capm5)
summary(first_stage)
#c
capm5$v_hat<-first_stage$residuals
Hausman_model<-lm(excess_msft~excess_mkt+v_hat,data=capm5)
summary(Hausman_model)
#d
capm_iv<-ivreg(excess_msft~excess_mkt|rank,data=capm5)
summary(capm_iv)
#e
capm5$pos<-ifelse(capm5$mkt-capm5$riskfree>0,1,0)
first_stage2<-lm(excess_mkt~pos+rank,data=capm5)
summary(first_stage2)
#f
capm5$v_hat2<-first_stage2$residuals
Hausman_model2<-lm(excess_msft~excess_mkt+v_hat2,data=capm5)
summary(Hausman_model2)
#g
capm_iv2<-ivreg(excess_msft~excess_mkt|pos+rank,data=capm5)
summary(capm_iv2)
#h
re <- resid(capm_iv2)  # 第二階段的殘差
sargan <- lm(re ~ capm5$rank + capm5$pos)  
R2 <- summary(sargan)$r.squared
n <- nrow(capm5)
S <- n * R2
p_value <- 1 - pchisq(S, df = 1)  

cat("Sargan 統計量 =", round(S, 4), "\n")
cat("p 值 =", round(p_value, 4), "\n")
