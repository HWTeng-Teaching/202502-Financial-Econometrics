library(POE5Rdata)
data("capm5")

#10.20(a)
capm5$ex_mkt <- capm5$mkt - capm5$riskfree
capm5$ex_msft <- capm5$msft - capm5$riskfree
capm<-lm(ex_msft~ex_mkt,data=capm5)
summary(capm)

#10.20(b)
capm5$rank<-rank(capm5$mkt-capm5$riskfree)
first_stage<-lm(capm5$mkt~capm5$rank,data=capm5)
summary(first_stage)

#10.20(c)
capm5$v_hat<-first_stage$residuals
hausman_model<-lm(ex_msft~ex_mkt+v_hat,data=capm5)
summary(hausman_model)

#10.20(d)
capm_iv<-ivreg(ex_msft~ex_mkt|rank,data=capm5)
summary(capm_iv)

#10.20(e)
capm5$pos<-ifelse(capm5$mkt-capm5$riskfree>0,1,0)
first_stage2<-lm(ex_mkt~pos+rank,data=capm5)
summary(first_stage2)

#10.20(f)
capm5$v_hat2<-first_stage2$residuals
hausman_model2<-lm(ex_msft~ex_mkt+v_hat2,data=capm5)
summary(hausman_model2)

#10.20(g)
capm_iv2<-ivreg(ex_msft~ex_mkt|pos+rank,data=capm5)
summary(capm_iv2)

#10.20(h)
re <- resid(capm_iv2)  # 第二階段的殘差
sargan <- lm(re ~ capm5$rank + capm5$pos)  
R2 <- summary(sargan)$r.squared
n <- nrow(capm5)
S <- n * R2
p_value <- 1 - pchisq(S, df = 1)  

cat("Sargan 統計量 =", round(S, 4), "\n")
cat("p-value =", round(p_value, 4), "\n")
