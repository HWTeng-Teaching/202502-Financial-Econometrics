library(POE5Rdata)
data(capm5)
rm = capm5$mkt
rf = capm5$riskfree
rj = capm5$msft

#a 
#(rj − r𝑓) = αj + βj (rm − r𝑓)
y = rj - rf
x = rm - rf
ols = lm(y~x)
ols$coefficients
cat("βj:", round(ols$coefficients[2], 2))
#若 β > 1：風險大（報酬對市場反應強）

#b
library(AER)
capm5$rank = rank(capm5$mkt - capm5$riskfree)
first_stage = lm(x ~ capm5$rank)
summary(first_stage)
#p-value < 2e-16, sinificant
#Multiple R-squared:  0.9126
#yes, strong IV

#c
fsresidual = first_stage$residuals
exo_test = lm(y ~ x + fsresidual)
summary(exo_test)
#fsresidual p-value = 0.0428 > 0.01
#non-reject H0
#x is exogenous

#d
capm5_iv = ivreg(y ~ x | capm5$rank, data = capm5)
capm5_iv $coefficients
if(capm5_iv $coefficients[2] == ols$coefficients[2]){
  print("same coef with OLS")
}else{
  print("different coef with OLS")
}

#e
capm5$pos = ifelse(capm5$mkt - capm5$riskfree > 0, 1, 0)
first_stage_2IV = lm(x ~ capm5$rank + capm5$pos)
summary(first_stage_2IV)#Multiple R-squared:  0.9149
capm5_2iv = ivreg(y ~ x | capm5$rank + capm5$pos)
summary(capm5_2iv, diagnostics = TRUE)
#Weak instruments p-value <2e-16 strong IV
#Sargan p-value = 0.4549 instruments are valid

#f
re = first_stage_2IV$residuals
exo_test = lm(y ~ x + re)
summary(exo_test) 
# re p-value 0.0287 < 0.05,endogenous

#g
capm5_2iv $coefficients
if(capm5_2iv $coefficients[2] == ols$coefficients[2]){
  print("same coef with OLS")
}else{
  print("different coef with OLS")
}

#h
re = resid(capm5_2iv)
sargan = lm(re ~ capm5$rank + capm5$pos)
R2 = summary(sargan)$r.squared
n = nrow(capm5)
S = n * R2
p_value = 1 - pchisq(S, df = 1)
#0.45488 non-reject H0
#valid IV


