url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata"
file_path <- "capm5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(capm5)

##10.20.1
capm5 <- transform(capm5,
                   er_msft =  msft - riskfree,
                   er_mkt  = mkt - riskfree
)


capm_msft <- lm(er_msft ~ er_mkt, data = capm5)
summary(capm_msft)

beta_hat   <- coef(capm_msft)["er_mkt"]
se_beta    <- summary(capm_msft)$coef["er_mkt", "Std. Error"]
ci_beta    <- confint(capm_msft)["er_mkt", ]
list(beta = beta_hat,
     se   = se_beta,
     ci95 = ci_beta)

##10.20.2
capm5$RANK <- rank(capm5$er_mkt, ties.method = "first")
RANK_capm <- lm(er_mkt ~ RANK, data = capm5)
summary(RANK_capm)

##10.20.3
fsresidual = RANK_capm$residuals
exo_test = lm(er_msft ~ er_mkt + fsresidual, data = capm5)
summary(exo_test)

##10.20.4
capm5_iv = ivreg(er_msft ~ er_mkt | capm5$RANK, data = capm5)
capm5_iv $coefficients
capm5_iv $coefficients[2]
capm_msft$coefficients[2]
summary(capm5_iv)

##10.20.5
capm5$pos <- ifelse(capm5$er_mkt > 0, 1, 0)
first_stage_2IV <- lm(er_mkt ~ RANK + pos, data = capm5)
summary(first_stage_2IV)

##10.20.6
re = first_stage_2IV$residuals
exo_test = lm(er_msft ~ er_mkt + re, data = capm5)
summary(exo_test) 

##10.20.7
capm5_2iv = ivreg(er_msft ~ er_mkt | capm5$RANK + capm5$pos, data = capm5)
summary(capm5_2iv, diagnostics = TRUE)
capm5_2iv $coefficients
capm5_2iv $coefficients[2] 
capm_msft$coefficients[2]

##10.20.8
re = resid(capm5_2iv)
sargan = lm(re ~ capm5$RANK + capm5$pos, data = capm5)
R2 = summary(sargan)$r.squared
n = nrow(capm5)
S = n * R2
p_value = 1 - pchisq(S, df = 1)
p_value










































