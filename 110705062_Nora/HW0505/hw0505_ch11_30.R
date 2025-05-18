url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/klein.rdata"
file_path <- "klein.rdata"
download.file(url, file_path, mode = "wb")
load(file_path) 

library(AER)
library(car)
library(dplyr)

klein_b <- klein %>%
  select(i, p, plag, klag, g, w2, tx, time, elag) %>%
  na.omit()

##11.30.1
model_a <- lm(i ~ p + plag + klag, data = klein_b)
summary(model_a)

##11.30.2
rf_P <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein_b)
summary(rf_P)

restricted_P <- lm(p ~ plag + klag, data = klein_b)
anova(restricted_P, rf_P) 
F_crit <- qf(0.95, df1 = 5, df2 = 13)
cat("Critical F(5,13;0.95) =", round(F_crit, 3), "\n")
klein_b$phat <- fitted(rf_P)
klein_b$vhat <- resid(rf_P)

##11.30.3
hausman_model <- lm(i ~ p + plag + klag + vhat, data = klein_b)
summary(hausman_model)
cat("Hausman test t-statistic on v̂t =", summary(hausman_model)$coefficients["vhat", "t value"], "\n")

##11.30.4
model_2sls <- ivreg(i ~ p + plag + klag | plag + klag + g + w2 + tx + time + elag, data = klein_b)
summary(model_2sls)

##11.30.5
model_e <- lm(i ~ phat + plag + klag, data = klein_b)
summary(model_e)

##11.30.6
klein_b$resid_2sls <- resid(model_e)
model_sargan <- lm(resid_2sls ~ plag + klag + g + w2 + tx + time + elag, data = klein_b)
summary(model_sargan)

r2_sargan <- summary(model_sargan)$r.squared
n_obs <- nrow(klein_b)
sargan_stat <- n_obs * r2_sargan
cat("Sargan test statistic TR^2 =", sargan_stat, "\n")
cat("Critical value from Chi-square(4, 0.95) =", qchisq(0.95, df = 4), "\n")
