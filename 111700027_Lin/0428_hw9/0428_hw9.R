install.packages(POE5Rdata)
library(POE5Rdata)

data(mroz)

#10.18
#(a)
data = mroz[mroz$lfp==1,]

data$MOTHERCOLL = ifelse(data$mothereduc > 12, 1, 0)
data$FATHERCOLL = ifelse(data$fathereduc > 12, 1, 0)

cat( mean(data$MOTHERCOLL)*100, "%", "\n" )
cat( mean(data$FATHERCOLL)*100, "%", "\n" )

#(b)
cor(data$educ, data$MOTHERCOLL, use = "complete.obs")
cor(data$educ, data$FATHERCOLL, use = "complete.obs")
cor(data$MOTHERCOLL, data$FATHERCOLL, use = "complete.obs")

#(c)
install.packages("ivreg")
library(ivreg)

iv1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = data)
summary(iv1)
confint(iv1, "educ", level = 0.95)

#(d)
install.packages(car)
library(car)

test = lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = data)
summary(test)
Anova(test, type = "II")#II=每個變數在控制其他變數後的邊際效果忽略交互作用

#(e)
iv2 = ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = data)
summary(iv2)
confint(iv2, "educ", level = 0.95)

#(f)(g)
summary(iv2, diagnostics = TRUE)

#10.20
#(a)
data(capm5)

capm5$rj_rf = capm5$msft - capm5$riskfree
capm5$rm_rf = capm5$mkt - capm5$riskfree

ols_model = lm(rj_rf ~ rm_rf, data = capm5)
summary(ols_model)

#(b)
capm5$RANK = rank(capm5$rm_rf)

first_stage = lm(rm_rf ~ RANK, data = capm5)
summary(first_stage)

#(c)
capm5$v = resid(first_stage)

aug_model = lm(rj_rf ~ rm_rf + v, data = capm5)
summary(aug_model)

#(d)
iv_model = ivreg(rj_rf ~ rm_rf | RANK, data = capm5)
summary(iv_model)

#(e)
capm5$POS = as.numeric(capm5$rm_rf > 0)

first_stage = lm(rm_rf ~ RANK + POS, data = capm5)
summary(first_stage)

anova(first_stage)

#(f)
capm5$v2 = resid(first_stage)

hausman_test = lm(rj_rf ~ rm_rf + v2, data = capm5)
summary(hausman_test)

#(g)
iv_model = ivreg(rj_rf ~ rm_rf | RANK + POS, data = capm5)
summary(iv_model)

#(h)
capm5$u = resid(iv_model)

sargan_aux = lm(u ~ RANK + POS, data = capm5)
n = nrow(capm5)
R2 = summary(sargan_aux)$r.squared

sargan_stat = n * R2
p_sargan = 1 - pchisq(sargan_stat, df = 1)

#10.24
#(a)
data(mroz)
install.packages("dplyr")   
library(dplyr)

data = mroz[mroz$lfp==1, ]
iv = ivreg(log(wage)~educ+exper+I(exper^2)| exper+I(exper^2)+mothereduc+fathereduc, data = data)

vcov_mroz = vcov(iv)
residual_of_2SLS = resid(iv)
experience = data$exper
plot(experience, residual_of_2SLS)

abline(h = 0, col = "red", lty = 2)

#(b)
squared_resid = residual_of_2SLS^2
hetero_test = lm(squared_resid ~ experience, data = data)

n = nrow(data)
R2 = summary(hetero_test)$r.squared
NR2 = n * R2
p_value = 1 - pchisq(NR2, df = 1)

cat("NR^2 =", NR2, "\nP-value =", p_value, "\n")

#(c)
robust_se = sqrt(diag(vcovHC(iv, type = "HC1")))

educ_coef = coef(iv)["educ"]
educ_se_robust = robust_se["educ"]

lower_robust = educ_coef - 1.96 * educ_se_robust
upper_robust = educ_coef + 1.96 * educ_se_robust

cat("Robust SE for EDU:", educ_se_robust, "\n")
cat("95% CI for EDU using robust SE: [", lower_robust, ",", upper_robust, "]\n")

#(d)
set.seed(123)
boot_iv = function(data, indices) {
  d = data[indices, ]
  model = ivreg(log(wage) ~ educ + exper + I(exper^2) |
                  exper + I(exper^2) + mothereduc + fathereduc, data = d)
  return(coef(model)["educ"])
}

library(boot)
boot_result = boot(data = data, statistic = boot_iv, R = 200)

educ_se_boot = sd(boot_result$t)

boot_ci = quantile(boot_result$t, probs = c(0.025, 0.975))

cat("Bootstrap SE for EDU:", educ_se_boot, "\n")
cat("95% CI for EDU using bootstrap: [", boot_ci[1], ",", boot_ci[2], "]\n")