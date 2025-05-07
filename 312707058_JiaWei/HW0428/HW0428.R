#-----------------C10Q18-----------------
rm(list = ls())
library(POE5Rdata)
data("mroz")

#(a)
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)
pert = mean(mroz$mothercoll == 1 & mroz$fathercoll == 1)
cat("percentage of parents have some college education:",100*pert,"%")
#(b)
cor(mroz[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")

#(c)
#install.packages("AER")
library(AER)
mroz1 <- mroz[mroz$lfp==1,]
mod_c <- ivreg(log(wage)~exper+I(exper^2)+educ|exper+I(exper^2)+mothercoll,data = mroz1)
sum_c = summary(mod_c)
sum_c$coefficients
coef_educ = sum_c$coefficients[4,]
t = qt(1-0.05/2,mod_c$df.residual)
cat(" 95% CI: [", coef_educ[1]-t*coef_educ[2],", ",coef_educ[1]+t*coef_educ[2],"]",
    "\n interval width:",2*t*coef_educ[2])

#(d)
library(car)
mod_d <- lm(educ~exper+I(exper^2)+mothercoll,data = mroz1)
linearHypothesis(mod_d,"mothercoll = 0")

#(e)
mod_e <- ivreg(log(wage)~exper+I(exper^2)+educ|exper+I(exper^2)+mothercoll+fathercoll,data = mroz1)
sum_e = summary(mod_e)
sum_e$coefficients
coef_educ = sum_e$coefficients[4,]
t = qt(1-0.05/2,mod_e$df.residual)
cat(" 95% CI: [", coef_educ[1]-t*coef_educ[2],", ",coef_educ[1]+t*coef_educ[2],"]",
    "\n interval width:",2*t*coef_educ[2])

#(f)
mod_f <- lm(educ~exper+I(exper^2)+mothercoll+fathercoll,data = mroz1)
linearHypothesis(mod_f,c("mothercoll = 0","fathercoll = 0"))

#(g)
summary(mod_e,diagnostics=TRUE)

#-----------------C10Q20-----------------
rm(list = ls())
library(POE5Rdata)
library(AER)
data("capm5")
#(a)
capm <- data.frame(x = capm5$mkt-capm5$riskfree,
                   y = capm5$msft-capm5$riskfree)
mod <- lm(y ~ x,data=capm)
beta = coef(mod)[[2]]
print(summary(mod))
#(b)
capm$rank <- rank(capm$x, ties.method = "min")
mod_b <- lm(x~rank,data = capm)
sum_b = summary(mod_b)
sum_b
linearHypothesis(mod_b,"rank = 0")
#(c)
capm$fs_resid = mod_b$residuals
mod_c <- lm(y~x+fs_resid,data=capm)
sum_c = summary(mod_c)
sum_c
# H0: gamma = 0 (market return is exogenous)
# H1: gamma != 0 (market return is endogenous)
#(d)
mod_d <- ivreg(y~x|rank,data = capm)
print(summary(mod_d))
#(e)
capm$pos <- ifelse(capm$x > 0, 1, 0)
mod_e <- lm(x~rank+pos,data = capm)
sum_e = summary(mod_e)
sum_e
linearHypothesis(mod_e,c("rank = 0", "pos = 0"))
#(f)
capm$fs_resid2 = mod_e$residuals
mod_f <- lm(y~x+fs_resid2,data=capm)
sum_f = summary(mod_f)
sum_f
#(g)
mod_g <- ivreg(y~x|rank+pos,data = capm)
print(summary(mod_g))
#(h)
n = nrow(capm)
capm$resid_h = mod_g$residuals
mod_h <- lm(resid_h~rank+pos,data=capm)
sum_h = summary(mod_h)
NR2 = n*sum_h$r.squared
cv = qchisq(1 - 0.05, 2-1)
cat("NR2:",NR2,"\ncritical value:",cv)
summary(mod_g,diagnostics=TRUE)

#-----------------C10Q24-----------------
rm(list = ls())
library(POE5Rdata)
library(AER)
data("mroz")
mroz1 <- mroz[mroz$lfp==1,]
mod <- ivreg(log(wage)~exper+I(exper^2)+educ|exper+I(exper^2)+mothereduc+fathereduc,data = mroz1)
coef = mod$coefficients

#(a)
plot(mroz1$exper,mod$residuals,
     xlab="EXPER", 
     ylab="residuals", 
    )

#(b)
n = nrow(mroz1)
resid2 = mod$residuals^2
mod_b = lm(resid2~mroz1$exper)
sum_b = summary(mod_b)
sum_b
NR2 = n*sum_b$r.squared
cv = qchisq(1 - 0.05, 2-1)
cat("NR2:",NR2,"\ncritical value:",cv)

#(c)
robust_se <- sqrt(diag(vcovHC(mod, type = "HC3")))
baseline_se <- summary(mod)$coefficients[,2]
baseline_se
robust_se
t = qt(1-0.05/2,mod$df.residual)
cat(" 95% CI: [", coef[4]-t*robust_se[4],", ",coef[4]+t*robust_se[2],"]",
    "\n interval width:",2*t*robust_se[4])
#(d)
library(boot) 

bootstrap_function <- function(data, indices) {
  data_bootstrap <- data[indices, ]
  mod_bootstrap <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothereduc + fathereduc, data = data_bootstrap)
  return(coef(mod_bootstrap))
}


set.seed(42)
bootstrap_results <- boot(data = mroz1, statistic = bootstrap_function, R = 200)
bootstrap_se <- apply(bootstrap_results$t, 2, sd)
baseline_se
robust_se
bootstrap_se
cat(" 95% CI: [", coef[4]-t*bootstrap_se[4],", ",coef[4]+t*bootstrap_se[2],"]",
    "\n interval width:",2*t*bootstrap_se[4])
