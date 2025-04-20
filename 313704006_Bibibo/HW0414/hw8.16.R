library(POE5Rdata)
data(vacation)

#a
ols = lm(miles ~ income+age+kids, data=vacation)
ols_summary = summary(ols)
beta4est = ols_summary$coefficients[4, 1]
beta4se = ols_summary$coefficients[4, 2]
df = df.residual(ols)
lb = beta4est - qt(0.975, df) * beta4se
ub = beta4est + qt(0.975, df) * beta4se

#b
residual = resid(ols)
income = vacation$income
age = vacation$age
plot(income, residual, main = "res vs income")
plot(age, residual, main = "res vs age")

#c
vacation_sorted = vacation[order(vacation$income), ]
low_income = vacation_sorted[1:90, ]
high_income = vacation_sorted[111:200, ]
low_model = lm(miles ~ income + age + kids, data = low_income)
high_model = lm(miles ~ income + age + kids, data = high_income)
low_df = low_model$df.residual
high_df = high_model$df.residual
f = summary(low_model)$sigma^2 / summary(high_model)$sigma^2
(f > qf(0.025, low_df, high_df))*(qf(0.975, low_df, high_df) < f)
#reject H0

#d
library(lmtest)
library(carData)
library(car)
cov1 = hccm(ols, type = "hc1")
vacation.hci = coeftest(ols, vcov. = cov1)
kidest = vacation.hci[4, 1]
kidse = vacation.hci[4, 2]
lb = kidest - qt(0.975, df) * kidse
ub = kidest + qt(0.975, df) * kidse

#e
weight = 1/vacation$income^2

vacation.gls = lm(miles ~ income + age + kids, weights = weight, data = vacation)
sm = summary(vacation.gls)
gls_est = sm$coefficients[4, 1]
gls_se = sm$coefficients[4, 2]
lb = gls_est - qt(0.975, df) * gls_se
ub = gls_est + qt(0.975, df) * gls_se

cov2 = hccm(vacation.gls, type = "hc1")
vacation.gls.robust = coeftest(vacation.gls, vcov. = cov2)
gls_robust_est = vacation.gls.robust[4, 1]
gls_robust_se = vacation.gls.robust[4, 2]
lb = gls_robust_est - qt(0.975, df) * gls_robust_se
ub = gls_robust_est + qt(0.975, df) * gls_robust_se
