#-----------------C08Q06-----------------
N = 1000
k = 4

#(a)
Nm = 577
SSEm = 97161.9174
STD2f = 12.024^2

STD2m = SSEm/(Nm-k)
Nf = N-Nm
GQ = STD2m/STD2f
cvl = qf(0.05/2, Nm-k, Nf-k)
cvr = qf(1 - 0.05/2, Nm-k, Nf-k)
cat("GQ statistic:",GQ,"\ncritical value: [",cvl,",",cvr,"]")

#(b)
k_b = 5
Ns = 400
SSEs = 56231.0382
Nm = 600
SSEm = 100703.0471

STD2s = SSEs/(Ns-k)
STD2m = SSEm/(Nm-k)
GQ = STD2m/STD2s
cv = qf(1-0.05, Nm-k, Nf-k)
cat("GQ statistic:",GQ,"\ncritical value:",cv)

#(c)
NR2 = 59.03
cv = qchisq(1 - 0.05, k_b - 1)
cat("NR-squared statistic:",NR2,"\ncritical value:",cv)

#(d)
NR2 = 78.82
df = (1+4+2+3+1)-1
cv = qchisq(1 - 0.05, df)
cat("degrees of freedom:",df,"\nNR-squared statistic:",NR2,"\ncritical value:",cv)

#-----------------C08Q16-----------------
rm(list = ls())
library(POE5Rdata)
data(vacation)
mod = lm(miles~income+age+kids,data = vacation)
sum = summary(mod)
N = nrow(vacation)
k = 4
b = sum[["coefficients"]][1:k,1]
se_b = sum[["coefficients"]][1:k,2]
cov = vcov(mod)

#(a)
se_b4 = sqrt(cov[4,4])
t = qt(1-0.05/2,N-k)
CI = c(b[4]-(t*se_b4),b[4]+(t*se_b4))
cat("95% CI: [", CI[1], ", ", CI[2], "]")
#(b)
plot(vacation$income, vacation$miles-predict(mod), 
     xlab="INCOME", 
     ylab="Residuals")
plot(vacation$age, vacation$miles-predict(mod), 
     xlab="AGE", 
     ylab="Residuals")

#(c)
sorted_vac_income <- vacation[order(vacation$income),]
mod_f90 <- lm(miles~income+age+kids,data = sorted_vac_income[1:90,])
mod_l90 <- lm(miles~income+age+kids,data = sorted_vac_income[(N-89):N,])

STD2_f90 = summary(mod_f90)$sigma^2
STD2_l90 = summary(mod_l90)$sigma^2

GQ = STD2_f90/STD2_l90
cvl = qf(0.05/2, 90-k, 90-k)
cvr = qf(1-0.05/2, 90-k, 90-k)
cat("GQ statistic:",GQ,"\ncritical value: [",cvl,",",cvr,"]")

#(d)
library(car)
robust_cov = hccm(mod, type = "hc1")
robust_se_b4 = sqrt(robust_cov[4,4])
t = qt(1-0.05/2,N-k)
CI = c(b[4]-(t*robust_se_b4),b[4]+(t*robust_se_b4))
cat("robust 95% CI: [", CI[1], ", ", CI[2], "]")

#(e)
mod_gls <- lm(miles~income+age+kids, data = vacation, weights = 1/(income^2))
sum_gls = summary(mod_gls)

b_gls = sum[["coefficients"]][1:k,1]
cov_gls = vcov(mod_gls)
se_gls_b4 = sqrt(cov_gls[4,4])

t = qt(1-0.05/2,N-k)
CI_GLS = c(b_gls[4]-(t*se_gls_b4),b_gls[4]+(t*se_gls_b4))
robust_cov_gls = hccm(mod_gls, type = "hc1")
robust_se_gls_b4 = sqrt(robust_cov_gls[4,4])
t = qt(1-0.05/2,N-k)
robust_CI_GLS = c(b_gls[4]-(t*robust_se_gls_b4),b_gls[4]+(t*robust_se_gls_b4))

cat("GLS 95% CI       : [", CI_GLS[1], ", ", CI_GLS[2], "]"
    ,"\nrobust GLS 95% CI: [", robust_CI_GLS[1], ", ", robust_CI_GLS[2], "]")


#-----------------C08Q18-----------------
rm(list = ls())
library(POE5Rdata)
data(cps5)
reg = log(wage) ~ educ+exper+I(exper^2)+female+black+metro+south+midwest+west
N = nrow(cps5)
k = 10

#(a)
cps5_male <- subset(cps5, female == 0)
cps5_female <- subset(cps5, female == 1)
mod_male <- lm(reg, data = cps5_male)
mod_female <- lm(reg, data = cps5_female)
STD2_male = sum((mod_male$residuals)^2)/(mod_male$df.residual)
STD2_female = sum((mod_male$residuals)^2)/(mod_female$df.residual)

GQ = STD2_male/STD2_female
cvl = qf(0.05/2,     mod_male$df.residual, mod_female$df.residual)
cvr = qf(1 - 0.05/2, mod_male$df.residual, mod_female$df.residual)
cat("GQ statistic:",GQ,"\ncritical value: [",cvl,",",cvr,"]")

#(b)
mod <- lm(reg, data = cps5)
mod_resid <- lm(I(resid(mod)^2) ~ metro + female + black, data = cps5)
NR2 = N*summary(mod_resid)$r.squared
cv = qchisq(1 - 0.01, 4-1)
mod_resid <- lm(I(resid(mod)^2) ~ educ+exper+I(exper^2)+female+black+metro+south+midwest+west, data = cps5)
NR2_all = N*summary(mod_resid)$r.squared
cv_all = qchisq(1 - 0.01, 10-1)

cat("NR2 (METRO, FEMALE, BLACK):",NR2,"critical value:",cv,
    "\nNR2 (all exp. variables  ):",NR2_all,"critical value:",cv_all)

#(c)
mod_white <- lm(I(resid(mod)^2) ~ 
                educ+exper+female+black+metro+south+midwest+west
                +I(educ^2)+I(exper^2)
                +(educ*exper)+(educ*female)+(educ*black)+(educ*metro)+(educ*south)+(educ*midwest)+(educ*west)
                +(exper*female)+(exper*black)+(exper*metro)+(exper*south)+(exper*midwest)+(exper*west)
                +(female*black)+(female*metro)+(female*south)+(female*midwest)+(female*west)
                +(black*metro)+(black*south)+(black*midwest)+(black*west)
                +(metro*south)+(metro*midwest)+(metro*west)
                
                , data = cps5)
NR2 = N*summary(mod_white)$r.squared
cv = qchisq(1 - 0.01, 36-1)

cat("NR2:",NR2,"critical value:",cv)

#(d)
library(car)
library(lmtest)
robust_cov <- hccm(mod, type = "hc1")
OLS = coeftest(mod)
robust = coeftest(mod,vcov.= robust_cov)
print(OLS)
print(robust)

#(e)(f)
std2_mod <- lm(log(resid(mod)^2) ~ metro+exper, data = cps5)
vari = exp(fitted(std2_mod))
mod_fgls = lm(reg,weights=1/vari,data = cps5)
FGLS = coeftest(mod_fgls)
robust_cov_fgls = hccm(mod_fgls, type = "hc1")
robust_FGLS = coeftest(mod_fgls,vcov=robust_cov_fgls)
print(FGLS)
print(robust_FGLS)
