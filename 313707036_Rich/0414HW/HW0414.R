library(POE5Rdata)
data <- POE5Rdata::vacation

#8.16.a
mod1 <- lm(miles ~ income + age + kids , data = data)
conf <- confint(mod1 , level = 0.95) 

#8.16.b
plot(data$income , mod1$residuals,
     xlab = 'Income',
     ylab = 'Residuals',
     main = 'Residual plot with respect to income')
plot(data$age , mod1$residuals,
     xlab = 'Age',
     ylab = 'Residuals',
     main = 'Residual plot with respect to age')

#8.16.c
sort <- data[order(data$income),]
low90 <- sort[1:90,]
high90 <- sort[(nrow(sort)-89):nrow(sort),]

modl <- lm(miles ~ income + age + kids , data = low90)
modh <- lm(miles ~ income + age + kids , data = high90)
pivot <- sum(residuals(modh)^2)/sum(residuals(modl)^2)
crit <- qf(1 - 0.05, modh$df.residual, modl$df.residual)
cat(pivot,crit)

#8.16.d
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)
# 計算穩健標準誤
robust_se <- sqrt(diag(vcovHC(mod1 , type = "HC1")))

# 顯示穩健標準誤的回歸結果
coeftest(mod1, vcov. = vcovHC(mod1, type = "HC1"))

coefficients <- coef(mod1)
t_value <- qt(0.975, df = df.residual(mod1))  # 95%信賴區間，alpha = 0.05

# 計算信賴區間
lower_bound <- coefficients - t_value * robust_se
upper_bound <- coefficients + t_value * robust_se

# 顯示信賴區間
data.frame(
  Estimate = coefficients,
  Lower_CI = lower_bound,
  Upper_CI = upper_bound
)

#8.16.e
install.packages("nlme")
library(nlme)
# 擬合GLS模型
gls_model <- lm(miles ~ income + age + kids, data = data, 
                 weights = 1/(income^2))

conf_conv_gls <- confint(gls_model, level = 0.95)

kid_coef <- coef(gls_model)["kids"]
robust_gls_se <- sqrt(vcovHC(gls_model, type = "HC1")["kids", "kids"])
ci_lower <- kid_coef - 1.9721 * robust_gls_se
ci_upper <- kid_coef + 1.9721 * robust_gls_se
c(ci_lower, ci_upper)

#Q8.18.a

data <- POE5Rdata::cps5
male <- subset(data, female == 0)
female <- subset(data, female == 1)
modm <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west , data = male)
modf <- lm(log(wage) ~ educ + exper + I(exper^2) + black + metro + south + midwest + west , data = female)
df0 <- modm$df.residual
df1 <- modf$df.residual
fstat <- (summary(modm)$sigma^2) / (summary(modf)$sigma^2)
flc <- qf(0.025,df0,df1)
fuc <- qf(0.975,df0,df1)
cat(flc,fuc,fstat)

#Q8.18.b
base <- lm(log(wage) ~ educ + exper + I(exper^2)
           + female + black + metro
           + south  + midwest + west,
           data = cps5)
bptest(base,
       varformula = ~ metro + female + black,
       data = cps5)

bptest(base,
       varformula = ~ educ + exper + I(exper^2)
       + female + black + metro
       + south  + midwest + west,
       data = cps5)

#Q8.18.c
bptest(base,
       varformula = ~ .^2,      # “.^2” = all terms + all pairwise interactions + squares
       data = cps5)

#Q8.18.d
library(sandwich)
library(lmtest)

conf_int_ols <- confint(base)

robust_cov  <- vcovHC(base, type = "HC0")
robust_tst  <- coeftest(base, robust_cov)
conf_int_hc <- confint(robust_tst)

list(OLS_CI = conf_int_ols,
     Robust_CI = conf_int_hc)

#Q8.18.e

vmod <- lm(resid(base)^2 ~ metro + exper, data = cps5)
sigma2_hat <- predict(vmod)

# 2. FGLS (WLS) fit
gls1 <- lm(log(wage) ~ educ + exper + I(exper^2)
           + female + black + metro
           + south  + midwest + west,
           weights = 1/sigma2_hat,
           data = cps5)

summary(gls1)

# 3. 95% CIs
confint(gls1)

#Q8.18.f
# Robust SEs on gls1
robust_cov_gls1 <- vcovHC(gls1, type = "HC0")
coeftest(gls1, robust_cov_gls1)
confint(coeftest(gls1, robust_cov_gls1))



