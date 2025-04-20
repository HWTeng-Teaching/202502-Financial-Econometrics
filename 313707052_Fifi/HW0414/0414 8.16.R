library(POE5Rdata)
data("vacation", package = "POE5Rdata")

names(vacation)

#a.

model_ols <- lm(miles ~ income + age + kids, data = vacation)

summary(model_ols)

confint(model_ols, "kids", level = 0.95)

#b.

vacation$resid <- resid(model_ols)

plot(vacation$income, vacation$resid,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals")
abline(h = 0, col = "red")

plot(vacation$age, vacation$resid,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals")
abline(h = 0, col = "red")

#c.

vacation_sorted <- vacation[order(vacation$income), ]

first90 <- vacation_sorted[1:90, ]
last90  <- vacation_sorted[(nrow(vacation_sorted) - 89):nrow(vacation_sorted), ]

model_first90 <- lm(miles ~ income + age + kids, data = first90)
model_last90  <- lm(miles ~ income + age + kids, data = last90)

summary(model_first90)
summary(model_last90)

library(lmtest)

gqtest(miles ~ income + age + kids, order.by = ~ income, data = vacation,
       fraction = 0.1) 

qf(0.95,86,86)

#d.

library(sandwich)
library(lmtest)

model_ols <- lm(miles ~ income + age + kids, data = vacation)

robust_se <- vcovHC(model_ols, type = "HC1")

robust_se

coeftest(model_ols, vcov. = robust_se)

ci_robust <- coefci(model_ols, vcov. = robust_se, level = 0.95)
ci_robust["kids", ]

#e.

w <- 1 / (vacation$income)^2
model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = w)

summary(model_gls)
confint(model_gls, "kids", level = 0.95)

library(sandwich)
library(lmtest)

coeftest(model_gls, vcov. = vcovHC(model_gls, type = "HC1"))

coefci(model_gls, "kids", vcov. = vcovHC(model_gls, type = "HC1"), level = 0.95)

