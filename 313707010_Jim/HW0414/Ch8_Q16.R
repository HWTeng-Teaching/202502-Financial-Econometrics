library(POE5Rdata)
library(lmtest)
library(sandwich)

data(vacation)

# (a)
ols_model <- lm(miles ~ income + age + kids, data = vacation)

summary(ols_model)

confint(ols_model, "kids", level = 0.95)

# (b)

residuals <- resid(ols_model)
plot(vacation$income, residuals, main = "Residuals vs INCOME", xlab = "INCOME", ylab = "Residuals")
abline(h = 0, col = "red")

plot(vacation$age, residuals, main = "Residuals vs AGE", xlab = "AGE", ylab = "Residuals")
abline(h = 0, col = "red")

# (c)
sorted_data <- vacation[order(vacation$income), ]
gqtest(miles ~ income + age + kids, order.by = ~income, data = sorted_data, fraction = 0.1)

# (d)
robust_se <- vcovHC(ols_model, type = "HC1")    
coeftest(ols_model, vcov = robust_se)   
coefci(ols_model, vcov. = robust_se, level = 0.95)  

# (e)
vacation$weights <- 1 / (vacation$income^2)   

model_gls <- lm(miles ~ income + age + kids, data = vacation, weights = weights)
summary(model_gls)
confint(model_gls, level = 0.95) 

gls_robust_se <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov. = gls_robust_se)
coefci(model_gls, vcov. = gls_robust_se, level = 0.95)  
