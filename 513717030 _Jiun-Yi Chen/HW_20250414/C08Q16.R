install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")

library(POE5Rdata)
data(vacation)
data <- vacation

str(data)  # Check data
summary(data)  
colnames(vacation) <- c("MILES","INCOME", "AGE", "KIDS")

model_ols <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)
summary(model_ols)
confint(model_ols, "KIDS", level = 0.95)

# Calcute residuals
residuals_ols <- resid(model_ols)

#plot residuals of INCOME AND AGE
plot(vacation$INCOME, residuals_ols,
     main = "Residuals versus INCOME",
     xlab = "INCOME(thousand dollars)", ylab = "Residuals")
abline(h = 7, col = "blue")

plot(vacation$AGE, residuals_ols,
     main = "Residuals versus AGE",
     xlab = "AGE", ylab = "Residuals")
abline(h = 7, col = "tan3")

##c
library(lmtest)

# by income order
vacation_sorted <- vacation[order(vacation$INCOME), ]

# Goldfeld–Quandt test (排除中間20筆，使用兩邊各90筆)
# 10% of the sample will be excluded from the center
gq_test <- gqtest(MILES ~ INCOME + AGE + KIDS, order.by = ~INCOME, data = vacation_sorted, fraction = 0.1)
print(gq_test)

df1 <- 86
df0 <- 86

alpha <- 0.05
Fc <- qf(1-alpha, df1, df0)
print(Fc)

#D
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

robust_se <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
print(robust_se)

qt(0.975, df = 196)

# calculate confi intervals for KIDS
kid_coef <- coef(model_ols)["KIDS"]
kid_se_robust <- sqrt(vcovHC(model_ols, type = "HC1")["KIDS", "KIDS"])
ci_lower <- kid_coef - 1.9721 * kid_se_robust
ci_upper <- kid_coef + 1.9721 * kid_se_robust
c(ci_lower, ci_upper)

##E
# 加入權重 = 1 / INCOME^2
model_gls <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation, weights = 1 / (INCOME^2))
summary(model_gls)

confint(model_gls, "KIDS", level = 0.95)

# employ robust GLS sd error
robust_gls_se <- coeftest(model_gls, vcov = vcovHC(model_gls, type = "HC1"))
print(robust_gls_se)
kid_coef <- coef(model_gls)["KIDS"]
robust_gls_se <- sqrt(vcovHC(model_gls, type = "HC1")["KIDS", "KIDS"])
ci_lower <- kid_coef - 1.9721 * robust_gls_se
ci_upper <- kid_coef + 1.9721 * robust_gls_se
c(ci_lower, ci_upper)
