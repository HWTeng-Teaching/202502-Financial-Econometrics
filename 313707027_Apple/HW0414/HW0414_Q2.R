# 8.16
library(POE5Rdata)
data(vacation)

# (a)
model <- lm(miles ~ income + age + kids, data = vacation)
summary(model)
confint(model, "kids", level = 0.95)

#(b)
residuals <- resid(model)

par(mfrow = c(1, 2))
plot(vacation$income, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals",
     pch = 16, cex = 0.7, col = "blue")
abline(h = 0, lwd = 2, col = "red")

plot(vacation$age, residuals,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 16, cex = 0.7, col = "blue")
abline(h = 0, lwd = 2, col = "red")

 

#(c)
vacation_sorted <- vacation[order(vacation$income),]
n <- nrow(vacation_sorted)
first90 <- vacation_sorted[1:90,]
last90 <- vacation_sorted[(n-89):n,]

mod1 <- lm(miles ~ income + age + kids, data = first90)
mod2 <- lm(miles ~ income + age + kids, data = last90)

rss1 <- sum(resid(mod1)^2)
rss2 <- sum(resid(mod2)^2)

gq_stat <- rss2 / rss1

df1 <- mod1$df.residual
df2 <- mod2$df.residual

alpha <- 0.05
F_critical <- qf(1 - alpha, df1, df2)

cat("GQ statistic:", gq_stat, "\n")
cat("Critical value at 5% level:", F_critical, "\n")

if (gq_stat > F_critical) {
  cat("Reject the null hypothesis: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject the null hypothesis: No evidence of heteroskedasticity.\n")
}

#(d)
# install.packages("sandwich")
# install.packages("lmtest")
library(sandwich)
library(lmtest)

model_OLS <- lm(miles ~ income + age + kids, data = vacation)

robust_se <- vcovHC(model_OLS, type = "HC1")

coeftest(model_OLS, vcov. = robust_se)

confint_robust <- coeftest(model_OLS, vcov. = robust_se)
beta_kids <- confint_robust["kids", 1]
se_kid <- confint_robust["kids", 2]

lower_OLS <- beta_kids - 1.96 * se_kids
upper_OLS <- beta_kids + 1.96 * se_kids

cat(lower_OLS, upper_OLS)


#(e)
vacation$weights_gls <- 1 / (vacation$income^2)

model_GLS <- lm(miles ~ income + age + kids, data = vacation, weights = weights_gls)

summary(model_GLS)
confint(model_GLS, level = 0.95)["kids", ]

robust_GLS_se <- vcovHC(model_GLS, type = "HC1")
coeftest(model_GLS, vcov. = robust_GLS_se)
beta_kids_GLS <- coef(model_GLS)["kids"]
se_kids_GLS <- sqrt(robust_GLS_se["kids", "kids"])

lower_GLS <- beta_kids_GLS - 1.96 * se_kids_GLS
upper_GLS <- beta_kids_GLS + 1.96 * se_kids_GLS

cat(lower_GLS, upper_GLS)
