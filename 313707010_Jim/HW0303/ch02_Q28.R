library(POE5Rdata)
data("cps5_small")

# a
print('WAGE')
print(summary(cps5_small$wage))
print('EDUC')
print(summary(cps5_small$educ))

hist(cps5_small$wage, main="Histogram of WAGE", xlab="WAGE", col="skyblue", border="white")
hist(cps5_small$educ, main="Histogram of EDUC", xlab="EDUC", col="lightgreen", border="white")

# b
model_linear <- lm(wage ~ educ, data=cps5_small)
summary(model_linear)

# c
residuals_linear <- resid(model_linear)
plot(cps5_small$educ, residuals_linear, main="Residuals vs EDUC", xlab="EDUC", ylab="Residuals", pch=19, col="purple")

# d
model_male <- lm(wage ~ educ, data=subset(cps5_small, female == 0))
model_female <- lm(wage ~ educ, data=subset(cps5_small, female == 1))
model_black <- lm(wage ~ educ, data=subset(cps5_small, black == 1))
model_white <- lm(wage ~ educ, data=subset(cps5_small, black == 0))

summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

# e
model_quadratic <- lm(wage ~ I(educ^2), data=cps5_small)
summary(model_quadratic)

marginal_effect_12 <- 2 * coef(model_quadratic)["I(educ^2)"] * 12
marginal_effect_16 <- 2 * coef(model_quadratic)["I(educ^2)"] * 16
print('when educ = 12, ME=')
print(marginal_effect_12)
print('when educ = 16, ME=')
print(marginal_effect_16)

# f
fitted_linear <- predict(model_linear)
fitted_quadratic <- predict(model_quadratic)

plot(cps5_small$educ, cps5_small$wage, main="WAGE vs EDUC", xlab="EDUC", ylab="WAGE", pch=19, col="gray")
lines(cps5_small$educ, fitted_linear, col="blue", lwd=2)

order_index <- order(cps5_small$educ)
lines(cps5_small$educ[order_index], fitted_quadratic[order_index], col="red", lwd=2)
legend("topright", legend=c("Data", "Linear Fit", "Quadratic Fit"), col=c("gray", "blue", "red"), lwd=2)
