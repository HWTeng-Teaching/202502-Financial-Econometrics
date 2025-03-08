# e

quadratic_model <- lm(wage ~ poly(educ, 2, raw=TRUE), data=cps5_small)

summary(quadratic_model)

alpha2 <- coef(quadratic_model)[3]
marginal_12 <- 2 * alpha2 * 12
marginal_16 <- 2 * alpha2 * 16

marginal_12
# 2.50421
marginal_16
# 3.338946 

# beta_2 = 2.3968

#In the quadratic regression, the marginal effect of education for both 12 years and 16 years of education is higher than in the linear regression.


# f

plot(cps5_small$educ, cps5_small$wage, main="Linear vs Quadratic Regression", xlab="EDUC", ylab="WAGE", col="gray")

# Add linear regression fit line
abline(linear_model, col="blue", lwd=2)

# Add quadratic regression fit line
educ_range <- seq(min(cps5_small$educ), max(cps5_small$educ), length=100)
quadratic_fit <- predict(quadratic_model, newdata=data.frame(educ=educ_range))
lines(educ_range, quadratic_fit, col="red", lwd=2)

#The quadratic model seems to fit the data better.

