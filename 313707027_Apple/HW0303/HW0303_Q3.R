# 2.28
# (a)
data("cps5_small")
summary(cps5_small$educ)
summary(cps5_small$wage)
par(mfrow=c(1,2))  
hist(cps5_small$educ,
     breaks = 30, xlab = "EDUC", ylab = "Frequency",
     col = rgb(0.1, 0.6, 0.8, 0.7),
     main = 'Histogram of EDUC')
hist(cps5_small$wage,breaks = 30,xlab = "WAGE", ylab = "Frequency",
     col = rgb(0.1, 0.6, 0.8, 0.7),
     main = 'Histogram of WAGE')

# (b)
model <- lm(wage~educ, data = cps5_small)
summary(model)

# (c)
residuals <- residuals(model)
plot(cps5_small$educ, residuals, 
     main = "Residuals vs. EDUC", 
     xlab = "EDUC", 
     ylab = "Residuals", 
     pch = 16, col = "blue",cex = 0.5)
# (d)
male_model <- lm(wage~educ, data = cps5_small, subset = (female == 0))
female_model <- lm(wage~educ, data = cps5_small, subset = (female == 1))
black_model <- lm(wage~educ, data = cps5_small, subset = (black == 1))
white_model <- lm(wage~educ, data = cps5_small, subset = (black == 0))

summary(male_model)
summary(female_model)
summary(black_model)
summary(white_model)

# (e)
quad_model <- lm(wage ~ I(educ^2), data = cps5_small)
summary(quad_model)

alpha2 <- coef(quad_model)["I(educ^2)"]
ME_12 <- 2 * alpha2 * 12
ME_16 <- 2 * alpha2 * 16

linear_model <- lm(wage~educ, data = cps5_small)
summary(linear_model)
beta2 <- coef(linear_model)["educ"]
beta2

# (f)
education_range <- seq(min(cps5_small$educ), max(cps5_small$educ), length.out = 100)

linear_fit <- predict(model, newdata = data.frame(educ = education_range))
quad_fit <- predict(quad_model, newdata = data.frame(educ = education_range))


plot(cps5_small$educ, cps5_small$wage, 
     pch = 16, col = "gray", 
     xlab = "EDUC", ylab = "WAGE", 
     main = "Comparison of Linear and Quadratic Regression")
lines(education_range, linear_fit, col = "red", lwd = 1.5)
lines(education_range, quad_fit, col = "blue", lwd = 1.5)
legend("topleft", legend = c("Linear Fit", "Quadratic Fit"),
       col = c("red", "blue"), lwd = 1.5)
