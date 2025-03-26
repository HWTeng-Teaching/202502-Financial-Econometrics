#4.4
#a.
library(ggplot2)
library(dplyr)

mod1_intercept <- 64.289
mod1_slope <- 0.990
mod2_intercept <- 39.464
mod2_slope <- 15.312

exper_values1 <- 0:30  # Model 1: 0 to 30 years
ratings_mod1 <- mod1_intercept + mod1_slope * exper_values1

exper_values2 <- 1:30  # Model 2: 1 to 30 years (since ln(0) is undefined)
ratings_mod2 <- mod2_intercept + mod2_slope * log(exper_values2)


#a. Plot
p1 <- ggplot(data.frame(EXPER = exper_values1, RATING = ratings_mod1), aes(x = EXPER, y = RATING)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Model 1: RATING = 64.289 + 0.990*EXPER",
       x = "Experience (years)", y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(p1)

#b. Plot
p2 <- ggplot(data.frame(EXPER = exper_values2, RATING = ratings_mod2), aes(x = EXPER, y = RATING)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Model 2: RATING = 39.464 + 15.312*ln(EXPER)",
       x = "Experience (years)", y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(p2)

#c.
# Comparison plot for both models
plot_data <- data.frame(
  Experience = c(exper_values2, exper_values2),
  Rating = c(mod1_intercept + mod1_slope * exper_values2, ratings_mod2),
  Model = factor(rep(c("Linear (Model 1)", "Logarithmic (Model 2)"), each = length(exper_values2)))
)

combined_plot <- ggplot(plot_data, aes(x = Experience, y = Rating, color = Model)) +
  geom_line(size = 1) +
  labs(title = "Comparison of Model 1 and Model 2",
       x = "Experience (years)", 
       y = "Performance Rating") +
  theme_minimal() +
  ylim(60, 100)
print(combined_plot)


#d.
exper_seq <- 1:30
me_values <- mod2_slope / exper_seq

me_plot <- ggplot(data.frame(EXPER = exper_seq, MarginalEffect = me_values), aes(x = EXPER, y = MarginalEffect)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Marginal Effect of Experience on Rating in Model 2",
       x = "Experience (years)", 
       y = "Marginal Effect") +
  theme_minimal()
print(me_plot)


#4.28
load("C:\\Users\\thaol\\Downloads\\wa_wheat.rdata")
print(wa_wheat)

# 1) YIELD = β0 + β1 * TIME
model1 <- lm(northampton ~ time, data = wa_wheat)
# 2) YIELD = α0 + α1 * ln(TIME)
model2 <- lm(northampton ~ log(time), data = wa_wheat)
# 3) YIELD = γ0 + γ1 * TIME^2
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)
# 4) ln(YIELD) = φ0 + φ1 * TIME
model4 <- lm(log(northampton) ~ time, data = wa_wheat)
# Summary of each model
summary(model1)  # For Model 1
summary(model2)  # For Model 2
summary(model3)  # For Model 3
summary(model4)  # For Model 4


#1.1. Plotting Fitted Equations and Residuals
par(mfrow = c(2, 2))

# Model 1: YIELD = β0 + β1 * TIME
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 1: Linear Fit")
abline(model1, col = "red")
plot(model1$residuals, main = "Residuals for Model 1", type = "p")

# Model 2: YIELD = α0 + α1 * ln(TIME)
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 2: Log(Time) Fit")
points(wa_wheat$time, predict(model2), type = "p", col = "blue")
plot(model2$residuals, main = "Residuals for Model 2", type = "p")

# Model 3: YIELD = γ0 + γ1 * TIME^2
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 3: Time^2 Fit")
points(wa_wheat$time, predict(model3), type = "p", col = "green")
plot(model3$residuals, main = "Residuals for Model 3", type = "p")

# Model 4: ln(YIELD) = φ0 + φ1 * TIME
plot(wa_wheat$time, log(wa_wheat$northampton), main = "Model 4: ln(Yield) vs. Time")
points(wa_wheat$time, predict(model4), type = "p", col = "orange")
plot(model4$residuals, main = "Residuals for Model 4", type = "p")


#1.2. Calculate effor normality test
# Shapiro-Wilk normality test for residuals
sw_test1 <- shapiro.test(model1$residuals)
sw_test2 <- shapiro.test(model2$residuals)
sw_test3 <- shapiro.test(model3$residuals)
sw_test4 <- shapiro.test(model4$residuals)

# Print results
print(sw_test1)
print(sw_test2)
print(sw_test3)
print(sw_test4)

#1.3 Calculate R squared
R2_1 <- summary(model1)$r.squared
R2_2 <- summary(model2)$r.squared
R2_3 <- summary(model3)$r.squared
R2_4 <- summary(model4)$r.squared

# Print R-squared values
print(paste("R-squared for Model 1:", R2_1))
print(paste("R-squared for Model 2:", R2_2))
print(paste("R-squared for Model 3:", R2_3))
print(paste("R-squared for Model 4:", R2_4))



#c.
stud_res <- rstudent(model3)
# Identify large studentized residuals
# Common threshold: greater than 2 in absolute value
large_stud_res <- which(abs(stud_res) > 2)

leverage <- hatvalues(model3)
# Identify high leverage points
# Common rule of thumb: leverage greater than 2*(p/n)
high_leverage <- which(leverage > 2 * (2 / length(leverage)))

dfbetas_values <- dfbetas(model3)
# Identify large DFBETAS
# Common threshold: greater than 2/sqrt(n)
threshold_dfbetas <- 2 / sqrt(nrow(wa_wheat))
large_dfbetas <- which(abs(dfbetas_values) > threshold_dfbetas, arr.ind = TRUE)

dfbetas_values <- dfbetas(model3)
# Identify large DFBETAS
# Common threshold: greater than 2/sqrt(n)
threshold_dfbetas <- 2 / sqrt(nrow(wa_wheat))
large_dfbetas <- which(abs(dfbetas_values) > threshold_dfbetas, arr.ind = TRUE)

dffits_values <- dffits(model3)
# Identify large DFFITS
# Common threshold: greater than 2*sqrt(p/n)
threshold_dffits <- 2 * sqrt(2 / length(dffits_values))
large_dffits <- which(abs(dffits_values) > threshold_dffits)


# Print results
cat("Large Studentized Residuals Indices:", large_stud_res, "\n")
cat("High Leverage Points Indices:", high_leverage, "\n")
cat("Large DFBETAS Indices (row: observation, column: parameter):", large_dfbetas, "\n")
cat("Large DFFITS Indices:", large_dffits, "\n")


#4.29
#a.
load("C:\\Users\\thaol\\Downloads\\cex5_small.rdata")
print(cex5_small)


summary(cex5_small$food)
summary(cex5_small$income)
std_food <- sd(cex5_small$food)
std_income <- sd(cex5_small$income)
print(std_food)
print(std_income)
hist(cex5_small$food, main = "Histogram of Food")
hist(cex5_small$income, main = "Histogram of Income")
install.packages("tseries")
library(tseries)

jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

#b.

mod_b = lm(food~income, data=cex5_small)
smod_b <- summary(mod_b)
smod_b
plot(cex5_small$food, cex5_small$income,  main = "food vs income with Linear model")
abline(mod_b, col = "blue", lwd = 2)
ci <- confint(mod_b, level = 0.95)
print(ci)

#c.
ehat <- mod_b$residuals
plot(cex5_small$income, ehat, xlab="income", ylab="residuals")
hist(ehat, main = "Linear model residual histogram")
jarque.bera.test(ehat)


#d.
# Vector of incomes
x <- c(19, 65, 160)
b1 <- coef(mod_b)[1]
b2 <- coef(mod_b)[2]
std_error_b2 <- smod_b$coefficients["income", "Std. Error"]
yhatx <- b1+b2*x
e <- b2 * x/yhatx
se_e_linaer <- std_error_b2 *x / yhatx
se_e_lb <- e - 1.96 * se_e_linaer
se_e_ub <- e + 1.96 * se_e_linaer
print(yhatx)
print(e)
print(se_e_lb)
print(se_e_ub)






#e.
mod_e <- lm(log(food)~log(income), data=cex5_small)
b1e <- coef(mod_e)[[1]]
b2e <- coef(mod_e)[[2]]
smod_e <- summary(mod_e)
smod_e
# Drawing the fitted values of the log-log equation
plot(log(cex5_small$income), log(cex5_small$food),
     main = "log(food) vs log(income)", col = "darkblue")
abline(mod_e, col = "red", lwd = 2)
sighat2 <- smod_e$sigma^2
# yĉ = exp(b1 + b2 log(x) + σ2̂ /2) 
yhatc <- exp(b1e+b2e*log(cex5_small$income)+sighat2/2)
rg <- cor(cex5_small$food, yhatc)^2
print(rg)


#f, for Log-Log model, elasticity is b2
cie <- confint(mod_e, level = 0.95)
print(cie)


#g.
ehat_f <- mod_e$residuals
plot(log(cex5_small$income), ehat_f, main = "Residuals (log-log) vs log(income)",col = "darkblue")
abline(ehat_f, col = "red", lwd = 2)
hist(ehat_f, col="grey", freq=FALSE, main="",
     ylab="density", xlab="ehat")
jarque.bera.test(ehat_f)


#h. 
mod_h <- lm(food~log(income), data=cex5_small)
plot(log(cex5_small$income), cex5_small$food, main = "food vs log(income)")
abline(mod_h, col = "red", lwd = 2)
smod_h <- summary(mod_h)
smod_h


#i.
x <- c(19, 65, 160)
b1h <- coef(mod_h)[1]
b2h <- coef(mod_h)[2]
std_error_b2h <- smod_h$coefficients["log(income)", "Std. Error"]
yhatxh <- b1h+b2h*log(x)
eh <- b2h/yhatxh
se_e_linaerh <- std_error_b2h / yhatxh
se_e_lbh <- eh - 1.96 * se_e_linaerh
se_e_ubh <- eh + 1.96 * se_e_linaerh
print(yhatxh)
print(eh)
print(se_e_lbh)
print(se_e_ubh)



#j.
ehat_h <- mod_h$residuals
plot(log(cex5_small$income), ehat_h, main = "Residuals (Linear-log) vs log(income)",col = "darkblue")
abline(mod_h, col = "red", lwd = 2)
hist(ehat_h, col="grey", freq=FALSE, main="",
     ylab="density", xlab="ehat")
jarque.bera.test(ehat_h)