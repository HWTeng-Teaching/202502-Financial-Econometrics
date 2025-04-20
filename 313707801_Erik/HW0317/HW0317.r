#Homework 0317
#Question 4.4

#Part a.
exper <- 0:30
model1 <- 64.298+0.990*exper

# Plotting the fitted values
plot(exper, fitted_values,
    type = "l", col = "blue", lwd = 2,
    main = "Fitted Values from Model 1",
    xlab = "Years of Experience", ylab = "Fitted Performance Rating"
    )


#Part b.
exper2 <- 1:30
model2 <- 39.464 + 15.312 * log(exper2)

# Plotting the fitted values for Model 2
plot(exper, fitted_values,
    type = "l", col = "blue", lwd = 2,
    main = "Fitted Values from Model 1&2",
    xlab = "Years of Experience", ylab = "Fitted Performance Rating"
    )

#Add Model 2 fitted values to the same graph (green line)
    lines(exper2, fitted_values_model2, type = "l", col = "green", lwd = 2)

#Add a legend to distinguish between the models
    legend("topleft",
        legend = c("Model 1", "Model 2"),
        col = c("blue", "green"), lwd = 2
    )

# Part c.
intercept2 <- 39.464
slope2 <- 15.312

marginal_effect_model2 <- function(exper2) {
    slope2/exper2
}

effect_10 <- marginal_effect_model2(10)
effect_20 <- marginal_effect_model2(20)

# Question 4.28

# Part a.
#Load the data
setwd('/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata')
data(wa_wheat)

df <- wa_wheat

df$TIME <- df$time
df$YIELD <- df$northampton

library(ggplot2)

ggplot(df, aes(x = TIME)) +
    geom_point(aes(y = YIELD), color = "black", size = 2) +
    geom_line(aes(y = fitted1), color = "blue", linetype = "solid") +
    geom_line(aes(y = fitted2), color = "green", linetype = "dashed") +
    geom_line(aes(y = fitted3), color = "red", linetype = "dotdash") +
    geom_line(aes(y = fitted4), color = "purple", linetype = "twodash") +
    labs(
        title = "Wheat Yield in Northampton: Model Fits",
        y = "Yield", x = "Time (Years since 1950)"
    ) +
    theme_minimal()
#Part B

par(mfrow = c(2, 2))
plot(resid(model1), main = "Residuals: Linear")
plot(resid(model2), main = "Residuals: Log(TIME)")
plot(resid(model3), main = "Residuals: TIME²")
plot(resid(model4), main = "Residuals: log(YIELD)")
par(mfrow = c(1, 1))

# Part C

library(nortest)

shapiro.test(resid(model1))
shapiro.test(resid(model2))
shapiro.test(resid(model3))
shapiro.test(resid(model4))

#Part D

summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared

#Question 29
#Part a
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("cex5.rdata")

df <- subset(cex5, count == 3)
mean(df$food)
median(df$food)
min(df$food)
max(df$food)
sd(df$food)

mean(df$income)
median(df$income)
min(df$income)
max(df$income)
sd(df$income)

#Histogram for food
hist(df$food,
    breaks = 30, col = "lightblue",
    main = "Histogram of FOOD Expenditure", xlab = "Food"
)
abline(v = mean(df$food), col = "red", lwd = 2, lty = 2)
abline(v = median(df$food), col = "blue", lwd = 2, lty = 2)
legend("topright",
    legend = c("Mean", "Median"),
    col = c("red", "blue"), lty = 2, lwd = 2
)


#Histogram for income
hist(df$income,
    breaks = 30, col = "lightgreen",
    main = "Histogram of INCOME", xlab = "Income"
)
abline(v = mean(df$income), col = "red", lwd = 2, lty = 2)
abline(v = median(df$income), col = "blue", lwd = 2, lty = 2)
legend("topright",
    legend = c("Mean", "Median"),
    col = c("red", "blue"), lty = 2, lwd = 2
)

# Jarque–Bera test for normality - note either load quantmod or tseries depends on set up
library(tseries)

jarque.bera.test(df$food)
jarque.bera.test(df$income)

# Estimate the linear model
model1 <- lm(food ~ income, data = df)
summary(model1)

# Scatter plot with fitted line
plot(df$income, df$food,
     main = 'FOOD vs INCOME with Fitted Line',
     xlab = 'Income', ylab = 'Food',
     pch = 19, col = 'darkgray')
abline(model2, col = 'blue', lwd = 2) 

# 95% Confidence interval for beta2 (slope)
confint(model2, level = 0.95)

#Part c
residuals_food_income <- resid(model_food_income)

plot(df$income, residuals_food_income,
    main = "Residuals vs INCOME",
    xlab = "Income", ylab = "Residuals",
    pch = 19, col = "darkred"
)

#Part d
#finding the coeffficients
b0 <- coef(model1)[1]
b1 <- coef(model1)[2]

#points to evaluatioe
x_vals <- c(19, 65, 160)

#fitted y values at x
y_hat <- b0 + b1 * x_vals

#point estimates of elasticity
point_elasticities <- b1 * x_vals / y_hat

#variance-covariance matrix
vcov_matrix <- vcov(model1)
var_b1 <- vcov_matrix[2, 2]
cov_b0b1 <- vcov_matrix[1, 2]
var_b0 <- vcov_matrix[1, 1]

#calculate se(elasticity) via delta method
se_elast <- function(x) {
    y_hat <- b0 + b1 * x
    dy_db0 <- -b1 * x / y_hat^2
    dy_db1 <- (x / y_hat) - (b1 * x^2 / y_hat^2)

    se_sq <- dy_db0^2 * var_b0 + dy_db1^2 * var_b1 + 2 * dy_db0 * dy_db1 * cov_b0b1
    sqrt(se_sq)
}

#apply to each x
se_vals <- sapply(x_vals, se_elast)

#95% CI bounds
z_crit <- qnorm(0.975)
lower_bounds <- point_elasticities - z_crit * se_vals
upper_bounds <- point_elasticities + z_crit * se_vals

#combine results
results <- data.frame(
    Income = x_vals,
    Fitted_FoodExp = round(y_hat, 2),
    Elasticity = round(point_elasticities, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4)
)

#combined results
library(knitr)
kable(results, caption = "Elasticity Estimates at Different Income Levels")

#part e
# add log variables to your dataset
df$ln_food <- log(df$food)
df$ln_income <- log(df$income)

# fit log-log model
model2 <- lm(ln_food ~ ln_income, data = df)
summary(model2)

#logarithmic scatter plot 
plot(df$ln_income, df$ln_food,
    xlab = "ln(INCOME)", ylab = "ln(FOOD)",
    main = "Log-Log: ln(FOOD) vs ln(INCOME)",
    pch = 19, col = "steelblue"
)

abline(model2, col ="green", lwd = 2)

#linear model comparison 
plot(df$income, df$food,
    xlab = "INCOME", ylab = "FOOD EXPENDITURE",
    main = "Linear: FOOD vs INCOME",
    pch = 19, col = "darkgreen"
)

abline(model1, col = "red", lwd = 2)

#R^2 from linear model
summary(model1)$r.squared

#R^2 from log-log model
model2 <- lm(ln_food ~ ln_income, data = df)
yhat_log <- exp(fitted(model2))
ss_res <- sum((df$food - yhat_log)^2)
ss_tot <- sum((df$food - mean(df$food))^2)
R2_generalized <- 1 - ss_res / ss_tot
R2_generalized

#Part g
#getting the elasticity
elasticity <- coef(model2)[2]
se_elasticity <- summary(model2)$coefficients[2, 2]

#95% confidence interval 
z_crit <- qnorm(0.975)
lower_bound <- elasticity - z_crit * se_elasticity
upper_bound <- elasticity + z_crit * se_elasticity

#organising the results into a table
elasticity_results <- data.frame(
    Estimate = round(elasticity, 4),
    Lower_95CI = round(lower_bound, 4),
    Upper_95CI = round(upper_bound, 4)
)

library(knitr)
kable(elasticity_results, caption = "Elasticity Estimate and 95% CI from Log-Log Model")

#Part g
#Log-log residuals
residuals_loglog <- resid(model2)
#plot residuals vs income
plot(df$ln_income, residuals_loglog,
    main = "Residuals vs ln(INCOME)",
    xlab = "ln(INCOME)", ylab = "Residuals",
    pch = 19, col = "darkred"
)
abline(h = 0, col = "blue", lty = 2)
#histogram
hist(residuals_loglog,
    breaks = 30, col = "lightblue",
    main = "Histogram of Residuals (Log-Log Model)",
    xlab = "Residuals"
)
#Jarque-Bera test
library(tseries)
jarque.bera.test(residuals_loglog)

#part h
#estimate the linear-log model
model3 <- lm(food ~ ln_income, data = df)
summary(model3)

#create scatter plot
plot(df$ln_income, df$food,
    xlab = "ln(INCOME)", ylab = "FOOD EXPENDITURE",
    main = "Linear-Log: FOOD vs ln(INCOME)",
    pch = 19, col = "orange"
)

abline(model3, col = "blue", lwd = 2)

#summarise all the r-squared values
r_squared_table <- data.frame(
    Model = c(
        "Linear (FOOD ~ INCOME)",
        "Log-Log (ln(FOOD) ~ ln(INCOME))",
        "Linear-Log (FOOD ~ ln(INCOME))"
    ),
    R_squared = c(
        summary(model1)$r.squared,
        R2_generalized, # computed manually for log-log model
        summary(model3)$r.squared
    )
)

#load the table 
library(knitr)
kable(r_squared_table, caption = "Comparison of R-squared Values Across Models")

#Part i 
a0 <- coef(model3)[1]
a1 <- coef(model3)[2]

# Income values
x_vals <- c(19, 65, 160)

# Fitted y values
y_hat <- a0 + a1 * log(x_vals)

# Point elasticities
elasticities <- a1 * x_vals / y_hat

# Delta method for standard errors
vcov_matrix <- vcov(model3)
var_a0 <- vcov_matrix[1, 1]
var_a1 <- vcov_matrix[2, 2]
cov_a0a1 <- vcov_matrix[1, 2]

# Function to compute SE using delta method
se_elast <- function(x) {
    y <- a0 + a1 * log(x)
    d_a0 <- -a1 * x / y^2
    d_a1 <- (x * log(x) / y) - (a1 * x * log(x) / y^2)
    se_sq <- d_a0^2 * var_a0 + d_a1^2 * var_a1 + 2 * d_a0 * d_a1 * cov_a0a1
    sqrt(se_sq)
}

# SE for each point
se_vals <- sapply(x_vals, se_elast)

# 95% CI
z_crit <- qnorm(0.975)
lower_bounds <- elasticities - z_crit * se_vals
upper_bounds <- elasticities + z_crit * se_vals

# Output table
results_llog <- data.frame(
    Income = x_vals,
    Fitted_FoodExp = round(y_hat, 2),
    Elasticity = round(elasticities, 4),
    Lower_95CI = round(lower_bounds, 4),
    Upper_95CI = round(upper_bounds, 4)
)

library(knitr)
kable(results_llog, caption = "Elasticity Estimates from Linear-Log Model")