#Homework 0331
#Question 5.6

#Part a, b, c
# Given coefficients
beta <- c(2, 3, -1)

# Given covariance matrix
cov_beta <- matrix(
    c(
        3, -2, 1,
        -2, 4, 0,
        1, 0, 3
    ),
    nrow = 3, byrow = TRUE
)

# Sample size and degrees of freedom
n <- 63
k <- 3
df <- n - k

# Wald test function
wald_test <- function(R, r, beta, cov_beta) {
    diff <- R %*% beta - r
    var_Rb <- R %*% cov_beta %*% t(R)
    W <- (diff)^2 / var_Rb
    F_value <- W
    p_value <- 1 - pf(F_value, 1, df)
    return(list(F_value = F_value, p_value = p_value))
}

# 1. Test H0: β2 = 0
R1 <- matrix(c(0, 1, 0), nrow = 1)
r1 <- 0
result1 <- wald_test(R1, r1, beta, cov_beta)

# 2. Test H0: β1 + 2β2 = 5
R2 <- matrix(c(1, 2, 0), nrow = 1)
r2 <- 5
result2 <- wald_test(R2, r2, beta, cov_beta)

# 3. Test H0: β1 - β2 + β3 = 4
R3 <- matrix(c(1, -1, 1), nrow = 1)
r3 <- 4
result3 <- wald_test(R3, r3, beta, cov_beta)

# Show results
result1
result2
result3

# Question 5.31

# Part a
setwd("/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata")
load("commute5.rdata")

library(ggplot2)
summary(commute5)

model1 <- lm(time ~ depart + reds + trains, data = commute5)

plot(commute5)
library(car)
library(carData
vif(model1)
plot(model1)

# Part b
# 95% Confidence Intervals for coefficients
confint(model1, level = 0.95)

#Part c
# Extract coefficient and standard error for reds
b3 <- coef(model1)["reds"]
se_b3 <- summary(model1)$coefficients["reds", "Std. Error"]

# Null hypothesis value
c <- 2

# Calculate t-statistic
t_value <- (b3 - c) / se_b3

# Degrees of freedom
df <- df.residual(model1)

# Calculate the p-value for a left-tail test
p_value <- pt(t_value, df)

# Print results
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")

#Part d
# Extract coefficient and standard error for trains
b4 <- coef(model1)["trains"]
se_b4 <- summary(model1)$coefficients["trains", "Std. Error"]

# Null hypothesis value
c <- 3

# Calculate t-statistic
t_value <- (b4 - c) / se_b4

# Degrees of freedom
df <- df.residual(model1)

# Calculate two-tailed p-value
p_value <- 2 * (1 - pt(abs(t_value), df))

# Print results
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")

#Part e

# Coefficient and standard error for DEPART from your model
b2 <- coef(model1)["depart"]
se_b2 <- summary(model1)$coefficients["depart", "Std. Error"]

# Difference in departure time: 7:30 AM vs 7:00 AM = 30 minutes
time_diff <- 30

# Expected increase in commute time
expected_increase <- time_diff * b2

# Standard error of the increase
se_increase <- time_diff * se_b2

# Null hypothesis: expected increase >= 10 minutes
# t-statistic for left-tail test
t_value <- (expected_increase - 10) / se_increase

# Degrees of freedom
df <- df.residual(model1)

# Calculate p-value (right-tail test for H0: increase >= 10)
p_value <- 1 - pt(t_value, df)

# Output results
cat("Expected Increase:", round(expected_increase, 3), "minutes\n")
cat("Standard Error:", round(se_increase, 3), "\n")
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")

#part f
# Extract coefficients
b3 <- coef(model1)["reds"]
b4 <- coef(model1)["trains"]

# Calculate L = b4 - 3*b3
L <- b4 - 3 * b3

# Extract variance-covariance matrix of coefficients
vcov_matrix <- vcov(model1)

# Calculate variance of L: var(L) = var(b4) + 9*var(b3) - 6*cov(b3, b4)
var_b3 <- vcov_matrix["reds", "reds"]
var_b4 <- vcov_matrix["trains", "trains"]
cov_b3b4 <- vcov_matrix["reds", "trains"]

var_L <- var_b4 + 9 * var_b3 - 6 * cov_b3b4
se_L <- sqrt(var_L)

# Null hypothesis: L >= 0 (H1: L < 0)
t_value <- (L - 0) / se_L
df <- df.residual(model1)
p_value <- pt(t_value, df)

# Output results
cat("L (b4 - 3*b3):", round(L, 3), "\n")
cat("Standard Error of L:", round(se_L, 3), "\n")
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")

#part f
# Extract coefficients
b0 <- coef(model1)["(Intercept)"]
b2 <- coef(model1)["depart"]
b3 <- coef(model1)["reds"]
b4 <- coef(model1)["trains"]

# Input values for DEPART = 30, REDS = 6, TRAINS = 1
depart_val <- 30
reds_val <- 6
trains_val <- 1

# Compute expected TIME
expected_time <- b0 + b2 * depart_val + b3 * reds_val + b4 * trains_val

# Create new data row for prediction
newdata <- data.frame(depart = depart_val, reds = reds_val, trains = trains_val)

# Predict with standard error
pred <- predict(model1, newdata, se.fit = TRUE)

# Extract predicted value and standard error
fit <- pred$fit
se_fit <- pred$se.fit

# Null hypothesis: Expected TIME ≤ 75 minutes
# t-statistic for right-tail test
t_value <- (fit - 75) / se_fit
df <- df.residual(model1)
p_value <- 1 - pt(t_value, df)

# Output results
cat("Expected Commute Time:", round(fit, 3), "minutes\n")
cat("Standard Error:", round(se_fit, 3), "\n")
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 4), "\n")