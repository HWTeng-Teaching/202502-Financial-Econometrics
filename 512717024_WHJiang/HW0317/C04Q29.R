# -----------------------------------------
# Regression Analysis Question 4.29
# -----------------------------------------
# Load necessary package for delta method calculations
library(msm)  # If not installed, run: install.packages("msm")

# -----------------------------------------
# 1. Data Preparation
# -----------------------------------------
# Read the data (adjust file path and name as needed)
data <- read.csv("cex5_small.csv", header = TRUE)

# Subset the data to three-person households (adjust variable name if needed)
subset_data <- subset(data, household_size == 3)

# Inspect the data structure
str(subset_data)

# -----------------------------------------
# (a) Descriptive Statistics & Histograms
# -----------------------------------------
# Summary statistics for FOOD
summary(subset_data$FOOD)
sd(subset_data$FOOD)

# Summary statistics for INCOME
summary(subset_data$INCOME)
sd(subset_data$INCOME)

# Histograms for FOOD and INCOME
hist(subset_data$FOOD, main = "Histogram of FOOD", xlab = "Food Expenditure")
hist(subset_data$INCOME, main = "Histogram of INCOME", xlab = "Household Income")

# Optionally, you can also produce Q-Q plots for normality:
qqnorm(subset_data$FOOD); qqline(subset_data$FOOD, col = "red")
qqnorm(subset_data$INCOME); qqline(subset_data$INCOME, col = "red")

# -----------------------------------------
# (b) Linear Model: FOOD = β1 + β2*INCOME + e
# -----------------------------------------
model_linear <- lm(FOOD ~ INCOME, data = subset_data)
summary(model_linear)

# Scatter plot with fitted line
plot(subset_data$INCOME, subset_data$FOOD,
     main = "FOOD vs INCOME (Linear Model)",
     xlab = "INCOME", ylab = "FOOD Expenditure", pch = 16)
abline(model_linear, col = "red", lwd = 2)

# 95% Confidence Interval for β2 (coefficient of INCOME)
confint_linear <- confint(model_linear, level = 0.95)
confint_linear["INCOME", ]

# -----------------------------------------
# (c) Residual Analysis for Linear Model
# -----------------------------------------
res_linear <- resid(model_linear)
plot(subset_data$INCOME, res_linear,
     main = "Residuals vs INCOME (Linear Model)",
     xlab = "INCOME", ylab = "Residuals", pch = 16)
abline(h = 0, col = "blue", lwd = 2, lty = 2)

# -----------------------------------------
# (d) Elasticity Estimation for Linear Model
#     Elasticity E = (β2*INCOME) / (β1 + β2*INCOME)
#     at INCOME = 19, 65, 160.
# -----------------------------------------
# Rename coefficients for deltaMethod clarity
coef_lin <- coef(model_linear)
names(coef_lin) <- c("b0", "b1")  # b0 = β1, b1 = β2

# Compute elasticity at specified income levels using deltaMethod
elasticity_19 <- deltaMethod(model_linear, "b1*19/(b0 + b1*19)", 
                             parameterNames = c("b0", "b1"))
elasticity_65 <- deltaMethod(model_linear, "b1*65/(b0 + b1*65)", 
                             parameterNames = c("b0", "b1"))
elasticity_160 <- deltaMethod(model_linear, "b1*160/(b0 + b1*160)", 
                              parameterNames = c("b0", "b1"))

print("Elasticity at INCOME = 19:")
print(elasticity_19)
print("Elasticity at INCOME = 65:")
print(elasticity_65)
print("Elasticity at INCOME = 160:")
print(elasticity_160)

# -----------------------------------------
# (e) Log-Log Model: ln(FOOD) = γ1 + γ2*ln(INCOME) + e
# -----------------------------------------
model_loglog <- lm(log(FOOD) ~ log(INCOME), data = subset_data)
summary(model_loglog)

# Scatter plot for log-log model
plot(log(subset_data$INCOME), log(subset_data$FOOD),
     main = "Log-Log Model: log(FOOD) vs log(INCOME)",
     xlab = "log(INCOME)", ylab = "log(FOOD)", pch = 16)
abline(model_loglog, col = "blue", lwd = 2)

# -----------------------------------------
# (f) Elasticity in Log-Log Model
#     In a log-log model, elasticity = γ2 (a constant)
# -----------------------------------------
elasticity_loglog <- coef(model_loglog)["log(INCOME)"]
elasticity_loglog_CI <- confint(model_loglog, level = 0.95)["log(INCOME)", ]
print("Elasticity (log-log model) and 95% CI:")
print(c(Elasticity = elasticity_loglog,
        Lower = elasticity_loglog_CI[1],
        Upper = elasticity_loglog_CI[2]))

# -----------------------------------------
# (g) Residual Analysis for Log-Log Model
# -----------------------------------------
res_loglog <- resid(model_loglog)
plot(log(subset_data$INCOME), res_loglog,
     main = "Residuals vs log(INCOME) (Log-Log Model)",
     xlab = "log(INCOME)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# -----------------------------------------
# (h) Linear-Log Model: FOOD = α1 + α2*ln(INCOME) + e
# -----------------------------------------
model_linlog <- lm(FOOD ~ log(INCOME), data = subset_data)
summary(model_linlog)

# Scatter plot for linear-log model
plot(log(subset_data$INCOME), subset_data$FOOD,
     main = "Linear-Log Model: FOOD vs log(INCOME)",
     xlab = "log(INCOME)", ylab = "FOOD Expenditure", pch = 16)
abline(model_linlog, col = "green", lwd = 2)

# -----------------------------------------
# (i) Elasticity Estimation for Linear-Log Model
#     For FOOD = α1 + α2*ln(INCOME), the derivative is dFOOD/dINCOME = α2/INCOME.
#     Hence, elasticity E = (α2/INCOME) * (INCOME/(α1 + α2*ln(INCOME))) = α2/(α1 + α2*ln(INCOME))
#     Compute at INCOME = 19, 65, and 160.
# -----------------------------------------
# Rename coefficients for deltaMethod clarity
coef_linlog <- coef(model_linlog)
names(coef_linlog) <- c("b0", "b1")  # b0 = α1, b1 = α2

elasticity_linlog_19 <- deltaMethod(model_linlog, "b1/(b0 + b1*log(19))", 
                                    parameterNames = c("b0", "b1"))
elasticity_linlog_65 <- deltaMethod(model_linlog, "b1/(b0 + b1*log(65))", 
                                    parameterNames = c("b0", "b1"))
elasticity_linlog_160 <- deltaMethod(model_linlog, "b1/(b0 + b1*log(160))", 
                                     parameterNames = c("b0", "b1"))

print("Elasticity (linear-log model) at INCOME = 19:")
print(elasticity_linlog_19)
print("Elasticity (linear-log model) at INCOME = 65:")
print(elasticity_linlog_65)
print("Elasticity (linear-log model) at INCOME = 160:")
print(elasticity_linlog_160)

# -----------------------------------------
# (j) Residual Analysis for Linear-Log Model
# -----------------------------------------
res_linlog <- resid(model_linlog)
plot(log(subset_data$INCOME), res_linlog,
     main = "Residuals vs log(INCOME) (Linear-Log Model)",
     xlab = "log(INCOME)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "purple", lwd = 2, lty = 2)

# -----------------------------------------
# (k) Model Comparison (Summary)
# -----------------------------------------
cat("Linear Model Summary:\n")
print(summary(model_linear))
cat("\nLog-Log Model Summary:\n")
print(summary(model_loglog))
cat("\nLinear-Log Model Summary:\n")
print(summary(model_linlog))
# Use these outputs (including R², residual diagnostics, and elasticity estimates)
# to decide which model best suits your data.
