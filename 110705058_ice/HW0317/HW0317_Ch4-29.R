
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
download.file(url, destfile = "cex5_small.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("cex5_small.rdata")

str(cex5_small)  # 查看結構
head(cex5_small)  


summary(cex5_small$food)

stats <- data.frame(
  Variable = c("Food", "Income"),
  Mean = c(mean(cex5_small$food), mean(cex5_small$income)),
  Median = c(median(cex5_small$food), median(cex5_small$income)),
  Min = c(min(cex5_small$food), min(cex5_small$income)),
  Max = c(max(cex5_small$food), max(cex5_small$income)),
  SD = c(sd(cex5_small$food), sd(cex5_small$income))
)
print(stats)

# Histogram for FOOD
hist(cex5_small$food, main="Histogram of Food Expenditure", xlab="Food ($)", col="lightblue", breaks=30)
abline(v=mean(cex5_small$food), col="red", lwd=2, lty=2)     # Mean
abline(v=median(cex5_small$food), col="blue", lwd=2, lty=2)  # Median
legend("topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=2, lwd=2)

# Histogram for INCOME
hist(cex5_small$income, main="Histogram of Income", xlab="Income ($)", col="lightgreen", breaks=30)
abline(v=mean(cex5_small$income), col="red", lwd=2, lty=2)
abline(v=median(cex5_small$income), col="blue", lwd=2, lty=2)
legend("topright", legend=c("Mean", "Median"), col=c("red", "blue"), lty=2, lwd=2)


install.packages("tseries")   # Run only if not installed
library(tseries)

jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

model_food_income <- lm(food ~ income, data = cex5_small)
summary(model_food_income)

plot(cex5_small$income, cex5_small$food,
     main = "FOOD vs INCOME",
     xlab = "Income ($)", ylab = "Food Expenditure ($)",
     pch = 16, col = "steelblue")

abline(model_food_income, col = "red", lwd = 2)
legend("topleft", legend = "Fitted Line", col = "red", lwd = 2)

confint(model_food_income, level = 0.95)

# sum(is.na(cex5_small$income))
# sum(is.na(cex5_small$food))


residuals <- resid(model_food_income)

# Residuals vs INCOME plot
plot(cex5_small$income, residuals,
     main = "Residuals vs Income",
     xlab = "Income ($)", ylab = "Residuals",
     pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Histogram
hist(residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightgray")
abline(v = mean(residuals), col = "blue", lty = 2)
abline(v = median(residuals), col = "purple", lty = 2)
legend("topright", legend = c("Mean", "Median"), col = c("blue", "purple"), lty = 2)

# Jarque-Bera test
library(tseries)
jarque.bera.test(residuals)

income_vals <- c(19, 65, 160)
beta1 <- coef(model_food_income)["(Intercept)"]
beta2 <- coef(model_food_income)["income"]

# Confidence interval for β2
beta2_CI <- confint(model_food_income, level = 0.95)["income", ]

# Function to calculate elasticity and its 95% CI
get_elasticity <- function(income, beta1, beta2, beta2_CI) {
  y_hat <- beta1 + beta2 * income
  elasticity <- beta2 * income / y_hat
  elasticity_lower <- beta2_CI[1] * income / y_hat
  elasticity_upper <- beta2_CI[2] * income / y_hat
  return(data.frame(
    Income = income,
    Fitted_Food = y_hat,
    Elasticity = elasticity,
    Lower_CI = elasticity_lower,
    Upper_CI = elasticity_upper
  ))
}

# Apply for all income values
elasticity_results <- do.call(rbind, lapply(income_vals, get_elasticity, beta1, beta2, beta2_CI))
print(elasticity_results)


# Remove zero or negative values (log can't handle them)
log_data <- subset(cex5_small, food > 0 & income > 0)

# Fit log-log model
model_loglog <- lm(log(food) ~ log(income), data = log_data)

# Summary
summary(model_loglog)

plot(log(log_data$income), log(log_data$food),
     main = "Log-Log: ln(FOOD) vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "ln(FOOD)",
     pch = 16, col = "blue")

abline(model_loglog, col = "red", lwd = 2)

data.frame(
  Model = c("Linear", "Log-Log"),
  R_squared = c(summary(model_food_income)$r.squared,
                summary(model_loglog)$r.squared)
)

summary(model_loglog)
# confint(model_loglog, level = 0.95)
beta1 <- coef(model_loglog)["(Intercept)"]
beta2 <- coef(model_loglog)["log(income)"]

# Confidence interval for β2
beta2_CI <- confint(model_loglog, level = 0.95)["log(income)", ]

# Function to calculate elasticity and its 95% CI
get_elasticity <- function(income, beta1, beta2, beta2_CI) {
  ln_y_hat <- beta1 + beta2 * log(income)
  y_hat <- exp(ln_y_hat)
  elasticity <- beta2 
  elasticity_lower <- beta2_CI[1] 
  elasticity_upper <- beta2_CI[2]
  return(data.frame(
    Income = income,
    Fitted_Food = y_hat,
    Elasticity = elasticity,
    Lower_CI = elasticity_lower,
    Upper_CI = elasticity_upper
  ))
}
elasticity_results <- do.call(rbind, lapply(income_vals, get_elasticity, beta1, beta2, beta2_CI))
print(elasticity_results)

residuals <- resid(model_loglog)
log_income <- log(cex5_small$income)
# Residuals vs INCOME plot
plot(log_income, residuals,
     main = "Residuals vs Income",
     xlab = "log(income) ($)", ylab = "Residuals",
     pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Histogram
hist(residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightgray")
abline(v = mean(residuals), col = "blue", lty = 2)
abline(v = median(residuals), col = "purple", lty = 2)
legend("topright", legend = c("Mean", "Median"), col = c("blue", "purple"), lty = 2)

# Jarque-Bera test
library(tseries)
jarque.bera.test(residuals)


# Fit log-log model
model_loglog <- lm(log(food) ~ log(income), data = log_data)

# Summary
summary(model_loglog)

plot(log(log_data$income), log(log_data$food),
     main = "Log-Log: ln(FOOD) vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "ln(FOOD)",
     pch = 16, col = "blue")

abline(model_loglog, col = "red", lwd = 2)

data.frame(
  Model = c("Linear", "Log-Log"),
  R_squared = c(summary(model_food_income)$r.squared,
                summary(model_loglog)$r.squared)
)

summary(model_loglog)
# confint(model_loglog, level = 0.95)
beta1 <- coef(model_loglog)["(Intercept)"]
beta2 <- coef(model_loglog)["log(income)"]

# Confidence interval for β2
beta2_CI <- confint(model_loglog, level = 0.95)["log(income)", ]

# Function to calculate elasticity and its 95% CI
get_elasticity <- function(income, beta1, beta2, beta2_CI) {
  ln_y_hat <- beta1 + beta2 * log(income)
  y_hat <- exp(ln_y_hat)
  elasticity <- beta2 
  elasticity_lower <- beta2_CI[1] 
  elasticity_upper <- beta2_CI[2]
  return(data.frame(
    Income = income,
    Fitted_Food = y_hat,
    Elasticity = elasticity,
    Lower_CI = elasticity_lower,
    Upper_CI = elasticity_upper
  ))
}
elasticity_results <- do.call(rbind, lapply(income_vals, get_elasticity, beta1, beta2, beta2_CI))
print(elasticity_results)

residuals <- resid(model_loglog)
log_income <- log(cex5_small$income)
# Residuals vs INCOME plot
plot(log_income, residuals,
     main = "Residuals vs Income",
     xlab = "log(income) ($)", ylab = "Residuals",
     pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Histogram
hist(residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightgray")
abline(v = mean(residuals), col = "blue", lty = 2)
abline(v = median(residuals), col = "purple", lty = 2)
legend("topright", legend = c("Mean", "Median"), col = c("blue", "purple"), lty = 2)

# Jarque-Bera test
library(tseries)
jarque.bera.test(residuals)







log_data <- subset(cex5_small,income > 0)
model_linearlog <- lm(food ~ log(income), data = log_data)

# Summary
summary(model_linearlog)

plot(log(log_data$income), log_data$food,
     main = "Log-Log: FOOD vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "FOOD",
     pch = 16, col = "blue")

abline(model_linearlog, col = "red", lwd = 2)

data.frame(
  Model = c("Linear", "Log-Log", "Linear-Log"),
  R_squared = c(summary(model_food_income)$r.squared,
                summary(model_loglog)$r.squared,
                summary(model_linearlog)$r.squared
  )
)

summary(model_linearlog)
# confint(model_linearlog, level = 0.95)
beta1 <- coef(model_linearlog)["(Intercept)"]
beta2 <- coef(model_linearlog)["log(income)"]

# Confidence interval for β2
beta2_CI <- confint(model_linearlog, level = 0.95)["log(income)", ]
beta2_CI
lr <- model_linearlog 
alpha = 0.05
df = df.residual(lr)
tc = qt(1-alpha/2, df)
tc

se_beta2 <- summary(model_linearlog)$coefficients["log(income)", "Std. Error"]

# Function to calculate elasticity and its 95% CI
get_elasticity <- function(income, beta1, beta2, beta2_CI) {
  y_hat <- beta1 + beta2 * log(income)
  elasticity_se_i <- se_beta2 / y_hat
  elasticity <- beta2/y_hat 
  elasticity_lower <- elasticity - tc*elasticity_se_i
  elasticity_upper <- elasticity + tc*elasticity_se_i
  return(data.frame(
    Income = income,
    Fitted_Food = y_hat,
    Elasticity = elasticity,
    Lower_CI = elasticity_lower,
    Upper_CI = elasticity_upper
  ))
}
elasticity_results <- do.call(rbind, lapply(income_vals, get_elasticity, beta1, beta2, beta2_CI))
print(elasticity_results)

residuals <- resid(model_linearlog)
log_income <- log(cex5_small$income)
# Residuals vs INCOME plot
plot(log_income, residuals,
     main = "Residuals vs Income",
     xlab = "log(income) ($)", ylab = "Residuals",
     pch = 16, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2, lty = 2)

# Histogram
hist(residuals, breaks = 30, main = "Histogram of Residuals",
     xlab = "Residuals", col = "lightgray")
abline(v = mean(residuals), col = "blue", lty = 2)
abline(v = median(residuals), col = "purple", lty = 2)
legend("topright", legend = c("Mean", "Median"), col = c("blue", "purple"), lty = 2)

# Jarque-Bera test
library(tseries)
jarque.bera.test(residuals)

