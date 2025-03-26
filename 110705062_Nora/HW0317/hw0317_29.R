url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5.rdata"
file_path <- "cex5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cex5)

## 4.29.1
library(tseries)

# Step 1
vars <- c("food", "income")
for (var in vars) {
  cat("\nSummary for", var, ":\n")
  x <- cex5[[var]]
  cat("Mean:   ", mean(x, na.rm = TRUE), "\n")
  cat("Median: ", median(x, na.rm = TRUE), "\n")
  cat("Min:    ", min(x, na.rm = TRUE), "\n")
  cat("Max:    ", max(x, na.rm = TRUE), "\n")
  cat("SD:     ", sd(x, na.rm = TRUE), "\n")
}

# Step 2
par(mfrow = c(1, 2))
for (var in vars) {
  x <- cex5[[var]]
  hist(x, breaks = 30, col = "lightblue", main = paste("Histogram of", var),
       xlab = var)
  abline(v = mean(x, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
  abline(v = median(x, na.rm = TRUE), col = "blue", lwd = 2, lty = 3)
  legend("topright", legend = c("Mean", "Median"),
         col = c("red", "blue"), lty = c(2, 3), cex = 0.8)
}
par(mfrow = c(1, 1))

# Step 3
for (var in vars) {
  x <- cex5[[var]]
  cat("\nJarque-Bera Normality Test for", var, ":\n")
  print(jarque.bera.test(x))
}

## 4.29.2
model_linear <- lm(food ~ income, data = cex5_small)

plot(cex5_small$income, cex5_small$food,
     main = "food vs. income with Fitted Line",
     xlab = "income", ylab = "food",
     pch = 16, col = "lightblue")
abline(model_linear, col = "blue", lwd = 2)

confint_beta2 <- confint(model_linear, level = 0.95)["income", ]
summary_model <- summary(model_linear)
summary(model_linear)
beta2_estimate <- summary_model$coefficients["income", "Estimate"]
beta2_se <- summary_model$coefficients["income", "Std. Error"]
cat("Estimated β2 (slope):", round(beta2_estimate, 6), "\n")
cat("Standard Error:", round(beta2_se, 6), "\n")
cat("95% Confidence Interval for β2:\n")
print(round(confint_beta2, 6))

## 4.29.3
residuals_lm <- resid(model_linear)
income_vals <- cex5_small$income

plot(income_vals, residuals_lm,
     main = "Residuals vs. income",
     xlab = "income", ylab = "Residuals",
     pch = 16, col = "lightblue")
abline(h = 0, col = "red", lwd = 2)

hist(residuals_lm, breaks = 50, col = "lightblue",
     main = "Histogram of Residuals", xlab = "Residuals", freq = FALSE)
lines(density(residuals_lm), col = "darkblue", lwd = 2)

if (!require(tseries)) install.packages("tseries")
library(tseries)
cat("\nJarque-Bera Normality Test for residuals:\n")
jb_result <- jarque.bera.test(residuals_lm)
print(jb_result)

## 4.29.4
beta2 <- coef(model_linear)["income"]
se_beta2 <- summary(model_linear)$coefficients["income", "Std. Error"]
confint_beta2 <- confint(model_linear)["income", ]  # 95% CI for beta2
income_values <- c(19, 65, 160)
results <- data.frame(
  income = income_values,
  Fitted_food = predict(model_linear, newdata = data.frame(income = income_values)),
  Elasticity = NA,
  Elasticity_Lower = NA,
  Elasticity_Upper = NA
)

for (i in 1:nrow(results)) {
  y_hat <- results$Fitted_food[i]
  x <- results$income[i]
  elasticity <- beta2 * x / y_hat
  elasticity_lower <- confint_beta2[1] * x / y_hat
  elasticity_upper <- confint_beta2[2] * x / y_hat
  
  results$Elasticity[i] <- elasticity
  results$Elasticity_Lower[i] <- elasticity_lower
  results$Elasticity_Upper[i] <- elasticity_upper
}
print(round(results, 4))

## 4.29.5
cex5_small$ln_food <- log(cex5_small$food)
cex5_small$ln_income <- log(cex5_small$income)
model_loglog <- lm(ln_food ~ ln_income, data = cex5_small)

plot(cex5_small$ln_income, cex5_small$ln_food,
     main = "log(food) vs. log(income) with Fitted Line",
     xlab = "log(income)", ylab = "log(food)",
     pch = 16, col = "lightblue")
abline(model_loglog, col = "red", lwd = 2)
summary(model_loglog)
summary_loglog <- summary(model_loglog)
r2_loglog <- summary_loglog$r.squared
r2_linear <- summary(model_linear)$r.squared
cat("R² of linear model:    ", round(r2_linear, 4), "\n")
cat("R² of log-log model:   ", round(r2_loglog, 4), "\n")

n <- nrow(cex5_small)
rss_null <- sum((cex5_small$ln_food - mean(cex5_small$ln_food))^2)
rss_model <- sum(resid(model_loglog)^2)
R2_generalized <- 1 - exp(-(rss_null - rss_model)/rss_null)
cat("Generalized R² for log-log model: ", round(R2_generalized, 4), "\n")

## 4.29.6
coef_loglog <- summary(model_loglog)$coefficients
elasticity_loglog <- coef_loglog["log(income)", "Estimate"]
se_loglog <- coef_loglog["log(income)", "Std. Error"]

z <- 1.96
lower_loglog <- elasticity_loglog - z * se_loglog
upper_loglog <- elasticity_loglog + z * se_loglog

cat("Log-log model elasticity estimate:\n")
cat("Point estimate:", round(elasticity_loglog, 4), "\n")
cat("95% CI: [", round(lower_loglog, 4), ",", round(upper_loglog, 4), "]\n\n")

model_linear <- lm(food ~ income, data = cex5_small)
beta2 <- coef(model_linear)["income"]
confint_linear <- confint(model_linear, level = 0.95)["income", ]
income_vals <- c(19, 65, 160)
fitted_vals <- predict(model_linear, newdata = data.frame(income = income_vals))
elasticity_linear <- beta2 * income_vals / fitted_vals
elasticity_linear_lower <- confint_linear[1] * income_vals / fitted_vals
elasticity_linear_upper <- confint_linear[2] * income_vals / fitted_vals

comparison <- data.frame(
  income = income_vals,
  Fitted_food = round(fitted_vals, 2),
  Linear_Elasticity = round(elasticity_linear, 4),
  Linear_Lower = round(elasticity_linear_lower, 4),
  Linear_Upper = round(elasticity_linear_upper, 4),
  LogLog_Elasticity = round(elasticity_loglog, 4),
  LogLog_Lower = round(lower_loglog, 4),
  LogLog_Upper = round(upper_loglog, 4)
)

cat("Comparison of Elasticity Estimates:\n")
print(comparison)

## 4.29.7
resid_loglog <- resid(model_loglog)
ln_income <- log(cex5_small$income)

plot(ln_income, resid_loglog,
     main = "Residuals vs. log(income)",
     xlab = "log(income)", ylab = "Residuals",
     pch = 16, col = "skyblue")
abline(h = 0, col = "red", lwd = 2)

hist(resid_loglog, breaks = 50, col = "lightblue",
     main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals", freq = FALSE)
lines(density(resid_loglog), col = "blue", lwd = 2)

if (!require("tseries")) install.packages("tseries")
library(tseries)
cat("\nJarque-Bera Normality Test for residuals from log-log model:\n")
jb_test <- jarque.bera.test(resid_loglog)
print(jb_test)

## 4.29.8
model_linlog <- lm(food ~ log(income), data = cex5_small)
summary(model_linlog )
plot(log(cex5_small$income), cex5_small$food,
     main = "food vs. log(income) with Fitted Line",
     xlab = "log(income)", ylab = "food",
     pch = 16, col = "skyblue")
abline(model_linlog, col = "red", lwd = 2)

r2_linear   <- summary(model_linear)$r.squared       # from part (b)
r2_loglog   <- summary(model_loglog)$r.squared       # from part (e)
r2_linlog   <- summary(model_linlog)$r.squared       # this model

cat("\nR² Comparison Across Models:\n")
cat("Linear (food ~ income):       ", round(r2_linear, 4), "\n")
cat("Log-log (log(food) ~ log(income)): ", round(r2_loglog, 4), "\n")
cat("Linear-log (food ~ log(income)):   ", round(r2_linlog, 4), "\n")

## 4.29.9
model_linlog <- lm(food ~ log(income), data = cex5_small)
alpha2 <- coef(model_linlog)["log(income)"]
se_alpha2 <- summary(model_linlog)$coefficients["log(income)", "Std. Error"]
z <- 1.96
alpha2_lower <- alpha2 - z * se_alpha2
alpha2_upper <- alpha2 + z * se_alpha2
income_vals <- c(19, 65, 160)
fitted_food <- predict(model_linlog, newdata = data.frame(income = income_vals))

elasticity <- alpha2 * income_vals / fitted_food
elasticity_lower <- alpha2_lower * income_vals / fitted_food
elasticity_upper <- alpha2_upper * income_vals / fitted_food

results <- data.frame(
  income = income_vals,
  Fitted_food = round(fitted_food, 2),
  Elasticity = round(elasticity, 4),
  Lower_95CI = round(elasticity_lower, 4),
  Upper_95CI = round(elasticity_upper, 4)
)

cat("Elasticity Estimates for the Linear-Log Model:\n")
print(results)

## 4.29.10
resid_linlog <- resid(model_linlog)
ln_income <- log(cex5_small$income)

plot(ln_income, resid_linlog,
     main = "Residuals vs. log(income)",
     xlab = "log(income)", ylab = "Residuals",
     pch = 16, col = "skyblue")
abline(h = 0, col = "red", lwd = 2)

hist(resid_linlog, breaks = 50, col = "lightblue",
     main = "Histogram of Residuals (Linear-Log Model)",
     xlab = "Residuals", freq = FALSE)
lines(density(resid_linlog), col = "blue", lwd = 2)

if (!require("tseries")) install.packages("tseries")
library(tseries)

cat("\nJarque-Bera Normality Test for Residuals:\n")
jb_result <- jarque.bera.test(resid_linlog)
print(jb_result)




