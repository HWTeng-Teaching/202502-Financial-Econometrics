install.packages("moments")
library(moments)
library(POE5Rdata)
data("cex5_small")
# (a)
summary_food <- summary(cex5_small$food)
summary_income <- summary(cex5_small$income)

print(summary_food)
print(summary_income)

sd_food <- sd(cex5_small$food)
sd_income <- sd(cex5_small$income)
print(sd_food)
print(sd_income)

hist(cex5_small$food, main = "food", xlab = "food", col = "blue")
abline(v = mean(cex5_small$food), col = "red", lwd = 2)
abline(v = median(cex5_small$food), col = "green", lwd = 2)

hist(cex5_small$income, main = "income", xlab = "income", col = "blue")
abline(v = mean(cex5_small$income), col = "red", lwd = 2)
abline(v = median(cex5_small$income), col = "green", lwd = 2)

jb_food <- jarque.test(cex5_small$food)
print(jb_food)
jb_income <- jarque.test(cex5_small$income)
print(jb_income)

# (b)
linear_model <- lm(food ~ income, data = cex5_small)
summary(linear_model)

plot(cex5_small$income, cex5_small$food, main = "scatter plot food & income", xlab = "income", ylab = "food")
abline(linear_model, col = "red")

confint(linear_model, level = 0.95)

# (c)
residuals_linear <- residuals(linear_model)

plot(cex5_small$income, residuals_linear, main = "殘差與 income 的關係圖", xlab = "income", ylab = "殘差")
hist(residuals_linear, main = "residuals", xlab = "residuals", col = "blue")

jb_residuals_linear <- jarque.test(residuals_linear)
print(jb_residuals_linear())

# (d)
linear_model <- lm(food ~ income, data = cex5_small)

beta2 <- coef(linear_model)[2]

income_values <- c(19, 65, 160)

# 計算每個收入值對應的food預測值
food_values <- predict(linear_model, newdata = data.frame(income = income_values))

elasticity_estimates <- beta2 * income_values / food_values

confint_beta2 <- confint(linear_model)[2,]
elasticity_lower <- confint_beta2[1] * income_values / food_values
elasticity_upper <- confint_beta2[2] * income_values / food_values

elasticity_estimates
elasticity_lower
elasticity_upper

# (e)
cex5_small$ln_food <- log(cex5_small$food)
cex5_small$ln_income <- log(cex5_small$income)
log_log_model <- lm(ln_food ~ ln_income, data = cex5_small)
summary(log_log_model)

plot(log(cex5_small$income), log(cex5_small$food), main = "log-log model：food vs. income", xlab = "log(income)", ylab = "log(food)")
abline(log_log_model, col = "red")

n <- nrow(cex5_small)
rss_null <- sum((cex5_small$ln_food - mean(cex5_small$ln_food))^2)
rss_model <- sum(resid(log_log_model)^2)
R2_generalized <- 1 - exp(-(rss_null - rss_model)/rss_null)
cat("Generalized R² for log-log model: ", round(R2_generalized, 4), "\n")

# (f)
gamma2 <- coef(log_log_model)[2]
gamma2
confint(log_log_model, level = 0.95)

# (g)
residuals_log_log <- residuals(log_log_model)

plot(log(cex5_small$income), residuals_log_log, main = "殘差與 log(income) 的關係圖", xlab = "log(income)", ylab = "殘差")

hist(residuals_log_log, main = "log-log model histogram", xlab = "殘差", col = "blue")

jb_residuals_log_log <- jarque.test(residuals_log_log)
jb_residuals_log_log

# (h)
linear_log_model <- lm(food ~ log(income), data = cex5_small)
summary(linear_log_model)

plot(log(cex5_small$income), cex5_small$food, main = "linear-log model：food vs. ln(income)", xlab = "income", ylab = "log(food)")
abline(linear_log_model, col = "red")

summary(linear_log_model)$r.squared

# (i)
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
print(results)

# (j)
residuals <- residuals(linear_log_model)

plot(log(cex5_small$income), residuals, main="Residuals vs. ln(INCOME)", 
     xlab="ln(INCOME)", ylab="Residuals", pch=20, col="blue")
abline(h = 0, col = "red", lty = 2)

hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
     col="lightblue", border="black", breaks=20)

library(tseries)
jb_test <- jarque.test(residuals)
jb_test