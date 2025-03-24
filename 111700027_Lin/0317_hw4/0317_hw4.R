#4.4
#(a)
install.packages(ggplot2)
library(ggplot2)

exper = seq(0, 30, by = 1)

rating_model1 = 64.289 + 0.990 * exper

ggplot(data = data.frame(exper, rating_model1), aes(x = exper, y = rating_model1)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Model 1: RATING vs. EXPERIENCE",
       x = "EXPER (Years of Experience)", y = "RATING") +
  theme_minimal()

#(b)
exper2 = seq(1, 30, by = 1)

rating_model2 = 39.464 + 15.312 * log(exper2)

ggplot(data = data.frame(exper2, rating_model2), aes(x = exper2, y = rating_model2)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Model 2: RATING vs. EXPERIENCE",
       x = "EXPER (Years of Experience)", y = "RATING") +
  theme_minimal()

#4.28
install.packages(POE5Rdata)
library(POE5Rdata)

data(wa_wheat)

names(wa_wheat)

northampton = data.frame(time = wa_wheat$time, yield = wa_wheat$northampton)

#(a)
model1 = lm(northampton ~ time, data=wa_wheat)
model2 = lm(northampton ~ log(time), data=wa_wheat)
model3 = lm(northampton ~ I(time^2), data=wa_wheat)
model4 = lm(log(northampton) ~ time, data=wa_wheat)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

par(mfrow=c(2,2))
plot(wa_wheat$time, wa_wheat$northampton, main="Model 1: YIELD ~ TIME",
     xlab="Time", ylab="Yield", pch=16, col="blue")
abline(model1, col="red", lwd=2)  

plot(wa_wheat$time, wa_wheat$northampton, main="Model 2: YIELD ~ log(TIME)",
     xlab="Time", ylab="Yield", pch=16, col="blue")
lines(wa_wheat$time, fitted(model2), col="red", lwd=2)  

plot(wa_wheat$time, wa_wheat$northampton, main="Model 3: YIELD ~ TIME^2",
     xlab="Time", ylab="Yield", pch=16, col="blue")
lines(wa_wheat$time, fitted(model3), col="red", lwd=2)  

plot(wa_wheat$time, log(wa_wheat$northampton), main="Model 4: log(YIELD) ~ TIME",
     xlab="Time", ylab="Log(Yield)", pch=16, col="blue")
abline(model4, col="red", lwd=2) 

shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)

par(mfrow=c(2,2))  

plot(model1$residuals, main="Residuals: Model 1",
     xlab="Index", ylab="Residuals", pch=1)
abline(h=0, col="black", lty=2)  
lines(lowess(model1$residuals), col="red", lwd=2)  

plot(model2$residuals, main="Residuals: Model 2",
     xlab="Index", ylab="Residuals", pch=1)
abline(h=0, col="black", lty=2)
lines(lowess(model2$residuals), col="red", lwd=2)

plot(model3$residuals, main="Residuals: Model 3",
     xlab="Index", ylab="Residuals", pch=1)
abline(h=0, col="black", lty=2)
lines(lowess(model3$residuals), col="red", lwd=2)

plot(model4$residuals, main="Residuals: Model 4",
     xlab="Index", ylab="Residuals", pch=1)
abline(h=0, col="black", lty=2)
lines(lowess(model4$residuals), col="red", lwd=2)

r_squared = c(summary(model1)$r.squared,
               summary(model2)$r.squared,
               summary(model3)$r.squared,
               summary(model4)$r.squared)
names(r_squared) = c("Linear", "Log(TIME)", "TIME^2", "Log(YIELD)")
r_squared

#(b)
summary(model3)

#(c)
install.packages(car)
install.packages("carData")
library(car) 
library(carData)

model_best = lm(yield ~ poly(time, 2), data = northampton)

residuals_stud = rstudent(model_best)   
leverage_vals = hatvalues(model_best)   
dfbetas_vals = dfbetas(model_best)      
dffits_vals  = dffits(model_best)      

threshold_resid = 2  
threshold_leverage = 2 * mean(leverage_vals)  
threshold_dffits = 2 * sqrt(2 / nrow(northampton))  

outliers = which(abs(residuals_stud) > threshold_resid |
                    leverage_vals > threshold_leverage |
                    abs(dffits_vals) > threshold_dffits)

northampton[outliers, ]


#(d)
train_data = subset(wa_wheat, time <= 47)

model3_train = lm(northampton ~ I(time^2), data = train_data)

new_data = data.frame(time = 48)

prediction = predict(model3_train,
                      newdata = new_data,
                      interval = "prediction",
                      level = 0.95)

actual_1997 = wa_wheat$northampton[wa_wheat$time == 48]

cat("95% Prediction Interval for northampton yield in 1997 (time = 48):\n")
print(round(prediction, 4))

if (length(actual_1997) == 0) {
  cat("\n沒有 1997 年的實際產量數據。\n")
} else {
  cat("\nActual northampton yield in 1997:", actual_1997, "\n")
  
  # 確認實際產量是否在 95% 預測區間內
  if (actual_1997 >= prediction[1, "lwr"] && actual_1997 <= prediction[1, "upr"]) {
    cat("\nThe actual yield is within the 95% prediction interval.\n")
  } else {
    cat("\nThe actual yield is not within the 95% prediction interval.\n")
  }
}

#4.29
library(POE5Rdata)
data("cex5_small")

install.packages("tseries")
library(tseries)

#(a)
food_stats = c(mean = mean(cex5_small$food), 
                median = median(cex5_small$food), 
                min = min(cex5_small$food), 
                max = max(cex5_small$food), 
                sd = sd(cex5_small$food))

income_stats = c(mean = mean(cex5_small$income), 
                  median = median(cex5_small$income), 
                  min = min(cex5_small$income), 
                  max = max(cex5_small$income), 
                  sd = sd(cex5_small$income))

cat("Food Summary:\n")
cat("Mean: ", food_stats["mean"], "\n")
cat("Median: ", food_stats["median"], "\n")
cat("Min: ", food_stats["min"], "\n")
cat("Max: ", food_stats["max"], "\n")
cat("Standard Deviation: ", food_stats["sd"], "\n\n")

cat("Income Summary:\n")
cat("Mean: ", income_stats["mean"], "\n")
cat("Median: ", income_stats["median"], "\n")
cat("Min: ", income_stats["min"], "\n")
cat("Max: ", income_stats["max"], "\n")
cat("Standard Deviation: ", income_stats["sd"], "\n\n")


par(mfrow = c(1, 2))  # Set up 1x2 layout
hist(cex5_small$food, main = "Food Histogram", xlab = "Food", col = "red", border = "black")
abline(v = food_mean, col = "pink", lwd = 3, lty = 2.5)
abline(v = food_median, col = "orange", lwd = 3, lty = 2.5)
hist(cex5_small$income, main = "Income Histogram", xlab = "Income", col = "blue", border = "black")
abline(v = income_mean, col = "pink", lwd = 3, lty = 2.5)
abline(v = income_median, col = "orange", lwd = 3, lty = 2.5)

print(jarque.bera.test(cex5_small$food))
print(jarque.bera.test(cex5_small$income))

#(b)
linear_model = lm(food ~ income, data = cex5_small)
summary(linear_model)

plot(cex5_small$income, cex5_small$food,
     main = "Food vs Income (Linear Model)",
     xlab = "Income",
     ylab = "Food",
     pch = 16,
     col = "blue")
abline(linear_model, col = "red", lwd = 2)

confint(linear_model, level = 0.95)

#(c)
linear_residuals = resid(linear_model)

plot(cex5_small$income, linear_residuals,
     main = "Residuals vs Income (Linear Model)",
     xlab = "Income",
     ylab = "Residuals",
     pch = 16,
     col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2)

hist(linear_residuals,
     main = "Histogram of Residuals (Linear Model)",
     xlab = "Residuals",
     col = "blue",
     border = "black")
abline(v = 0, col = "red", lwd = 2, lty = 2)

print(jarque.bera.test(linear_residuals))

#(d)
income_vals = c(19, 65, 160)
pred_food = predict(linear_model, newdata = data.frame(income = income_vals))
pred_food

beta2 = coef(linear_model)[2]
elasticity = beta2 * (income_vals / pred_food)
elasticity

confint_beta2 = confint(linear_model)["income", ]
elasticity_lower = confint_beta2[1] * (income_vals / pred_food)
elasticity_upper = confint_beta2[2] * (income_vals / pred_food)

elasticity_df = data.frame(
  income = income_vals,
  food_predicted = round(pred_food, 2),
  elasticity = round(elasticity, 4),
  lower_95_ci = round(elasticity_lower, 4),
  upper_95_ci = round(elasticity_upper, 4)
)
elasticity_df

#(e)
cex5_small$log_food = log(cex5_small$food)
cex5_small$log_income = log(cex5_small$income)
loglog_model = lm(log_food ~ log_income, data = cex5_small)
summary(loglog_model)

plot(cex5_small$log_income, cex5_small$log_food,
     main = "log(Food) vs log(Income) (Log-Log Model)",
     xlab = "log(Income)",
     ylab = "log(Food)",
     pch = 16,
     col = "purple")
abline(loglog_model, col = "orange", lwd = 2)

r2_loglog = summary(loglog_model)$r.squared
r2_linear = summary(linear_model)$r.squared

cat("R-squared of Linear Model: ", r2_linear, "\n")
cat("R-squared of Log-Log Model: ", r2_loglog, "\n")

#(f)
confint(loglog_model, level = 0.95)

#(g)
loglog_residuals = resid(loglog_model)

par(mfrow = c(1, 2))
plot(cex5_small$log_income, loglog_residuals,
     main = "Residuals vs log(Income) (Log-Log Model)",
     xlab = "log(Income)",
     ylab = "Residuals",
     pch = 16, col = "purple")
abline(h = 0, col = "orange", lwd = 2)

hist(loglog_residuals,
     main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals",
     col = "purple",
     breaks = 20)

print(jarque.bera.test(loglog_residuals))

#(h)
cex5_small$log_income = log(cex5_small$income)
linear_log_model = lm(food ~ log_income, data = cex5_small)
summary(linear_log_model)

plot(cex5_small$log_income, cex5_small$food,
     main = "Food vs log(Income) (Linear Model)",
     xlab = "log(Income)",
     ylab = "Food",
     pch = 16,
     col = "red")
abline(linear_log_model, col = "blue", lwd = 2)

r2_linear_log = summary(linear_log_model)$r.squared
cat("R-squared of Linear Model with log(Income): ", r2_linear_log, "\n")
cat("R-squared of Log-Log Model: ", r2_loglog, "\n")

#(i)
income_vals_log = c(19, 65, 160)
log_income_vals = log(income_vals_log)
pred_logfood = predict(linear_log_model, newdata = data.frame(log_income = log_income_vals))
pred_logfood

logbeta2 = coef(linear_log_model)[2]
logelasticity = logbeta2 / pred_logfood
logelasticity

confint_logbeta2 = confint(linear_log_model)["log_income", ]
elasticity_lower_log = confint_logbeta2[1] / pred_logfood
elasticity_upper_log = confint_logbeta2[2] / pred_logfood

logelasticity_df = data.frame(
  income = income_vals_log,
  food_predicted = round(pred_logfood, 2),
  elasticity = round(logelasticity, 4),
  lower_95_ci = round(elasticity_lower_log, 4),
  upper_95_ci = round(elasticity_upper_log, 4)
)
logelasticity_df

#(j)
residuals_linearlog = resid(linear_log_model)

par(mfrow = c(1, 2))
plot(cex5_small$log_income, residuals_linearlog,
     main = "Residuals vs log(Income) (Linear-Log Model)",
     xlab = "log(Income)",
     ylab = "Residuals",
     pch = 16, col = "red")
abline(h = 0, col = "blue", lwd = 2)

hist(residuals_linearlog,
     main = "Histogram of Residuals (Linear-Log Model)",
     xlab = "Residuals",
     col = "red",
     breaks = 20)

print(jarque.bera.test(residuals_linearlog))
