library(POE5Rdata)
##4.4(a)
exper <- seq(0, 30, by = 1)

fitted_values <- 64.289 + 0.990 * exper

plot(exper, fitted_values, type = "l", col = "blue", lwd = 2,
     xlab = "Years of Experience (EXPER)", ylab = "Predicted Rating",
     main = "Fitted Values from Model 1")
grid()

##(b)
exper <- seq(1, 30, by = 1)

fitted_values <- 39.464 + 15.312 * log(exper)

plot(exper, fitted_values, type = "l", col = "red", lwd = 2,
     xlab = "Years of Experience (EXPER)", ylab = "Predicted Rating",
     main = "Fitted Values from Model 2 (Log-Linear)")
grid()
## We do not use the 4 artists with exper = 0, since log(0) is not defined

##(c)
beta2 <- 0.990 

cat("Marginal effect at 10 years of experience:", beta2, "\n")
cat("Marginal effect at 20 years of experience:", beta2, "\n")

##(d)
alpha2 <- 15.312  

exper_10 <- alpha2 / 10
exper_20 <- alpha2 / 20

cat("Marginal effect at 10 years of experience:", exper_10, "\n")
cat("Marginal effect at 20 years of experience:", exper_20, "\n")

##(e)
##Model2 fits better since it has greater goodness of fit.

##(f)
##Model2 is more plausible since it can explain the diminishing effect of experiences.

##4.28(a)
data("wa_wheat")
library(ggplot2)
library(car)

model1 <- lm(northampton ~ time, data = wa_wheat)                 
model2 <- lm(northampton ~ log(time), data = wa_wheat)           
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)            
model4 <- lm(log(northampton) ~ time, data = wa_wheat)   

summary(model1)
summary(model2)
summary(model3)
summary(model4)

par(mfrow = c(2,2)) 
plot(model1$residuals, main="Residuals of Model 1")
plot(model2$residuals, main="Residuals of Model 2")
plot(model3$residuals, main="Residuals of Model 3")
plot(model4$residuals, main="Residuals of Model 4")


shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)

r_squared <- data.frame(
  Model = c("Linear", "Log-Linear", "Quadratic", "Log-Log"),
  R2 = c(summary(model1)$r.squared, 
         summary(model2)$r.squared, 
         summary(model3)$r.squared, 
         summary(model4)$r.squared)
)

print(r_squared)

ggplot(wa_wheat, aes(x=time, y=northampton)) +
  geom_point() +
  geom_line(aes(y=predict(model1)), color="blue", linetype="dashed") +  
  geom_line(aes(y=predict(model2)), color="red", linetype="dotted") +  
  geom_line(aes(y=predict(model3)), color="green", linetype="dotdash") +  
  geom_line(aes(y=exp(predict(model4))), color="purple") +  
  labs(title="Fitted Models", y="YIELD", x="TIME") +
  theme_minimal()
## We prefer the third model since it has the highest Rsquare and also the residuals don't deviate from normality.

##(b)
## In model3, as time increase by 1, then the yield increases by 4.986e-04*1^2

##(c)
library(car)

chosen_model <- model3 

influence_measures <- influence.measures(chosen_model)

leverage <- hatvalues(chosen_model)  
student_residuals <- rstudent(chosen_model)  
dfbetas_values <- dfbetas(chosen_model)  
dffits_values <- dffits(chosen_model)  

diagnostics <- data.frame(
  TIME = wa_wheat$time,
  YIELD = wa_wheat$northampton,
  Leverage = leverage,
  Studentized_Residuals = student_residuals,
  DFFITS = dffits_values
)

dfbetas_df <- as.data.frame(dfbetas_values)
colnames(dfbetas_df) <- paste0("DFBETA_", names(coef(chosen_model)))
diagnostics <- cbind(diagnostics, dfbetas_df)

high_leverage <- which(leverage > (2 * mean(leverage)))  
high_residuals <- which(abs(student_residuals) > 2)
high_dffits <- which(abs(dffits_values) > (2 * sqrt(ncol(model.matrix(chosen_model)) / nrow(wa_wheat))))  
high_dfbetas <- which(apply(abs(dfbetas_values) > 2 / sqrt(nrow(wa_wheat)), 1, any)) 

unusual_obs <- unique(c(high_leverage, high_residuals, high_dffits, high_dfbetas))

print(diagnostics[unusual_obs, ])

par(mfrow=c(2,2))
plot(leverage, main="Leverage", ylab="Leverage", xlab="Observation Index")
plot(student_residuals, main="Studentized Residuals", ylab="Residuals", xlab="Observation Index")
plot(dffits_values, main="DFFITS", ylab="DFFITS", xlab="Observation Index")
matplot(dfbetas_values, type="h", main="DFBETAS", ylab="DFBETAS", xlab="Observation Index")

##(d)
library(car)

wa_wheat_train <- subset(wa_wheat, time <= 47)
wa_wheat_test <- subset(wa_wheat,  time == 48)

model <- lm(northampton ~ time, data = wa_wheat_train)
pred_1997 <- predict(model, newdata = wa_wheat_test, interval = "prediction", level = 0.95)
print(pred_1997)

actual_yield_1997 <- wa_wheat_test$northampton
if (actual_yield_1997 >= pred_1997[,"lwr"] & actual_yield_1997 <= pred_1997[,"upr"]) {
  print("The true value is within the prediction interval.")
} else {
  print("The true value is NOT within the prediction interval.")
}

##4.29(a)
data("cex5_small")
library(ggplot2)
library(moments)

summary_stats <- function(var) {
  return(c(
    Mean = mean(var, na.rm = TRUE),
    Median = median(var, na.rm = TRUE),
    Min = min(var, na.rm = TRUE),
    Max = max(var, na.rm = TRUE),
    Std_Dev = sd(var, na.rm = TRUE)
  ))
}

food_stats <- summary_stats(cex5_small$food)
income_stats <- summary_stats(cex5_small$income)

food_stats
income_stats

ggplot(cex5_small, aes(x = food)) +
  geom_histogram(binwidth = 10 ,fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = mean(food, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(food, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
  ggtitle("Histogram of FOOD") +
  labs(x = "FOOD Expenditure", y = "Frequency")

ggplot(cex5_small, aes(x = income)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = mean(income, na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(income, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
  ggtitle("Histogram of INCOME") +
  labs(x = "Household Income", y = "Frequency")

jb_test_food <- jarque.test(cex5_small$food)
jb_test_income <- jarque.test(cex5_small$income)

jb_test_food
jb_test_income
## None of them looks like bell-shaped and symmetric, both of them have a larger mean, and also the Jarque_Bera test show that both of them deviate from normality.

##(b)
model_food <- lm(food ~ income, data = cex5_small)

summary(model_food)

conf_interval <- confint(model_food, level = 0.95)
conf_interval

ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # 
  ggtitle("Scatter Plot of FOOD vs. INCOME") +
  labs(x = "Household Income", y = "Food Expenditure") +
  theme_minimal()
## It is a not very precise estimate since it is a wide interval.

##(c)
library(tseries)
residuals_food <- resid(model_food)

ggplot(data = cex5_small, aes(x = income, y = residuals_food)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs. Income") +
  labs(x = "Household Income", y = "Residuals") +
  theme_minimal()
ggplot(data.frame(residuals_food), aes(x = residuals_food)) +
  geom_histogram(bins = 30, color = "black", fill = "lightblue", alpha = 0.7) +
  geom_vline(xintercept = mean(residuals_food), color = "red", linetype = "dashed") +
  ggtitle("Histogram of Residuals") +
  labs(x = "Residuals", y = "Frequency") +
  theme_minimal()

jarque_bera_test <- jarque.bera.test(residuals_food)
jarque_bera_test
## The residuals plot doesn't appear any patterns, it is more important for the error to be normally distributed, since this ensures that OLS estimator is unbiased.

##(d)
library(tidyverse)
library(sandwich)  
library(lmtest) 

model_linear <- lm(food ~ income, data = cex5_small)

beta_1 <- coef(model_linear)[1]
beta_2 <- coef(model_linear)[2]

income_values <- c(19, 65, 160)
predicted_food <- predict(model_linear, newdata = data.frame(income = income_values))

elasticity_estimates <- beta_2 * (income_values / predicted_food)


se_beta2 <- sqrt(diag(vcovHC(model_linear, type = "HC0")))[2]  
t_critical <- qt(0.975, df = nrow(cex5_small) - 2)

lower_bound <- (beta_2 - t_critical * se_beta2) * (income_values / predicted_food)
upper_bound <- (beta_2 + t_critical * se_beta2) * (income_values / predicted_food)

results <- data.frame(
  INCOME = income_values,
  Elasticity_Estimate = elasticity_estimates,
  CI_Lower = lower_bound,
  CI_Upper = upper_bound
)

print(results)
## The results are very different since it depends on income level, also as income increases the income elaticity should also increase.

##(e)
library(tidyverse)
library(ggplot2)
library(sandwich) 
library(lmtest)   

cex5_small <- cex5_small %>%
  mutate(log_FOOD = log(food),
         log_INCOME = log(income))

model_loglog <- lm(log_FOOD ~ log_INCOME, data = cex5_small)

summary(model_loglog)

r2_loglog <- summary(model_loglog)$r.squared

ggplot(cex5_small, aes(x = log_INCOME, y = log_FOOD)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Log-Log Model: ln(FOOD) vs. ln(INCOME)",
       x = "ln(INCOME)", y = "ln(FOOD)") +
  theme_minimal()

rss_loglog <- sum(residuals(model_loglog)^2)
tss_loglog <- sum((cex5_small$log_FOOD - mean(cex5_small$log_FOOD))^2)
n <- nrow(cex5_small)

r2_gen_loglog <- 1 - exp((rss_loglog/n) - log(tss_loglog))

r2_linear <- summary(model_linear)$r.squared

cat("Linear Model R²:", r2_linear, "\n")
cat("Log-Log Model Generalized R²:", r2_gen_loglog, "\n")
## Obviously, the log log model is more well-defined and seems that it fits the data better.

##(f)
coef_loglog <- summary(model_loglog)$coefficients

elasticity_estimate <- coef_loglog["log_INCOME", "Estimate"]
cat("Point estimate of elasticity:", elasticity_estimate, "\n")

se_loglog <- coef_loglog["log_INCOME", "Std. Error"]
df_loglog <- model_loglog$df.residual


t_critical <- qt(0.975, df_loglog)
lower_bound <- elasticity_estimate - t_critical * se_loglog
upper_bound <- elasticity_estimate + t_critical * se_loglog

cat("95% Confidence Interval for elasticity: (", lower_bound, ",", upper_bound, ")\n")
## They are not very similar since they do not totally overlap each other in a 95% confidence level.

##(g)
residuals_loglog <- resid(model_loglog)

plot(log(cex5_small$income), residuals_loglog, 
     xlab = "ln(INCOME)", ylab = "Residuals", 
     main = "Residuals vs. ln(INCOME)", pch = 19, col = "blue")
abline(h = 0, col = "red", lwd = 2, lty = 2) 

hist(residuals_loglog, breaks = 30, col = "lightblue", probability = TRUE,
     main = "Histogram of Residuals", xlab = "Residuals")

curve(dnorm(x, mean = mean(residuals_loglog), sd = sd(residuals_loglog)), 
      col = "red", lwd = 2, add = TRUE)
library(tseries)
jb_test <- jarque.bera.test(residuals_loglog)
print(jb_test)
## There doesn't seem like there exists a specific pattern but the JB test still shows that the residual deviates from normality.

##(h)
model_linlog <- lm(food ~ log(income), data = cex5_small)

summary(model_linlog)

plot(log(cex5_small$income), cex5_small$food, 
     xlab = "ln(INCOME)", ylab = "FOOD",
     main = "FOOD vs ln(INCOME)", col = "blue", pch = 19)


abline(model_linlog, col = "red", lwd = 2)

r2_linear <- summary(model_linear)$r.squared
r2_loglog <- summary(model_loglog)$r.squared
r2_linlog <- summary(model_linlog)$r.squared


cat("R² (Linear Model):", r2_linear, "\n")
cat("R² (Log-Log Model):", r2_loglog, "\n")
cat("R² (Linear-Log Model):", r2_linlog, "\n")
## It doesn't seem more well-defined compared to the log log model and also the log log model still fits better comparing the Rsquared values.

##(i)
income_values <- c(19, 65, 160)
predicted_food <- predict(model_linlog, newdata = data.frame(income = income_values))

alpha2_hat <- coef(model_linlog)["log(income)"]

elasticity_estimates <- alpha2_hat * (predicted_food / income_values)

data.frame(INCOME = income_values, Elasticity = elasticity_estimates)

se_alpha2 <- summary(model_linlog)$coefficients["log(income)", "Std. Error"]

alpha2_CI <- confint(model_linlog, "log(income)", level = 0.95)


lower_bound <- alpha2_CI[1] * (predicted_food / income_values)
upper_bound <- alpha2_CI[2] * (predicted_food / income_values)

data.frame(INCOME = income_values, 
           Elasticity = elasticity_estimates, 
           Lower_95_CI = lower_bound, 
           Upper_95_CI = upper_bound)

elasticity_loglog <- coef(model_loglog)["log(income)"]  

overlap_check <- (elasticity_loglog >= lower_bound) & (elasticity_loglog <= upper_bound)


data.frame(INCOME = income_values, 
           Elasticity_LinLog = elasticity_estimates, 
           Lower_95_CI_LinLog = lower_bound, 
           Upper_95_CI_LinLog = upper_bound,
           Elasticity_LogLog = elasticity_loglog,
           Overlap = overlap_check)
## It is still dissimilar since the 95% cinfidence intervals don't overlap each other.

##(j)
residuals_linlog <- residuals(model_linlog)

log_income <- log(cex5_small$income)

plot(log_income, residuals_linlog, 
     main = "Residuals vs. log(INCOME)", 
     xlab = "log(INCOME)", 
     ylab = "Residuals", 
     pch = 16, col = "blue")

hist(residuals_linlog, breaks = 30, col = "lightblue",
     main = "Histogram of Residuals", xlab = "Residuals", probability = TRUE)


curve(dnorm(x, mean = mean(residuals_linlog), sd = sd(residuals_linlog)), 
      col = "red", lwd = 2, add = TRUE)
library(tseries)  

jb_test_result <- jarque.bera.test(residuals_linlog)
print(jb_test_result)
## The plot seems very right skewed and the JB test also shows that the residuals deviate from normality.

##(k)
## I prefer the log log model the most, since its residuals look most likely similar to a normal distribution and also that it has the largest Rsquared value.

