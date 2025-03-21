#a
EXPER <- 0:30
RATING_1 <- 64.289 + 0.990 * EXPER

plot(EXPER, RATING_1, type = "l", col = "blue", lwd = 2,
     xlab = "Years of Experience", ylab = "Performance Rating",
     main = "Model 1")
grid()

#b
EXPER_2 <- 1:30
RATING_2 <- 39.464 + 15.312 * log(EXPER_2)

plot(EXPER_2, RATING_2, type = "l", col = "red", lwd = 2,
     xlab = "Years of Experience", ylab = "Performance Rating",
     main = "Model 2")
grid()

#d
EXPER_values <- c(10, 20)
marginal_effect_2 <- 15.312 / EXPER_values

cat("Marginal effect of Model 2 (EXPER = 10):", marginal_effect_2[1], "\n")
cat("Marginal effect of Model 2 (EXPER = 20):", marginal_effect_2[2], "\n")

#4.28
#a(i)
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
download.file(url, destfile = "wa_wheat.rdata", mode = "wb")  # 'mode = "wb"' ensures the file is downloaded in binary mode
load("wa_wheat.rdata")
file_info <- file.info("wa_wheat.rdata")
print(file_info)

yield <- wa_wheat$northampton
time <- wa_wheat$time

mod1 <- lm(yield ~ time, data = wa_wheat)
plot(time, yield, type = "p", pch = 16, col = "blue", lwd = 2,
     xlab = "Time", ylab = "Yield", main = "Linear Model")
lines(time, predict(mod1), col = "red", lwd = 2, lty = 1) 

mod2 <- lm(yield ~ log(time), data = wa_wheat)
plot(time, yield, type = "p", pch = 16, col = "blue", lwd = 2,
     xlab = "Time", ylab = "Yield", main = "Linear-Log Model")
lines(time, predict(mod2), col = "red", lwd = 2, lty = 1)   

mod3 <- lm(yield ~ I(time^2), data = wa_wheat)
plot(time, yield, type = "p", pch = 16, col = "blue", lwd = 2,
     xlab = "Time", ylab = "Yield", main = "Quadratic Model")
lines(time, predict(mod3), col = "red", lwd = 2, lty = 1)   

mod4 <- lm(log(yield) ~ time, data = wa_wheat)
plot(time, yield, type = "p", pch = 16, col = "blue", lwd = 2,
     xlab = "Time", ylab = "Yield", main = "Log-Linear Model")
lines(time, exp(fitted(mod4)), col = "red", lwd = 2, lty = 1)   

#(ii)
plot(mod1$residuals, main = "Linear", ylab = "Residuals", xlab = "Index")
plot(mod2$residuals, main = "Linear-Log", ylab = "Residuals", xlab = "Index")
plot(mod3$residuals, main = "Quadratic", ylab = "Residuals", xlab = "Index")
plot(mod4$residuals, main = "Log-Linear", ylab = "Residuals", xlab = "Index")

#(iii)
install.packages("tseries")  
library(tseries)

resid1 <- residuals(mod1)
resid2 <- residuals(mod2)
resid3 <- residuals(mod3)
resid4 <- residuals(mod4)

test1 <- jarque.bera.test(resid1)  
test2 <- jarque.bera.test(resid2)  
test3 <- jarque.bera.test(resid3)  
test4 <- jarque.bera.test(resid4)  

jb_values <- data.frame(
  Model = c("Linear", "Linear-Log", "Quadratic", "Log-Linear"),
  JB_p_value = c(test1$p.value, test2$p.value, 
                 test3$p.value, test4$p.value)
)
print(jb_values)

summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared

#b
summary(mod3)

#c
install.packages("car")  
library(car)

student_resid <- rstudent(mod3)
leverage <- hatvalues(mod3)
dfbetas_values <- dfbetas(mod3)
dffits_values <- dffits(mod3)
outliers <- which(abs(student_resid) > 2)

k <- length(coef(mod3)) - 1  
n <- length(resid(mod3))  
high_leverage <- which(leverage > (2 * (k + 1) / n))

influential_dfbetas <- which(apply(abs(dfbetas_values), 1, max) > 0.2)

influential_dffits <- which(abs(dffits_values) > 2 * sqrt(k / n))

cat("Studentized Residuals:", outliers, "\n")
cat("Leverage:", high_leverage, "\n")
cat("DFBETAS:", influential_dfbetas, "\n")
cat("DFFITS:", influential_dffits, "\n")

#d
data_1996 <- wa_wheat[1:47, ]
data_1996$time_squared <- data_1996$time^2
head(data_1996$time_squared)

mod_quad <- lm(data_1996$northampton ~ data_1996$time_squared, data = data_1996)
summary(mod_quad)

train <- wa_wheat[1:47, ]
YIELD <- wa_wheat$northampton
TIME <- wa_wheat$time

model_train <- lm(YIELD~ I(TIME^2), data = train)
newdata <- data.frame(TIME = 48)
(pred <- predict(model_train, newdata, interval = "prediction",
                 level = 0.95))

cat("Predicted YIELD for 1997:", pred[1], "\n")
cat("95% Prediction Interval: [", pred[2], ",", pred[3], "]\n")

true_value_1997 <- wa_wheat$northampton[wa_wheat$time == 48]
cat("True value of 1997:", true_value_1997, "\n")

train <- wa_wheat[1:47, ]
model_train <- lm(YIELD ~ I(TIME^2), data = train)
newdata <- data.frame(TIME = 48)
(pred <- predict(model_train, newdata, interval = "prediction",
                 level = 0.95))
(origin <- wa_wheat[48, 1])

#4.28
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("cex5_small")

mean_food = mean(cex5_small$food)
median_food = median(cex5_small$food)
min_food = min(cex5_small$food)
max_food = max(cex5_small$food)
sd_food = sd(cex5_small$food)
mean_income = mean(cex5_small$income)
median_income = median(cex5_small$income)
min_income = min(cex5_small$income)
max_income = max(cex5_small$income)
sd_income = sd(cex5_small$income)

summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Min", "Max", "SD"),
  Food = c(mean(cex5_small$food), median(cex5_small$food), 
           min(cex5_small$food), max(cex5_small$food), sd(cex5_small$food)),
  Income = c(mean(cex5_small$income), median(cex5_small$income), 
             min(cex5_small$income), max(cex5_small$income), sd(cex5_small$income))
)
print(summary_table)


library(ggplot2)
ggplot(cex5_small, aes(x = food)) + 
  geom_histogram(bins = 30, color = "blue", alpha = 0.5) + 
  geom_vline(xintercept = mean_food, color = "red", linetype = "dashed", linewidth = 1) + 
  geom_vline(xintercept = median_food, color = "green", linetype = "dashed", linewidth = 1) + 
  labs(x = "Food Expenditure") + 
  annotate("text", x = mean_food, y = max(table(cut(cex5_small$food, breaks = 30))) * 0.9, 
           label = "Mean", color = "red", angle = 90, vjust = -0.5) + 
  annotate("text", x = median_food, y = max(table(cut(cex5_small$food, breaks = 30))) * 0.8, 
           label = "Median", color = "green", angle = 90, vjust = -0.5) +
  theme_minimal()

ggplot(cex5_small, aes(x = income)) + 
  geom_histogram(bins = 30, color = "blue", alpha = 0.5) + 
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed", linewidth = 1) + 
  geom_vline(xintercept = median_income, color = "green", linetype = "dashed", linewidth = 1) + 
  labs(x = "Income") + 
  annotate("text", x = mean_income, y = max(table(cut(cex5_small$income, breaks = 30))) * 0.9, 
           label = "Mean", color = "red", angle = 90, vjust = -0.5) + 
  annotate("text", x = median_income, y = max(table(cut(cex5_small$income, breaks = 30))) * 0.8, 
           label = "Median", color = "green", angle = 90, vjust = -0.5) +
  theme_minimal()

jb_food <- jarque.bera.test(cex5_small$food)
jb_income <- jarque.bera.test(cex5_small$income)

print(jb_food)
print(jb_income)

#b
lm_model <- lm(food ~ income, data = cex5_small)
summary(lm_model)

ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

confint(lm_model, level = 0.95)

#c
cex5_small$residuals <- residuals(lm_model)

ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point() +
  theme_minimal()

ggplot(cex5_small, aes(x = residuals)) + geom_histogram(bins = 30, fill = "blue", alpha = 0.5)

jb_residuals <- jarque.bera.test(cex5_small$residuals)
print(jb_residuals)

#d
income_given <- data.frame(income = c(19, 65, 160))

predictions_linear <- predict(lm_model, newdata = income_given, interval = "confidence", level = 0.95)

beta2 <- coef(lm_model)["income"]  
beta2_se <- summary(lm_model)$coefficients["income", "Std. Error"]  
elasticity_point <- beta2 * income_given$income / predictions_linear[, "fit"]  
elasticity_se <- beta2_se * income_given$income / predictions_linear[, "fit"]  

z_value <- qnorm(0.975) 
elasticity_lower <- elasticity_point - z_value * elasticity_se
elasticity_upper <- elasticity_point + z_value * elasticity_se

elasticity_table <- data.frame(
  Income = income_given$income,
  Food_hat = predictions_linear[, "fit"],
  ε = elasticity_point,
  se_ε = elasticity_se,
  ε_lower_bound = elasticity_lower,
  ε_upper_bound = elasticity_upper
)
print(elasticity_table)

#e
mod_loglog <- lm(log(food) ~ log(income), data = cex5_small)

plot(log(cex5_small$income), log(cex5_small$food), pch = 16, col = "blue",
     xlab = "Log of Income", ylab = "Log of Food", main = "Log-Log Model")

r2_linear <- summary(lm_model)$r.squared
r2_loglog <- summary(mod_loglog)$r.squared
r2_generalized <- 1 - exp((log(1 - r2_loglog) * length(cex5_small$food)) / (length(cex5_small$food) - 1))
print(r2_linear)
print(r2_generalized)

#f
summary_mod <- summary(mod_loglog)
elasticity_estimate <- coef(summary_mod)[2, 1] 
se_elasticity <- coef(summary_mod)[2, 2] 
summary(mod_loglog)

loglog_residual <- residuals(mod_loglog)
t_value <- qt(1 - 0.05/2, df = 1198) 
ci_lower <- elasticity_estimate - t_value * se_elasticity
ci_upper <- elasticity_estimate + t_value * se_elasticity

print(ci_lower)
print(ci_upper)

elasticity_loglog <- 0.1863054
se_loglog <- (0.2432675 - 0.1293432) / (2 * 1.96)  

elasticity_linear <- c(0.07145038, 0.20838756, 0.39319883)  
se_linear <- c((0.09072601 - 0.05217475) / (2 * 1.96),
               (0.26460562 - 0.15216951) / (2 * 1.96),
               (0.49927462 - 0.28712305) / (2 * 1.96))  


for (i in 1:length(income_values)) {
  z_value <- (elasticity_loglog - elasticity_linear[i]) / sqrt(se_loglog^2 + se_linear[i]^2)
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  
  cat("\nIncome:", income_values[i], "\n")
  cat("Z-value:", z_value, "\n")
  cat("P-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("The two elasticities are significantly different (reject H0).\n")
  } else {
    cat("No significant difference between the two elasticities (fail to reject H0).\n")
  }
}

#g
residuals_loglog <- residuals(mod_loglog)
hist(residuals_loglog, 
     main = "Histogram of Residuals", breaks = 40,
     xlab = "Residuals", col = "lightblue", 
     border = "black", probability = TRUE)

jb_test <- jarque.bera.test(residuals_loglog)
print(jb_test)

#h
mod_linlog <- lm(food~log(cex5_small$income), data = cex5_small)
plot(log_income, cex5_small$food, pch = 16, col = "blue",
     xlab = 'Log(Income)',
     ylab = 'Food',
     main = 'Linear-Log Model')
lines(log_income, fitted(mod_linlog), col = "red", lwd = 2)
summary(mod_linlog)

r2_lin_log <- summary(mod_linlog)$r.squared
r2_log_log <- summary(mod_loglog)$r.squared 

print(r2_linear)
print(r2_lin_log)
print(r2_log_log)

#i

# Getting the model oefficients and their confidence intervals
beta0_hat3 <- coef(mod_linlog)[1]
beta1_hat3 <- coef(mod_linlog)[2]
beta1_CI3 <- confint(mod_linlog, level = 0.95)[2,]

# Define the income values for which we want to compute elasticity
income_values <- c(19, 65, 160)

# Function to compute elasticity and the confidence interval
compute_values <- function(income) {
  # Fitted food expenditure value for the given income
  Y_hat3 <- beta0_hat3 + beta1_hat3 * log(income)
  
  # Elasticity: beta1_hat3 / income * (income / Y_hat3)
  elasticity3 <- beta1_hat3 / income * (income / Y_hat3)  
  
  # 95% CI for elasticity
  elasticity_CI3 <- beta1_CI3 / income * (income / Y_hat3)  
  
  # Compute Standard Error (SE) from the confidence interval
  SE_elasticity <- (elasticity_CI3[2] - elasticity_CI3[1]) / (2 * 1.96)
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat3, 
                    Elasticity = elasticity3, 
                    Elasticity_Lower = elasticity_CI3[1], 
                    Elasticity_Upper = elasticity_CI3[2],
                    SE = SE_elasticity))  
}

# Compute the results for each income level
results <- do.call(rbind, lapply(income_values, compute_values))

# Print the results
print(results)

# If needed, you can compare the elasticities across income levels
num_incomes <- length(income_values)
for (i in 1:(num_incomes - 1)) {
  for (j in (i + 1):num_incomes) {
    
    # Extract elasticities and SE for both income levels
    E1 <- results$Elasticity[i]
    SE1 <- results$SE[i]
    
    E2 <- results$Elasticity[j]
    SE2 <- results$SE[j]
    
    # Compute Z-score
    z_value <- (E1 - E2) / sqrt(SE1^2 + SE2^2)
    
    # Compute p-value (two-tailed test)
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    # Print results for the comparison
    cat("\nComparing Elasticities: INCOME =", income_values[i], "vs", income_values[j], "\n")
    cat("P-value:", p_value, "\n")
    if (p_value < 0.05) {
      cat("The two elasticities are significantly different (reject H0).\n")
    } else {
      cat("No significant difference between the two elasticities (fail to reject H0).\n")
    }
  }
}

#j
residuals_lin_log <- residuals(mod_linlog)

plot(log(cex5_small$income), residuals_lin_log,
     xlab = "log(Income)", ylab = "Residuals",
     col = "blue", pch = 16)

abline(h = 0, col = "red", lwd = 2)

hist(residuals_lin_log, main = "Histogram of Residuals", xlab = "Residuals",
     col = "lightblue", border = "black", breaks = 40)

library(tseries)
jb_test <- jarque.bera.test(residuals_lin_log)
print(jb_test)

# Interpretation of the Jarque-Bera test
if (jb_test$p.value < 0.05) {
  cat("The residuals are not normally distributed (reject H0).\n")
} else {
  cat("The residuals are normally distributed (fail to reject H0).\n")
}