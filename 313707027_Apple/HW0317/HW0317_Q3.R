# 4.29
# (a)
data('cex5_small')
summary_stats <- function(x) {
  c(mean = mean(x), median = median(x), min = min(x), max = max(x), sd = sd(x))
}
summary_stats(cex5_small$food)
summary_stats(cex5_small$income)

par(mfrow = c(1, 2))
hist(cex5_small$food, main = "Histogram of FOOD", breaks = 60,
     xlab = "FOOD", col = "lightblue")
abline(v = mean(cex5_small$food), col = "red", lwd = 2)   
abline(v = median(cex5_small$food), col = "blue", lwd = 2) 
legend("topright", legend = c("Mean", "Median"),
       col = c("red", "blue"),lwd = 2,  bty = "n")

hist(cex5_small$income, main = "Histogram of INCOME", breaks = 60,
     xlab = "INCOME", col = "lightgreen")
abline(v = mean(cex5_small$income), col = "red", lwd = 2)  
abline(v = median(cex5_small$income), col = "blue", lwd = 2)
legend("topright", legend = c("Mean", "Median"),
       col = c("red", "blue"),lwd = 2,  bty = "n")

# Jarque-Bera 
library(tseries)  
jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

# (b)
model <- lm(food ~ income,data=cex5_small)
summary(model)

plot(cex5_small$income,cex5_small$food,
     pch = 16, cex = 0.5, col = "blue",
     main = "Scatter Plot with Fitted Line",
     xlab = "INCOME", ylab = "FOOD")
abline(model, col = "red", lwd = 2)

confint(model, level = 0.95)[2,]

# (c)
residuals <- residuals(model)
plot(cex5_small$income, residuals,
     pch = 16, cex = 0.5, col = "blue",
     main = "Residual Plot",
     xlab = "INCOME", ylab = "Residuals")
abline(h = 0, col = "red", lwd = 2)


hist(residuals, main = "Histogram of Residuals",breaks = 60,
     xlab = "Residuals", col = "lightblue")
abline(v = mean(residuals), col = "red", lwd = 2)
abline(v = median(residuals), col = "blue", lwd = 2)
legend("topright", legend = c("Mean", "Median"),
       col = c("red", "blue"),lwd = 2,  bty = "n")

jarque.bera.test(residuals)

# (d)
beta0_hat <- coef(model)[1]
beta1_hat <- coef(model)[2]
beta1_CI <- confint(model,level = 0.95)[2,]

compute_values <- function(income) {
  Y_hat <- beta0_hat + beta1_hat * income  
  elasticity <- beta1_hat * (income / Y_hat)  
  elasticity_CI <- beta1_CI * (income / Y_hat)  
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat, 
                    Elasticity = elasticity, 
                    Elasticity_Lower = elasticity_CI[1], 
                    Elasticity_Upper = elasticity_CI[2]))
}
income_values <- c(19, 65, 160)  # Income levels to test
(results <- do.call(rbind, lapply(income_values, compute_values)))

# (e)
cex5_small$lnfood <- log(cex5_small$food)
cex5_small$lnincome <- log(cex5_small$income)
log_log_model <- lm(lnfood~lnincome,data=cex5_small)
summary(log_log_model)

plot(cex5_small$lnincome,cex5_small$lnfood,
     pch=16,cex=0.5, col = "blue",
     xlab = "log(INCOME)",ylab = "log(FOOD)",
     main = "Log-Log Model")
lines(cex5_small$lnincome, fitted(log_log_model), col = "red", lwd = 2)

linear_R2 <- summary(model)$r.squared
loglog_R2 <- summary(log_log_model)$r.squared

# (f)
beta0_hat_log <- coef(log_log_model)[1]
beta1_hat_log <- coef(log_log_model)[2]
beta1_CI_loglog <- confint(log_log_model, level = 0.95)[2,]

elasticity_loglog <- beta1_hat_log
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
    cat("The two elasticities are significantly different, so we reject H₀.\n")
  } else {
    cat("No significant difference between the two elasticities, so we fail to reject H₀.\n")
  }
}

# (g)
loglog_residuals <- residuals(log_log_model)

plot(cex5_small$lnincome, loglog_residuals, 
     pch = 16, cex = 0.5, col ="blue",
     xlab = "log(INCOME)", ylab = "Residuals",
     main = "Residuals vs log(INCOME)")
abline(h = 0, col = "red", lwd = 2)

hist(loglog_residuals, breaks = 50, 
     xlab = "Residuals", col = "lightblue")

# Jarque-Bera 
jarque.bera.test(loglog_residuals)

# (h)
linear_log_model <- lm(food~lnincome,data=cex5_small)
summary(linear_log_model)

plot(cex5_small$lnincome,cex5_small$food, 
     pch = 16, cex = 0.5, col ="blue",
     xlab = "log(INCOME)", ylab = "FOOD", 
     main = "Linear-Log Model")
abline(linear_log_model, col = "red", lwd = 2)

summary(linear_log_model)$r.squared

#(i)
beta0_hat_linear_log <- coef(linear_log_model)[1]
beta1_hat_linear_log <- coef(linear_log_model)[2]
beta1_CI_linear_log <- confint(linear_log_model,level = 0.95)[2,]

compute_values <- function(income) {
  Y_hat_linear_log <- beta0_hat_linear_log + beta1_hat_linear_log * log(income)  
  elasticity_linear_log <- beta1_hat_linear_log / income * (income / Y_hat_linear_log)  
  elasticity_CI_linear_log <- beta1_CI_linear_log / income * (income / Y_hat_linear_log)  
  
  SE_elasticity <- (elasticity_CI_linear_log[2] - elasticity_CI_linear_log[1]) / (2 * 1.96)
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat_linear_log, 
                    Elasticity = elasticity_linear_log, 
                    Elasticity_Lower = elasticity_CI_linear_log[1], 
                    Elasticity_Upper = elasticity_CI_linear_log[2],
                    SE = SE_elasticity))  # Include SE
}

(results <- do.call(rbind, lapply(income_values, compute_values)))

num_incomes <- length(income_values)
for (i in 1:(num_incomes - 1)) {
  for (j in (i + 1):num_incomes) {
    
    E1 <- results$Elasticity[i]
    SE1 <- results$SE[i]
    
    E2 <- results$Elasticity[j]
    SE2 <- results$SE[j]
    
    z_value <- (E1 - E2) / sqrt(SE1^2 + SE2^2)
    
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    cat("\nComparing Elasticities: INCOME =", income_values[i], "vs", income_values[j], "\n")
    cat("P-value:", p_value, "\n")
    if (p_value < 0.05) {
      cat("The two elasticities are significantly different, so we reject H₀.\n")
    } else {
      cat("No significant difference between the two elasticities, so we fail to reject H₀.\n")
    }
  }
}

# (j)
plot(cex5_small$lnincome, resid(linear_log_model),
     pch = 16, cex = 0.5, col ="blue",
     xlab = "log(INCOME)", ylab = "Residuals", 
     main = 'Residual Plot')
abline(h = 0, col = "red", lwd = 2)

hist(resid(linear_log_model),breaks = 60,
     xlab = "Residuals", col = "lightblue")
abline(v = mean(residuals), col = "red", lwd = 2)
abline(v = median(residuals), col = "blue", lwd = 2)
legend("topright", legend = c("Mean", "Median"),
       col = c("red", "blue"),lwd = 2,  bty = "n")

jarque.bera.test(resid(linear_log_model))


