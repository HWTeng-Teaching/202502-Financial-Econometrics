library(POE5Rdata)
library(ggplot2)
data("wa_wheat")

mod1 <- lm(northampton ~ time, data = wa_wheat)                 
mod2 <- lm(northampton ~ log(time), data = wa_wheat)           
mod3 <- lm(northampton ~ I(time^2), data = wa_wheat)          
mod4 <- lm(log(northampton) ~ time, data = wa_wheat)        

new_data <- data.frame(time = seq(min(wa_wheat$time), max(wa_wheat$time), length.out = 100))
new_data$pred1 <- predict(mod1, newdata = new_data) 
new_data$pred2 <- predict(mod2, newdata = new_data)  
new_data$pred3 <- predict(mod3, newdata = new_data) 
new_data$pred4 <- exp(predict(mod4, newdata = new_data)) 

new_data_clean <- new_data[new_data$time > 0, ]

ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "black", size = 2) + 
  #mod1
  geom_line(data = new_data, aes(x = time, y = pred1), color = "blue", linetype = "solid") + 
  #mod2
  geom_line(data = new_data_clean, aes(x = time, y = pred2), color = "red", linetype = "dotted") + 
  #mod3
  geom_line(data = new_data, aes(x = time, y = pred3), color = "green", linetype = "dotdash") + 
  #mod4
  geom_line(data = new_data, aes(x = time, y = pred4), color = "purple", linetype = "twodash") + 
  
  
  labs(x = "TIME (Year)",
       y = "Northampton Yield") +
  scale_x_continuous(limits = c(min(wa_wheat$time) * 0.9, max(wa_wheat$time) * 1.1)) +
  scale_y_continuous(limits = c(min(wa_wheat$northampton) * 0.9, max(wa_wheat$northampton) * 1.1)) +
  theme_minimal() 

#residual plot
# Model 1 
plot(fitted(mod1), resid(mod1),
     main = "Model 1 Residuals",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 2 
plot(fitted(mod2), resid(mod2),
     main = "Model 2 Residuals",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 3 
plot(fitted(mod3), resid(mod3),
     main = "Model 3 Residuals",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)

# Model 4 
plot(fitted(mod4), resid(mod4),
     main = "Model 4 Residuals",
     xlab = "Fitted Values", ylab = "Residuals",
     col = "blue", pch = 16)
abline(h = 0, col = "red", lty = 2)

#error normality tests
cat("Model 1:\n"); print(shapiro.test(resid(mod1)))
cat("Model 2:\n"); print(shapiro.test(resid(mod2)))
cat("Model 3:\n"); print(shapiro.test(resid(mod3)))
cat("Model 4:\n"); print(shapiro.test(resid(mod4)))

#R-squared values
cat("Model 1 (Linear):", summary(mod1)$r.squared, "\n")
cat("Model 2 (Logarithmic):", summary(mod2)$r.squared, "\n")
cat("Model 3 (Quadratic):", summary(mod3)$r.squared, "\n")
cat("Model 4 (Exponential):", summary(mod4)$r.squared, "\n")

#b
coefficients(mod3)

#c
stud_resid <- rstudent(mod3)  
leverage <- hatvalues(mod3) 
dfbetas <- dfbetas(mod3)    
dffits <- dffits(mod3)        


print(dim(dfbetas))  
print(colnames(dfbetas))  


n <- length(stud_resid)  
p <- length(coefficients(mod3)) 
stud_resid_threshold <- 2
leverage_threshold <- 2 * p / n
dfbetas_threshold <- 0.5
dffits_threshold <- 2 * sqrt(p / n)


diagnostics <- data.frame(
  Time = wa_wheat$time,
  Yield = wa_wheat$northampton,
  Stud_Resid = stud_resid,
  Leverage = leverage,

  DFBETAS_Time = ifelse(ncol(dfbetas) >= 2, dfbetas[, 2], NA),
  DFBETAS_Time2 = ifelse(ncol(dfbetas) >= 3, dfbetas[, 3], NA),
  DFFITS = dffits
)


outliers <- subset(diagnostics, 
                   abs(Stud_Resid) > stud_resid_threshold | 
                     leverage > leverage_threshold |
                     abs(DFBETAS_Time) > dfbetas_threshold |
                     abs(DFBETAS_Time2) > dfbetas_threshold |
                     abs(DFFITS) > dffits_threshold)

print(outliers)

#d
train_data <- subset(wa_wheat, time <= 46) 
mod3_restricted <- lm(northampton ~ time + I(time^2), data = train_data)

new_obs <- data.frame(time = 47) 
prediction <- predict(mod3_restricted, newdata = new_obs, interval = "prediction", level = 0.95)


cat("\n=== Prediction for 1997 ===\n")
print(prediction)

true_value <- wa_wheat$northampton[wa_wheat$time == 47]

cat("\n=== Actual Yield in 1997 ===\n")
cat("True value:", true_value, "\n")


if (true_value >= prediction[1, "lwr"] && true_value <= prediction[1, "upr"]) {
  cat("\n✅ The true value falls within the prediction interval.\n")
} else {
  cat("\n❌ The true value is outside the prediction interval.\n")
}


