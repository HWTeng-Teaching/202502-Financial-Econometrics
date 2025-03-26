# Load necessary packages
if (!require(tseries)) install.packages("tseries")
library(tseries)

# Load dataset
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
download.file(url, destfile = "wa_wheat.rdata")
load("wa_wheat.rdata")

# Extract Northampton data
northampton_data <- data.frame(YIELD = wa_wheat$northampton, TIME = wa_wheat$time)
northampton_data$ln_YIELD <- log(northampton_data$YIELD)
northampton_data$TIME2 <- northampton_data$TIME^2
northampton_data$ln_TIME <- log(northampton_data$TIME)

# Define models
model1 <- lm(YIELD ~ TIME, data = northampton_data)
model2 <- lm(YIELD ~ ln_TIME, data = northampton_data)
model3 <- lm(YIELD ~ TIME2, data = northampton_data)
model4 <- lm(ln_YIELD ~ TIME, data = northampton_data)

# Plot fitted models
par(mfrow = c(2, 2))
plot(northampton_data$TIME, northampton_data$YIELD, main = "Linear Model", xlab = "TIME", ylab = "YIELD", col = "blue", pch = 16)
lines(northampton_data$TIME, fitted(model1), col = "red", lwd = 2)
plot(northampton_data$TIME, northampton_data$YIELD, main = "Linear-Log Model", xlab = "TIME", ylab = "YIELD", col = "blue", pch = 16)
lines(northampton_data$TIME, fitted(model2), col = "red", lwd = 2)
plot(northampton_data$TIME, northampton_data$YIELD, main = "Quadratic Model", xlab = "TIME", ylab = "YIELD", col = "blue", pch = 16)
lines(northampton_data$TIME, fitted(model3), col = "red", lwd = 2)
plot(northampton_data$TIME, northampton_data$YIELD, main = "Log-Linear Model", xlab = "TIME", ylab = "YIELD", col = "blue", pch = 16)
lines(northampton_data$TIME, exp(fitted(model4)), col = "red", lwd = 2)

# Residual analysis
par(mfrow = c(2, 2))
plot(model1$residuals, main = "Residuals: Linear Model", ylab = "Residuals", xlab = "Index", col = "blue", pch = 16)
plot(model2$residuals, main = "Residuals: Linear-Log Model", ylab = "Residuals", xlab = "Index", col = "blue", pch = 16)
plot(model3$residuals, main = "Residuals: Quadratic Model", ylab = "Residuals", xlab = "Index", col = "blue", pch = 16)
plot(model4$residuals, main = "Residuals: Log-Linear Model", ylab = "Residuals", xlab = "Index", col = "blue", pch = 16)

# Normality test (Jarque-Bera)
jb_values <- data.frame(
  Model = c("Linear", "Linear-Log", "Quadratic", "Log-Linear"),
  JB_p_value = c(jarque.bera.test(model1$residuals)$p.value, 
                 jarque.bera.test(model2$residuals)$p.value,
                 jarque.bera.test(model3$residuals)$p.value, 
                 jarque.bera.test(model4$residuals)$p.value)
)
print(jb_values)

# R² values
r2_values <- data.frame(
  Model = c("Linear", "Linear-Log", "Quadratic", "Log-Linear"),
  R2 = c(summary(model1)$r.squared, summary(model2)$r.squared, 
         summary(model3)$r.squared, summary(model4)$r.squared)
)
print(r2_values)

# Select the best model (Quadratic in this case)
summary(model3)

# Outlier detection
northampton_data$student_resid <- rstudent(model3)
outliers <- which(abs(northampton_data$student_resid) > 2)
print(northampton_data[outliers, ])

# Leverage
leverage_values <- hatvalues(model3)
h_bar <- 2 / nrow(northampton_data)
high_leverage <- which(leverage_values > 2 * h_bar)
print(northampton_data[high_leverage, ])

# DFBETAS
dfbetas_values <- dfbetas(model3)
dfbetas_threshold <- 2 / sqrt(nrow(northampton_data))
high_dfbetas <- which(abs(dfbetas_values[,2]) > dfbetas_threshold)
print(northampton_data[high_dfbetas, ])

# Prediction for 1997
train_data <- subset(northampton_data, TIME <= 47)
model_restricted <- lm(YIELD ~ TIME2, data = train_data)
pred_1997 <- predict(model_restricted, newdata = data.frame(TIME2 = 48^2), interval = "prediction", level = 0.95)
print(pred_1997)

# Compare with actual value
actual_1997 <- northampton_data$YIELD[northampton_data$TIME == 48]
print(actual_1997)
