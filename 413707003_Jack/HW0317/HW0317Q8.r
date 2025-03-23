# 4.29
# g

library(tseries)

residuals_loglog <- residuals(loglog_model)

plot(log(cex5_small$income), residuals_loglog, main = "Residuals vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "Residuals", pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)

# I did not observe any patterns in the residual plot.

hist(residuals_loglog, main = "Histogram of Residuals (log-log model)",
     xlab = "Residuals", col = "lightblue", border = "black", freq = FALSE)
lines(density(residuals_loglog), col = "red", lwd = 2)

jb_test_loglog <- jarque.test(residuals_loglog)
print(jb_test_loglog)

# The p-value is less than 0.05, so we reject the null hypothesis, indicating that the regression errors are not normally distributed.


# h

linear_log_model <- lm(food ~ log(income), data = cex5_small)

plot(log(cex5_small$income), cex5_small$food, main = "FOOD vs ln(INCOME)",
     xlab = "INCOME", ylab = "FOOD", pch = 20, col = "blue")
abline(linear_log_model, col = "red", lwd = 2)

r2_linear_log <- summary(linear_log_model)$r.squared
r2_linear_log
# 0.03799984

# I believe this relationship is more well-defined compared to the other models.
# It seems that the linear model fits the data better.
