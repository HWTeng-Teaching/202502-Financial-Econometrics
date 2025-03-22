# 4.28
# a

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
load(url(url))

model1 <- lm(northampton ~ time, data = wa_wheat)

model2 <- lm(northampton ~ log(time), data = wa_wheat)

model3 <- lm(northampton ~ I(time^2), data = wa_wheat)

model4 <- lm(log(northampton) ~ time, data = wa_wheat)

models <- list(model1, model2, model3, model4)
model_names <- c("YIELD ~ TIME", "YIELD ~ ln(TIME)", "YIELD ~ TIME^2", "ln(YIELD) ~ TIME")

par(mfrow = c(2, 2))

for (i in 1:4) {
  plot(wa_wheat$time, wa_wheat$northampton, pch = 16, col = "black",
       main = paste("Fitted vs Actual:", model_names[i]),
       xlab = "Time", ylab = "YIELD")
  lines(wa_wheat$time, fitted(models[[i]]), col = "blue", lwd = 2)
  legend("topleft", legend = c("Actual Data", "Fitted Line"),
         col = c("black", "blue"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
}

par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(residuals(models[[i]]), main = paste("Residuals:", model_names[i]),
       ylab = "Residuals", xlab = "Index")
  abline(h = 0, col = "red")
}

par(mfrow = c(2, 2))
for (i in 1:4) {
  qqnorm(residuals(models[[i]]), main = paste("QQ Plot:", model_names[i]))
  qqline(residuals(models[[i]]), col = "red")
}

r_squared_values <- sapply(models, function(model) summary(model)$r.squared)
for (i in 1:4) {
  cat(paste("Model", i, "(", model_names[i], ") R^2: ", round(r_squared_values[i], 4), "\n"))
}

# I consider Model 3 preferable because its R^2 is higher than that of the other models.


# b

# Wheat yield is lower in the early stages and then increases at an accelerating rate over time, showing a nonlinear pattern.





















