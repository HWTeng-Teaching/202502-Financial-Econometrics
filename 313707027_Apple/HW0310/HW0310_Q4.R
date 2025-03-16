#3.19
data("motel")

# (a) 
plot(motel$time, motel$motel_pct,
     type = "l", col = "blue",
     xlab = "Time", ylab = "Occupancy Percentage",
     main = "Occupancy Rates Over Time")
lines(motel$time, motel$comp_pct, col = "red")
legend("topright", legend = c("Motel Occupancy", "Competitor Occupancy"), 
       col = c("blue", "red"), lty = 1, cex =0.8, bty = "n")

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

confint(model, level = 0.95)

# (b) 
predict(model, newdata = data.frame(comp_pct = 70),
        interval = "confidence", level = 0.90)

# (c) 
coef_summary <- summary(model)$coefficients
beta2 <- coef_summary[2, 1] 
t_stat <- coef_summary[2, 3]
df <- model$df.residual
p_value <- (qt(0.01,23,lower.tail = F))
list(t_stat = t_stat, p_value = p_value)

# (d) 
beta2_se <- coef_summary[2, 2]
t_stat2 <- (beta2 - 1) / beta2_se
p_value2 <- (qt(0.005,23,lower.tail = F))
list(t_stat2 = t_stat2, p_value2 = p_value2)

# (e) 
motel$residuals <- resid(model)
plot(motel$time, motel$residuals,
     xlab = "Time", ylab = "Residuals",
     main = "Residuals Over Time",pch = 16, cex=0.7)
abline(h = 0, lty = 2, col = "red")
abline(v=c(17,23),col='blue',lwd=2 ,lty=2)

subset(motel, time >= 17 & time <= 23, select = residuals)
