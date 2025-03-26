library(POE5Rdata)
data("wa_wheat")
# (a)
model_1 <- lm(northampton ~ time, data=wa_wheat) #linear
model_2 <- lm(northampton ~ log(time), data=wa_wheat)  #linear-log
model_3 <- lm(northampton ~ I(time^2), data=wa_wheat)  #Quadratic
model_4 <- lm(log(northampton) ~ time, data=wa_wheat)  #log-linear

summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

## (i)
par(mfrow = c(2, 2))
plot(wa_wheat$time, wa_wheat$northampton , main="Model 1: Linear", 
     xlab="Time", ylab="Yield", pch=16)
lines(wa_wheat$time, fitted(model_1), col="blue", lwd=2)

plot(wa_wheat$time, wa_wheat$northampton , main="Model 2: Log-linear", 
     xlab="Time", ylab="Yield", pch=16)
lines(wa_wheat$time, fitted(model_2), col="blue", lwd=2)

plot(wa_wheat$time, wa_wheat$northampton , main="Model 3: Quadratic", 
     xlab="Time", ylab="Yield", pch=16)
lines(wa_wheat$time, fitted(model_3), col="blue", lwd=2)

plot(wa_wheat$time, log(wa_wheat$northampton ), main="Model 4: Log-Linear", 
     xlab="Time", ylab="Log(Yield)", pch=16)
lines(wa_wheat$time, fitted(model_4), col="blue", lwd=2)

## (ii)
par(mfrow = c(2, 2))
plot(model_1$residuals, main="Residuals (Model 1)")
plot(model_2$residuals, main="Residuals (Model 2)")
plot(model_3$residuals, main="Residuals (Model 3)")
plot(model_4$residuals, main="Residuals (Model 4)")

shapiro.test(model_1$residuals)
shapiro.test(model_2$residuals)
shapiro.test(model_3$residuals)
shapiro.test(model_4$residuals)
## (iii)
r_squared <- c(summary(model_1)$r.squared, summary(model_2)$r.squared, 
               summary(model_3)$r.squared, summary(model_4)$r.squared)

names(r_squared) <- c("Linear", "Log-Time", "Quadratic", "Log-YIELD")
print(r_squared)

# (c)
stud_res <- rstudent(model_3)
leverage <- hatvalues(model_3)
dfbetas <- dfbetas(model_3)
dffits <- dffits(model_3)

outliers <- which(
  abs(stud_res) > 2 | 
    leverage > (2 * mean(leverage)) | 
    abs(dffits) > 2
)

for (i in outliers) {
  cat("異常觀察值 ID:", i, "\n")
  if (abs(stud_res[i]) > 2) {
    cat("  - Studentized Residuals > 2: 誤差異常大，可能是異常值\n")
  }
  
  if (leverage[i] > (2 * mean(leverage))) {
    cat("  - Leverage 遠大於平均值: 影響回歸結果\n")
  }
  
  if (any(abs(dfbetas[i, ]) > 2)) {
    cat("  - DFBETAS 絕對值 > 2: 觀察值對回歸係數的影響過大\n")
  }
  
  if (abs(dffits[i]) > 2) {
    cat("  - DFFITS 絕對值 > 2: 觀察值對預測值的影響過大\n")
  }
  
  cat("\n")
}
wa_wheat[outliers, ]

# (d)
train_data <- subset(wa_wheat, time <= 47)
model3_train <- lm(northampton ~ I(time^2), data = train_data)
new_data <- data.frame(time = 48)
prediction <- predict(model3_train,
                      newdata = new_data,
                      interval = "prediction",
                      level = 0.95)
actual_1997 <- wa_wheat$northampton[wa_wheat$time == 48]
cat("95% Prediction Interval for northampton in 1997 (time = 48):\n")
print(round(prediction, 4))

cat("\nActual northampton in 1997:", actual_1997, "\n")
if (actual_1997 >= prediction[1, "lwr"] && actual_1997 <= prediction[1, "upr"]) {
  cat("\nThe actual yield is within the 95% prediction interval.\n")
} else {
  cat("\nThe actual yield is not within the 95% prediction interval.\n")
}