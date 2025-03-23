if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("ggplot2")
install.packages("car") 
library(dplyr)
library(POE5Rdata)
data("wa_wheat")

# 估計四個模型
model1 <- lm(northampton ~ time, data=wa_wheat) #linear
model2 <- lm(northampton ~ log(time), data=wa_wheat)  #linear-log
model3 <- lm(northampton ~ I(time^2), data=wa_wheat)  #Quadratic
model4 <- lm(log(northampton) ~ time, data=wa_wheat)  #log-linear

par(mfrow=c(1,2))  # 圖片並排
# 繪製linear Model(1)原始數據(2)迴歸線(3)殘差圖
plot(wa_wheat$time, wa_wheat$northampton, main="Model 1: Linear", 
     xlab="time", ylab="northampton", pch=19, col="blue")
time_seq <- seq(min(wa_wheat$time), max(wa_wheat$time), length.out=100)
pred_values <- predict(model1, newdata=data.frame(time=time_seq))
lines(time_seq, pred_values, col="red", lwd=2)
plot(model1$residuals, main="Model 1: Residuals", ylab="Residuals", xlab="Index", pch=19, col="darkgreen")

par(mfrow=c(1,2))
# 繪製linear-log Model(1)原始數據(2)迴歸線(3)殘差圖
plot(wa_wheat$time, wa_wheat$northampton, main="Model 2: Log(time)", 
     xlab="time", ylab="northampton", pch=19, col="blue")
time_seq <- seq(min(wa_wheat$time), max(wa_wheat$time), length.out=100)
pred_values <- predict(model2, newdata=data.frame(time=time_seq))
lines(time_seq, pred_values, col="red", lwd=2)
plot(model2$residuals, main="Model 2: Residuals", ylab="Residuals", xlab="Index", pch=19, col="darkgreen")

par(mfrow=c(1,2))
# 繪製Quardratic Model(1)原始數據(2)迴歸線(3)殘差圖
plot(wa_wheat$time, wa_wheat$northampton, main="Model 3: Quadratic", 
     xlab="time", ylab="northampton", pch=19, col="blue")
time_seq <- seq(min(wa_wheat$time), max(wa_wheat$time), length.out=100)
pred_values <- predict(model3, newdata=data.frame(time=time_seq))
lines(time_seq, pred_values, col="red", lwd=2)
plot(model3$residuals, main="Model 3: Residuals", ylab="Residuals", xlab="Index", pch=19, col="darkgreen")

par(mfrow=c(1,2))
# 繪製log-linear Model(1)原始數據(2)迴歸線(3)殘差圖
plot(wa_wheat$time, wa_wheat$northampton, main="Model 4: Log(YIELD)", 
     xlab="time", ylab="northampton", pch=19, col="blue")
time_seq <- seq(min(wa_wheat$time), max(wa_wheat$time), length.out=100)
pred_values <- predict(model4, newdata=data.frame(time=time_seq))
pred_values <- exp(pred_values)
lines(time_seq, pred_values, col="red", lwd=2)
plot(model4$residuals, main="Model 4: Residuals", ylab="Residuals", xlab="Index", pch=19, col="darkgreen")
# 判斷模型好壞
# 殘差正態性檢定
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)
# R平方比較
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared
#Model3最佳
best_model <- model3
#(b)最佳模型之回歸結果
summary(best_model)
#(c)可能影響回歸結果的資料點
library(car)
influence.measures(best_model)
leverage <- hatvalues(best_model)
dfbetas <- dfbetas(best_model)
dffits <- dffits(best_model)
# (d)預測1997
new_data <- data.frame(time=48)
pred <- predict(best_model, newdata=new_data, interval="prediction", level=0.95)
print(pred)