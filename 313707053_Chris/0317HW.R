
#4.4(a)
EXPER <- seq(0, 30) #會產生一個從0 到30 的數值序列，總共有100 個點
mod1 <- 64.289 + 0.990 * EXPER
plot(EXPER, mod1, type = "l", col = "orange", 
     xlab = "Years of Experience (EXPER)", ylab = "Performance Rating (RATING)", 
     main = "Fitted Values from Model 1: RATING vs EXPER", lwd = 2)
grid() #當你調用 grid() 時，R 會自動在圖表的背景中添加水平和垂直的網格線

#4.4(b)
EXPER1 <- seq(1, 30) #會產生一個從0 到30 的數值序列，總共有100 個點
mod2 <-39.464 + 15.312*log(EXPER1)
plot(EXPER1, mod2, type = "l", col = "orange", 
     xlab = "Years of LOG Experience (LNEXPER)", ylab = "Performance Rating (RATING)", 
     main = "Fitted Values from Model 2: RATING vs LNEXPER", lwd = 2)
grid()


#4.28(a)
library(POE5Rdata)
POE5Rdata::wa_wheat
mod3 <- lm(northampton~time, data = wa_wheat)
data("wa_wheat")
predicted_values <- predict(mod3, newdata = wa_wheat)
plot(wa_wheat$time,wa_wheat$northampton ,
     col="red",lwd=2,xlab = "Years", ylab = "Yields", 
     main = "linear-linear model",pch = 16)
lines(wa_wheat$time, predicted_values, col = "red", lwd = 2)

res1 <- resid(mod3)
plot(wa_wheat$time,res1,col="red",lwd=2,xlab = "Years", ylab = "Residuals", 
     main = "linear-linear model",pch = 16)

install.packages("tseries")
library(tseries)
ebar <- mean(res1)
sde <- sd(res1)
qqnorm(res1)
hist(res1,col = "grey",freq = FALSE,main = "",ylab = "density",xlab = "ehat") #直方圖的縱軸顯示的是密度而不是頻數。
curve(dnorm(x,ebar,sde),col = 2,add = TRUE,ylab = "density",xlab = "ehat")
jarque.bera.test(res1)
cat(summary(mod3)$r.squared)

#4.28(a2) linear log
mod4 <- lm(northampton~log(time), data = wa_wheat)
data("wa_wheat")
predicted_values1 <- predict(mod4, newdata = wa_wheat)
plot(wa_wheat$time,wa_wheat$northampton ,
     col="red",lwd=2,xlab = "Years", ylab = "Yields", 
     main = "linear-log model",pch = 16)
lines(wa_wheat$time, predicted_values1, col = "blue", lwd = 2)

res1 <- resid(mod4)
plot(wa_wheat$time,res1,col="red",lwd=2,xlab = "Years", ylab = "Residuals", 
     main = "linear-log model",pch = 16)

ebar <- mean(res1)
sde <- sd(res1)
qqnorm(res1)
hist(res1,col = "grey",freq = FALSE,main = "",ylab = "density",xlab = "ehat") #直方圖的縱軸顯示的是密度而不是頻數。
curve(dnorm(x,ebar,sde),col = 2,add = TRUE,ylab = "density",xlab = "ehat")
jarque.bera.test(res1)
cat(summary(mod4)$r.squared)

#4.28(a3) Quadratic
mod5 <- lm(northampton~I(time^2), data = wa_wheat) #I() 函數告訴 R 你希望對變數進行數學運算
data("wa_wheat")
predicted_values2 <- predict(mod5, newdata = wa_wheat)
plot(wa_wheat$time,wa_wheat$northampton ,
     col="red",lwd=2,xlab = "Years", ylab = "Yields", 
     main = "Quadratic model",pch = 16)
lines(wa_wheat$time, predicted_values2, col = "blue", lwd = 2)

res1 <- resid(mod5)
plot(wa_wheat$time,res1,col="red",lwd=2,xlab = "Years", ylab = "Residuals", 
     main = "Quadratic model",pch = 16)

ebar <- mean(res1)
sde <- sd(res1)
qqnorm(res1)
hist(res1,col = "grey",freq = FALSE,main = "",ylab = "density",xlab = "ehat") #直方圖的縱軸顯示的是密度而不是頻數。
curve(dnorm(x,ebar,sde),col = 2,add = TRUE,ylab = "density",xlab = "ehat")
jarque.bera.test(res1)
cat(summary(mod5)$r.squared)


#4.28(a4)
mod6 <- lm(log(northampton)~time, data = wa_wheat) 
data("wa_wheat")
predicted_values3 <- predict(mod6, newdata = wa_wheat)
plot(wa_wheat$time,log(wa_wheat$northampton) ,
     col="red",lwd=2,xlab = "Years", ylab = "log(Yields)", 
     main = "log-linear model",pch = 16)
lines(wa_wheat$time, predicted_values3, col = "blue", lwd = 2)

res1 <- resid(mod6)
plot(wa_wheat$time,res1,col="red",lwd=2,xlab = "Years", ylab = "Residuals", 
     main = "Log linear model",pch = 16)

ebar <- mean(res1)
sde <- sd(res1)
qqnorm(res1)
hist(res1,col = "grey",freq = FALSE,main = "",ylab = "density",xlab = "ehat") #直方圖的縱軸顯示的是密度而不是頻數。
curve(dnorm(x,ebar,sde),col = 2,add = TRUE,ylab = "density",xlab = "ehat")
jarque.bera.test(res1)
cat(summary(mod6)$r.squared)


#4.28(C)
studentized_residuals <- rstudent(mod5)
leverage <- hatvalues(mod5)
dfbetas <- dfbetas(mod5)
dffits_values <- dffits(mod5)
outlier_studentized_residuals <- which(abs(studentized_residuals) > 2)
high_leverage <- which(leverage > 2 * mean(leverage))
high_dfbetas <- which(abs(dfbetas) > 1)
high_dffits <- which(abs(dffits_values) > 2 * sqrt(length(coef(mod5)) / length(mod5$fitted.values)))

outlier_studentized_residuals
high_leverage
high_dfbetas
high_dffits

#4.28(d)
mod5 <- lm(northampton ~ I(time^2), data = wa_wheat)
new_data <- data.frame(time = 47)
predictions <- predict(mod5, newdata = new_data, interval = "prediction", level = 0.95)
predictions

#4.29(a)
POE5Rdata::cex5_small
summary(cex5_small$food)
summary(cex5_small$income)
cat(sd(cex5_small$food))
cat(sd(cex5_small$income))
mean_food <- mean(cex5_small$food)
median_food <- median(cex5_small$food)
hist(cex5_small$food,col = "red",main = "Histogram of Food",xlab = "Food Expenditure")
abline(v = mean_food, col = "black", lwd = 2, lty = 2)   
abline(v = median_food, col = "green", lwd = 2, lty = 2)  
text(mean_food, max(density(cex5_small$food)$y), labels = paste("Mean =", round(mean_food, 2)), col = "black", pos = 3)
text(median_food, max(density(cex5_small$food)$y) * 0.9, labels = paste("Median =", round(median_food, 2)), col = "green", pos = 2)

mean_income <- mean(cex5_small$income)
median_income <- median(cex5_small$income)
hist(cex5_small$food,col = "blue",main = "Histogram of Income",xlab = "Income")
abline(v = mean_income, col = "black", lwd = 2, lty = 2)   
abline(v = median_income, col = "green", lwd = 2, lty = 2)  
text(mean_income, max(density(cex5_small$income)$y), labels = paste("Mean =", round(mean_income, 2)), col = "black", pos = 3)
text(median_income, max(density(cex5_small$income)$y) * 0.9, labels = paste("Median =", round(median_income, 2)), col = "green", pos = 2)

jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)


#(b)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata) #要載入
POE5Rdata::cex5_small
mod11 <- lm(food~income,data=cex5_small)
summary(mod11)
predict <- predict(mod11,data = cex5_small)
plot(cex5_small$income,cex5_small$food,col="red",xlab = "income",ylab = "food",pch = 16)
lines(cex5_small$income,predict,col = "black",lwd = 2)
confint(mod11,parm = "income")

mod11 <- lm(log(food)~log(income),data=cex5_small)
summary(mod11)
mod11 <- lm(food~log(income),data=cex5_small)
summary(mod11)

#(c)
rid <- mod11$residuals
plot(cex5_small$income,rid,pch = 16,col="red",xlab = "income",ylab = "Risiduals")
hist(rid,main="Histogram of Risiduals")
jarque.bera.test(rid)

#(d)
mod1 <- lm(food~income,data=cex5_small)
new_data <- data.frame(income = c(19,65,100))
predicted_value <- predict(mod1, newdata = new_data)
predicted_value
prediction_with_interval <- predict(mod1, newdata = new_data, interval = "confidence", level = 0.95)
prediction_with_interval

#(e)
mod1 <- lm(log(food)~log(income),data=cex5_small)
plot(cex5_small$income,cex5_small$food,pch = 16)
predicted_values <- predict(mod1, newdata = cex5_small)
lines(cex5_small$income, predicted_values, col = "blue", lwd = 2)

#(f)
mod1 <- lm(log(food)~log(income),data=cex5_small)
confint(mod1,parm = "log(income)")

#(g)
Residuals <- mod1$residuals
plot(log(cex5_small$income),Residuals,pch = 16 ,xlab="income",col = "red")
hist(Residuals)
jarque.bera.test(Residuals)

#(h)
mod1 <- lm(food~log(income),data=cex5_small)
plot(cex5_small$income,cex5_small$food,pch = 16)
predicted_values <- predict(mod1, newdata = cex5_small)
lines(cex5_small$income, predicted_values, col = "blue", lwd = 2)
re <- summary(mod1)
re

#(i)
mod00 <- lm(food~log(income),data=cex5_small)
coef(mod00)
se <- summary(mod00)$coefficients[, 2]
beta_1 <- coef(mod00)["log(income)"]
incomes <- c(19, 65, 160)
food_pred <- predict(mod00, newdata = data.frame(income = incomes))
elasticity <- beta_1 * (incomes / food_pred)
confidence_interval <- beta_1 + c(-1, 1) * qt(0.975, df = df.residual(mod00)) * se["log(income)"]
elasticity
confidence_interval

#(J)
mod0 <- lm(food~log(income),data=cex5_small)
rid2 <- mod0$residuals
plot(cex5_small$income,rid2,col="red",main = "linear log residual plot",xlab="income",ylab="residual",pch = 16)
jarque.bera.test(rid2)
hist(rid2)