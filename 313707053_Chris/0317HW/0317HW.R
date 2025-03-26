
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
gamma2 <- coef(loglog_model)["log(cex5_small$income)"]
se_gamma2 <- summary(loglog_model)$coefficients["log(cex5_small$income)", "Std. Error"]
ci_lower <- gamma2 - qt(0.975, df = nrow(cex5_small) - 2) * se_gamma2
ci_upper <- gamma2 + qt(0.975, df = nrow(cex5_small) - 2) * se_gamma2

beta1 <- coef(linear_model)["cex5_small$income"]
mean_income <- mean(cex5_small$income)
mean_food <- mean(cex5_small$food)
elasticity_linear <- beta1 * (mean_income / mean_food)
se_beta1 <- summary(linear_model)$coefficients["cex5_small$income", "Std. Error"]
se_elasticity_linear <- se_beta1 * (mean_income / mean_food)
ci_lower_linear <- elasticity_linear - qt(0.975, df = nrow(cex5_small) - 2) * se_elasticity_linear
ci_upper_linear <- elasticity_linear + qt(0.975, df = nrow(cex5_small) - 2) * se_elasticity_linear

cat("Point Estimate of Elasticity (Log-Log Model):", gamma2, "\n")
cat("95% Confidence Interval for Elasticity:", ci_lower, "to", ci_upper, "\n")
cat("Elasticity from Linear Model (at mean):", elasticity_linear, "\n")
cat("95% Confidence Interval for Linear Model Elasticity:", ci_lower_linear, "to", ci_upper_linear, "\n")

t_stat <- (gamma2 - elasticity_linear) / sqrt(se_gamma2^2 + se_elasticity_linear^2)
p_value <- 2 * (1 - pt(abs(t_stat), df = nrow(cex5_small) - 2))
cat("t-statistic for difference in elasticities:", t_stat, "\n")
cat("p-value:", p_value, "\n")
if (p_value < 0.05) {
  cat("There's statistical evidence that the elasticities are different\n")
} else {
  cat("There's no statistical evidence that the elasticities are different\n")
}

#(g)
Residuals <- mod1$residuals
plot(log(cex5_small$income),Residuals,pch = 16 ,xlab="income",col = "red")
hist(Residuals)
jarque.bera.test(Residuals)
if (jb_test$p.value < 0.05) {
  cat("Conclusion: Reject the null hypothesis of normality (p < 0.05). The regression errors are not normally distributed.\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis of normality (p >= 0.05). The regression errors are approximately normally distributed.\n")
}

#(h)
linlog_model <- lm(cex5_small$food ~ log(cex5_small$income), data = cex5_small)
print(summary(linlog_model))

par(mfrow = c(1, 3))
plot(cex5_small$income, cex5_small$food, pch = 20, col = "black",
     xlab = "INCOME", ylab = "FOOD",
     main = "Linear Model: FOOD vs INCOME")
abline(linear_model, col = "blue", lwd = 2)

plot(log(cex5_small$income), log(cex5_small$food), pch = 20, col = "black",
     xlab = "ln(INCOME)", ylab = "ln(FOOD)",
     main = "Log-Log Model: ln(FOOD) vs ln(INCOME)")
abline(loglog_model, col = "blue", lwd = 2)

plot(log(cex5_small$income), cex5_small$food, pch = 20, col = "black",
     xlab = "ln(INCOME)", ylab = "FOOD",
     main = "Linear-Log Model: FOOD vs ln(INCOME)")
abline(linlog_model, col = "blue", lwd = 2)
par(mfrow = c(1, 1))


r2_linear <- summary(linear_model)$r.squared
r2_loglog <- cor(fitted(loglog_model), log(cex5_small$food))^2
r2_linlog <- summary(linlog_model)$r.squared

cat("\nR^2 Comparison:\n")
cat("Linear Model R^2:", r2_linear, "\n")
cat("Log-Log Model Generalized R^2:", r2_loglog, "\n")
cat("Linear-Log Model R^2:", r2_linlog, "\n")

#(i)
income_values <- c(19,65, 160)
predictions <- predict(linlog_model, newdata = data.frame(income = income_values), 
                       interval = "confidence", level = 0.95)
print("FOOD 的點估計和 95% 置信區間 (INCOME = 19,65 和 160):")
print(predictions[19,])
print(predictions[65,])
print(predictions[160,])
beta_1 <- coef(linlog_model)["log(cex5_small$income)"]
elasticity_19 <- beta_1 * (19 / predictions[19, "fit"])
elasticity_65 <- beta_1 * (65 / predictions[65, "fit"])
elasticity_160 <- beta_1 * (160 / predictions[160, "fit"])
print("收入彈性 (INCOME = 19):")
print(elasticity_19)
print("收入彈性 (INCOME = 65):")
print(elasticity_65)
print("收入彈性 (INCOME = 160):")
print(elasticity_160)

#(J)
mod0 <- lm(food~log(income),data=cex5_small)
rid2 <- mod0$residuals
plot(cex5_small$income,rid2,col="red",main = "linear log residual plot",xlab="income",ylab="residual",pch = 16)
jarque.bera.test(rid2)
hist(rid2)
