url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata"
download.file(url, destfile = "wa_wheat.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("wa_wheat.rdata")

str(wa_wheat)  # 查看結構
head(wa_wheat)  

model1 <- lm(northampton ~ time, data = wa_wheat)                 
model2 <- lm(northampton ~ log(time), data = wa_wheat)           
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)            
model4 <- lm(log(northampton) ~ time, data = wa_wheat)   

summary(model1)
summary(model2)
summary(model3)
summary(model4)

r_squared <- data.frame(
  Model = c("Linear", "Linear-Log", "Quadratic", "Log-Linear"),
  R2 = c(summary(model1)$r.squared, 
         summary(model2)$r.squared, 
         summary(model3)$r.squared, 
         summary(model4)$r.squared)
)
r_squared

ggplot(wa_wheat, aes(x=time, y=northampton)) +
  geom_point() +
  geom_line(aes(y=predict(model1)), color="blue") +  
  geom_line(aes(y=predict(model2)), color="red") +  
  geom_line(aes(y=predict(model3)), color="green") +  
  geom_line(aes(y=exp(predict(model4))), color="purple") +  
  labs(title="Fitted Models", y="YIELD", x="TIME") +
  theme_minimal()

par(mfrow = c(2, 2))  # 2x2 layout

# Model 1: YIELD ~ TIME
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 1: YIELD ~ TIME",
     xlab = "Time", ylab = "Yield", pch = 16)
lines(wa_wheat$time, fitted(model1), col = "blue", lwd = 2)

# Model 2: YIELD ~ log(TIME)
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 2: YIELD ~ log(TIME)",
     xlab = "Time", ylab = "Yield", pch = 16)
lines(wa_wheat$time, fitted(model2), col = "red", lwd = 2)

# Model 3: YIELD ~ TIME²
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 3: YIELD ~ TIME²",
     xlab = "Time", ylab = "Yield", pch = 16)
lines(wa_wheat$time, fitted(model3), col = "green", lwd = 2)

# Model 4: log(YIELD) ~ TIME → back-transform
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 4: log(YIELD) ~ TIME",
     xlab = "Time", ylab = "Yield", pch = 16)
lines(wa_wheat$time, exp(fitted(model4)), col = "purple", lwd = 2)


par(mfrow = c(2,2)) 
plot(model1$residuals, main="Residuals of Model 1")
plot(model2$residuals, main="Residuals of Model 2")
plot(model3$residuals, main="Residuals of Model 3")
plot(model4$residuals, main="Residuals of Model 4")

shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)

influence.measures(model3)  # or your chosen model

# Studentized residuals
rstudent(model3)

# Leverage
hatvalues(model3)

# DFBETAS
dfbetas(model3)

# DFFITS
dffits(model3)


n <- nrow(wa_wheat)         # 樣本數
k <- length(coef(model1))   # 變數數量（包含截距）
n
k
# 1. Studentized Residuals
stud_res <- rstudent(model3)
high_residuals <- which(abs(stud_res) > 2)  # 找出異常大的殘差
high_residuals 

# 2. Leverage
lev <- hatvalues(model3)
high_leverage <- which(lev > 2*k/n)         # 高 leverage 點
high_leverage
# 3. DFBETAS
dfb <- dfbetas(model3)
high_dfbetas <- which(abs(dfb) > 2/sqrt(n), arr.ind = TRUE)  # 有異常影響係數的觀測值
high_dfbetas   #col=1第 i 筆資料對 截距項 (Intercept) 的影響太大
              #col=2第 i 筆資料對 TIME 係數 的影響太大
# 4. DFFITS
dff <- dffits(model3)
high_dffits <- which(abs(dff) > 2*sqrt(k/n))  # 有異常影響預測值的觀測值
high_dffits

par(mfrow=c(2,2))
plot(stud_res, main="Studentized Residuals", ylab="Residuals", xlab="Observation Index")
plot(lev, main="Leverage", ylab="Leverage", xlab="Observation Index")

plot(dff, main="DFFITS", ylab="DFFITS", xlab="Observation Index")
matplot(dfb, type="h", main="DFBETAS", ylab="DFBETAS", xlab="Observation Index")




wa_wheat_train <- subset(wa_wheat, time <= 47)
wa_wheat_test <- subset(wa_wheat,  time == 48)

model <- lm(northampton ~ I(time^2), data = wa_wheat_train)
pred_1997 <- predict(model, newdata = wa_wheat_test, interval = "prediction", level = 0.95)
print(pred_1997)
print(actual_yield_1997)

actual_yield_1997 <- wa_wheat_test$northampton
if (actual_yield_1997 >= pred_1997[,"lwr"] & actual_yield_1997 <= pred_1997[,"upr"]) {
  print("The true value is within the prediction interval.")
} else {
  print("The true value is NOT within the prediction interval.")
}

