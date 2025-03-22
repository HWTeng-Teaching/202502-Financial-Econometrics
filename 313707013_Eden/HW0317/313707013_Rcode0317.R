#4.4
library(POE5Rdata)
#a
modol_1 <- function(x){y=64.289+0.990*x}
curve(modol_1,0,30,lwd=3,col='green',xlab = 'Years of experience(EXPER)',ylab = 'Performance rating(RATING)',main='Model 1')
y_at_0 <- modol_1(0)# 計算當 x = 0 時的 y 值
points(0, y_at_0, pch = 19, col = 'red')# 在圖上加上 x=0 的點
text(0, y_at_0, labels = paste0("(0, ", round(y_at_0, 4), ")"), pos = 4, cex = 1)
#b
modol_2 <- function(x){y=39.464+15.312*log(x)}
curve(modol_2,1,30,lwd=3,col='blue',xlab = 'Years of experience(EXPER)',ylab = 'Performance rating(RATING)',main='Model 2')
y_at_1 <- modol_2(1)# 計算當 x = 1 時的 y 值
points(1, y_at_1, pch = 19, col = 'red')# 在圖上加上 x=0 的點
text(1, y_at_1, labels = paste0("(0, ", round(y_at_1, 4), ")"), pos = 4, cex = 1)


#4.28
#a
TIME <- wa_wheat$time
YIELD <- wa_wheat$northampton
model1 <- lm(YIELD ~ TIME)            # 線性模型
model2 <- lm(YIELD ~ log(TIME))       # 對數模型
model3 <- lm(YIELD ~ I(TIME^2))       # 二次模型
model4 <- lm(log(YIELD) ~ TIME)       # 指數模型

#i繪製擬合圖
par(mfrow=c(2,2))
plot(TIME, YIELD, pch=16,cex=0.7,main="Model 1: YIELD ~ TIME")
lines(TIME, fitted(model1), col="blue")
plot(TIME, YIELD, pch=16,cex=0.7,main="Model 2: YIELD ~ ln(TIME)")
lines(TIME, fitted(model2), col="red")
plot(TIME, YIELD, pch=16,cex=0.7,main="Model 3: YIELD ~ TIME^2")
lines(TIME, fitted(model3), col="green")
plot(TIME, log(YIELD), pch=16,cex=0.7,main="Model 4: ln(YIELD) ~ TIME")
lines(TIME, fitted(model4), col="red")
#ii畫殘差圖
par(mfrow=c(2,2))
resid(Model1)
plot(wa_wheat$time, resid(model1), pch = 16, cex = 0.5, 
     xlab = "TIME", ylab = "Residuals", 
     main = 'Residuals of Model 1: YIELD ~ TIME')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)

resid(Model2)
plot(wa_wheat$time, resid(model2), pch = 16, cex = 0.5, 
     xlab = "TIME", ylab = "Residuals", 
     main = 'Residuals of Model 2: YIELD ~ ln(TIME)')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)

resid(Model3)
plot(wa_wheat$time, resid(model3), pch = 16, cex = 0.5, 
     xlab = "TIME", ylab = "Residuals", 
     main = 'Residuals of Model 3: YIELD ~ TIME^2')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)

resid(Model4)
plot(wa_wheat$time, resid(model4), pch = 16, cex = 0.5, 
     xlab = "TIME", ylab = "Residuals", 
     main = 'Residuals of Model 4: ln(YIELD) ~ TIME')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)

# iii及iiii
library(knitr)
# Shapiro-Wilk 正態性檢定
shapiro1 <- shapiro.test(model1$residuals)
shapiro2 <- shapiro.test(model2$residuals)
shapiro3 <- shapiro.test(model3$residuals)
shapiro4 <- shapiro.test(model4$residuals)
# 提取 R² 值
r2_1 <- summary(model1)$r.squared
r2_2 <- summary(model2)$r.squared
r2_3 <- summary(model3)$r.squared
r2_4 <- summary(model4)$r.squared
# 判斷是否為常態分佈 (p-value > 0.05)
normality_check <- function(p_value) {
  if (p_value > 0.05) "Yes" else "No"
}
# 建立結果表格
shapiro_results <- data.frame(
  Model = c("Model 1: YIELD ~ TIME",
            "Model 2: YIELD ~ ln(TIME)",
            "Model 3: YIELD ~ TIME^2",
            "Model 4: ln(YIELD) ~ TIME"),
  W_Statistic = c(shapiro1$statistic, shapiro2$statistic, shapiro3$statistic, shapiro4$statistic),
  P_Value = c(shapiro1$p.value, shapiro2$p.value, shapiro3$p.value, shapiro4$p.value),
  Normality = c(normality_check(shapiro1$p.value),
                normality_check(shapiro2$p.value),
                normality_check(shapiro3$p.value),
                normality_check(shapiro4$p.value)),
  R_Squared = c(r2_1, r2_2, r2_3, r2_4)
)

# 使用 knitr::kable() 美觀輸出
kable(shapiro_results, digits = 4, caption = "Shapiro-Wilk Normality Test & R² Comparison")

#b
summary(model3)

#c
par(mfrow=c(2,2))
studentized_residuals <- rstudent(model3)
plot(1:48,studentized_residuals ,xlab = 'INDEX',main = 'Studentized Residuals')
abline(h =c(-2,2), col = "blue", lwd=3) #95%C.I.

leverage_values <- hatvalues(model3)
plot(1:48,leverage_values,xlab = 'INDEX',main = 'LEVERAGE')
abline(h =2*2/48, col = "blue", lwd=3) #h_bar=2/48

plot(dffits(model3), main = "DFFITS")
abline(h=2*sqrt(2/48),col='blue',lwd=3)
abline(h=-2*sqrt(2/48),col='blue',lwd=3)

dfbetas_plot <- dfbetas(model3)
matplot(dfbetas_plot, type = "h", main = "DFBETAS")
abline(h=2/sqrt(48),col='blue',lwd=3)
abline(h=-2/sqrt(48),col='blue',lwd=3)   

# 1. 找出 Studentized Residuals 超過 ±2 的觀測值
which(abs(studentized_residuals) > 2)

# 2. 找出 Leverage 值超過 2p/n 的觀測值
which(leverage_values > (2*2/48))

# 3. 找出 DFFITS 絕對值超過 2sqrt(p/n) 的觀測值
which(abs(dffits(model3)) > 2*sqrt(2/48))

# 4. 找出 DFBETAS 絕對值超過 2/sqrt(n) 的觀測值
which(apply(abs(dfbetas_plot), 1, max) > 2/sqrt(48))

#d
(sampledate <- wa_wheat[48, 1])
traindata <- wa_wheat[1:47, ]
modeltrain <- lm(YIELD ~ I(TIME^2), data = traindata)
newdata <- data.frame(TIME = 48)
(pred <- predict(modeltrain, newdata, interval = "prediction",
                 level = 0.95))

#4.29
#a
data('cex5_small')
data29<- cex5_small[,c(6,9)]
summary(data29)
sapply(data29, sd)
par(mfrow = c(1, 2))
for(i in 1:2){
  hist_data <- hist(data29[,i], breaks = 35, plot = FALSE) 
  hist_data$counts <- hist_data$counts / sum(hist_data$counts) * 100  # 轉成百分比

  plot(hist_data, main = colnames(data29)[i], xlab = colnames(data29)[i], 
       ylab = "Percent", col = "khaki", border = "black", 
       xlim = c(min(data29[,i]), max(data29[,i])))

  # 計算均值與中位數
  mean_value <- mean(data29[,i])
  median_value <- median(data29[,i])

  # 加入紅色和藍色虛線
  abline(v = mean_value, col = "red", lwd = 2, lty = 2)   
  abline(v = median_value, col = "blue", lwd = 2, lty = 2) 

  # 在紅線和藍線旁標示數值和標籤
  text(mean_value, max(hist_data$counts) * 0.9, 
       labels = paste0("Mean: ", round(mean_value, 2)), 
       col = "red", pos = 4, cex = 0.8)
  text(median_value, max(hist_data$counts) * 0.8, 
       labels = paste0("Median: ", round(median_value, 2)), 
       col = "blue", pos = 4, cex = 0.8)

  # 調整圖例位置
  legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), 
         lwd = 2, lty = 2, bty = "n", x.intersp = 0.5, y.intersp = 0.7)
}

install.packages("tseries")  
library(tseries)  # 加載 tseries 套件
jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

#b
linear_income <- lm(food~income,data=cex5_small)
summary(linear_income)
plot(cex5_small$income,cex5_small$food,pch=16,cex=0.5,xlab = 'Household Income',ylab = 'Food Expenditure')
lines(cex5_small$income, fitted(linear_income), col = "red", lwd = 1)
(ci_linearbeta2 <- confint(linear_income, level = 0.95)[2,])

#c
resid(linear_income)
plot(cex5_small$income, resid(linear_income), pch = 16, cex = 0.5, 
     xlab = "House Income", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)
hist(resid(linear_income),xlab = "Residuals",main="Residual Histogram",breaks = 30)
jarque.bera.test(resid(linear_income))


#d
# 抓出模型的截距和斜率
b1 <- coef(linear_income)[1]  # 取出截距
b2 <- coef(linear_income)[2]  # 取出斜率
# 取得斜率 b2 的信賴區間（95%）
b2_ci <- confint(linear_income, level = 0.95)[2, ]
# 建立一個計算函數，輸入收入後回傳多個值
calc_metrics <- function(income_value) {
  # 使用模型公式，計算預測的 Y 值
  pred_food <- b1 + b2 * income_value
  # 計算收入對 food 的彈性
  e <- b2 * (income_value / pred_food)
  # 計算彈性的信賴區間
  e_ci <- b2_ci * (income_value / pred_food)
  # 回傳結果表格
  data.frame(
    Income = income_value,
    Predicted_Food = pred_food,
    Elasticity = e,
    Elasticity_Lower = e_ci[1],
    Elasticity_Upper = e_ci[2]
  )
}
# 定義要測試的收入值
income_test <- c(19, 65, 160)
# 套用函數到每個收入上，並合併結果
final_result <- do.call(rbind, lapply(income_test, calc_metrics))
final_result

#e
cex5_small$lnfood <- log(cex5_small$food)
cex5_small$lnincome <- log(cex5_small$income)
log_log <- lm(lnfood~lnincome,data=cex5_small)
summary(log_log)
plot(cex5_small$lnincome,cex5_small$lnfood,pch=16,cex=0.5,xlab = 'ln(INCOME)',ylab = 'ln(FOOD)',main="Log_log model")
lines(cex5_small$lnincome, fitted(log_log), col = "red", lwd = 2)
summary(log_log)$r.squared
summary(linear_income)$r.squared

#f
log_b2 <- coef(log_log)[2]  # 取出斜率
log_b2_ci <- confint(log_log, level = 0.95)[2, ]
log_b2
log_b2_ci


#g
resid(log_log)
plot(cex5_small$lnincome, resid(log_log), pch = 16, cex = 0.5, 
     xlab = "ln(Income)", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)
hist(resid(log_log),xlab = "Residuals",main="Residual Histogram",breaks = 40)
jarque.bera.test(resid(log_log))

#h
linear_log <- lm(food~lnincome,data=cex5_small)
summary(linear_log)
plot(cex5_small$lnincome,cex5_small$food,pch=16,cex=0.5,xlab = 'ln(INCOME)',ylab = 'FOOD',main="Linear_log model")
lines(cex5_small$lnincome, fitted(linear_log), col = "red", lwd = 2)
summary(linear_income)$r.squared
summary(log_log)$r.squared
summary(linear_log)$r.squared

#i
income_levels <- c(19, 65, 160)
predicted_food2 <- predict(linear_log, newdata = data.frame(lnincome = log(income_levels)))
linear_log_beta_2 <- coef(linear_log)["lnincome"]  
elasticity2 <- linear_log_beta_2 / predicted_food2  # Elasticity formula
conf_linear_log <- confint(linear_log, "lnincome", level = 0.95)
elasticity_lower2 <- conf_linear_log[1] / predicted_food2 
elasticity_upper2 <- conf_linear_log[2] / predicted_food2 
# Store results in a dataframe
elasticity_results2 <- data.frame(
  INCOME = income_levels,
  Predicted_FOOD = predicted_food2,
  Elasticity = elasticity2,
  Elasticity_Lower = elasticity_lower2,
  Elasticity_Upper = elasticity_upper2
)
print(elasticity_results2)

#j
resid(linear_log)
plot(cex5_small$lnincome, resid(linear_log), pch = 16, cex = 0.5, 
     xlab = "ln(Income)", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2.5, lty = 3)
hist(resid(linear_log),xlab = "Residuals",main="Residual Histogram",breaks = 40)
jarque.bera.test(resid(linear_log))


