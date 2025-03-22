library(POE5Rdata)
data("wa_wheat")

y = wa_wheat$northampton
x = wa_wheat$time
lr_lr = lm(y~x)
lr_log = lm(y~log(x))
qr = lm(y~I(x^2))
log_lr = lm(log(y)~x)

plot(x, y, xlab = "time", ylab = "Northampton", main = "model compare")
# 繪圖
lines(x, predict(lr_lr), col = "blue", lwd = 2, lty = 1)      
lines(x, predict(lr_log), col = "red", lwd = 2, lty = 2)      
lines(x, predict(qr), col = "green", lwd = 2, lty = 3)       
lines(x, exp(predict(log_lr)), col = "purple", lwd = 2, lty = 4)  
# 圖例
legend("topleft", legend = c("lr_lr", "lr_log", "qr", "log_lr"),
       col = c("blue", "red", "green", "purple"), lty = c(1, 2, 3, 4), lwd = 2)

#residual
lr_lr_res = resid(lr_lr)
lr_log_res = resid(lr_log)
qr_res = resid(qr)
log_lr_res = resid(log_lr)
plot(lr_lr_res, main = "residual plot")
abline(0, 0)
plot(lr_log_res, main = "residual plot")
abline(0, 0)
plot(qr_res, main = "residual plot")
abline(0, 0)
plot(log_lr_res, main = "residual plot")
abline(0, 0)

#error normality test
#install.packages('tseries')
library(tseries)
library(knitr)
jb_lr_lr = jarque.bera.test(lr_lr_res)
jb_lr_log = jarque.bera.test(lr_log_res)
jb_qr = jarque.bera.test(qr_res)
jb_log_lr = jarque.bera.test(log_lr_res)
# 建立表格存放 p-value
jb_results <- data.frame(
  Model = c("lr_lr", "lr_log", "qr", "log_lr"),
  P_Value = c(jb_lr_lr$p.value, jb_lr_log$p.value, jb_qr$p.value, jb_log_lr$p.value)
)
# 顯示結果表格
kable(jb_results, caption = "Jarque-Bera Test Results")

#values for R2
sm_lr_lr = summary(lr_lr)
sm_lr_log = summary(lr_log)
sm_qr = summary(qr)
sm_log_lr = summary(log_lr)
# 建立表格存放 p-value
sm_results <- data.frame(
  Model = c("lr_lr", "lr_log", "qr", "log_lr"),
  r_square = c(sm_lr_lr$r.squared, sm_lr_log$r.squared, sm_qr$r.squared, sm_log_lr$r.squared)
)
# 顯示結果表格
kable(sm_results, caption = "Multiple R-squared")

#unusual observations
#Studentized residuals
#通常超過 ±2 或 ±3，表示該點可能是異常值。
sr = rstudent(qr)
sr[abs(sr)>2] #潛在異常
sr[abs(sr)>3] #強異常值
index <- which(abs(sr) > 2)
outliers <- data.frame(
  Observation = index,   # 第幾筆資料
  residual_Value = sr[index]  # DFBETAS 值
)
print(outliers)

#leverage 衡量某個觀測值對回歸模型的影響力 (>2(k+1)/n) k:自變數數量
lv = hatvalues(qr)
k = 1
n = 48
threshold = 2*(k+1)/n
index = which(lv>threshold)
outliers <- data.frame(
  Observation = index,   # 第幾筆資料
  leverage_Value = lv[index]  # DFBETAS 值
)
print(outliers)

#DFBETAS（回歸係數變動指標）測量某個觀測值 對每個回歸係數的影響
#絕對值大於 2 / sqrt(n)（n 是樣本數）表示該點影響較大。
#> 2 / sqrt(n)
n = 48
threshold = 2/sqrt(n)
dfb = dfbetas(qr)
index <- which(abs(dfb) > threshold, arr.ind = TRUE)
outliers <- data.frame(
  Observation = index[, 1],   # 第幾筆資料
  Coefficient = colnames(dfb)[index[, 2]],  # 影響的係數
  DFBETAS_Value = dfb[index]  # DFBETAS 值
)
print(outliers)

#DFFITS（影響預測值的變化量）衡量某個觀測值對模型預測結果的影響力。
#絕對值超過 2 * sqrt(p/n)（p 是參數數量，n 是樣本數）表示影響大。
#2 * sqrt((k+1)/n)
dff = dffits(qr)
k = 1
n = 48
threshold = sqrt(2*(k+1)/n)
index = which(abs(dff) > threshold)
outliers <- data.frame(
  Observation = index,   # 第幾筆資料
  DFFITS_Value = dff[index]  # DFBETAS 值
)
print(outliers)

x_train = wa_wheat$time[1:47]
y_train = wa_wheat$northampton[1:47]
quadratic = lm(y_train~I(x_train^2))
alpha = 0.05
x_test = wa_wheat$time[48]
y_test = wa_wheat$northampton[48]
varb1 = vcov(quadratic)[1, 1]
varb2 = vcov(quadratic)[2, 2]
covb1b2 = vcov(quadratic)[1, 2]
varyhat = varb1 + (x_test^2)^2*varb2 + 2*(x_test^2)*covb1b2
sm = summary(quadratic)
vary = (sm$sigma)^2
varf = vary + varyhat
sigmaf = varf^(0.5)
b1 = coef(quadratic)[1]
b2 = coef(quadratic)[2]
yhat = b1 + b2*(x_test^2)
df = df.residual(quadratic)
tc = qt(1-alpha/2, df)
lb = yhat - tc*sigmaf
ub = yhat + tc*sigmaf
c(lb, ub)
predict = (y_test>lb)*(ub>y_test)




