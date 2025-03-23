library(POE5Rdata)
data(cex5_small)

#summary statistics
x = cex5_small$income
y = cex5_small$food
summary(x)
summary(y)
hist(x, main = "histogram of income")
abline(v = mean(x), col = "red", lwd = 2, lty = 2)  
abline(v = median(x), col = "blue", lwd = 2, lty = 2) 
legend("topright",legend = c("mean", "median"), col = c("red","blue"), 
       lwd = c(2,2), lty = c(2,2))
hist(y, main = "histogram of food")
abline(v = mean(y), col = "red", lwd = 2, lty = 2)  
abline(v = median(y), col = "blue", lwd = 2, lty = 2) 
legend("topright",legend = c("mean", "median"), col = c("red","blue"), 
       lwd = c(2,2), lty = c(2,2))
library(tseries)
library(knitr)
jb_x = jarque.bera.test(x)
jb_y = jarque.bera.test(y)
# 建立表格存放 p-value
jb_results <- data.frame(
  Model = c("income", "food"),
  P_Value = c(jb_x$p.value, jb_y$p.value)
)
# 顯示結果表格
kable(jb_results, caption = "Jarque-Bera Test Results")

#linear relationship FOOD = β1 + β2INCOME + e
lr = lm(y~x)
b1 = coef(lr)[1]
b2 = coef(lr)[2]
plot(x, y, xlab = "income", ylab = "food")
abline(b1, b2, col = "red")
sm = summary(lr)
seb2 = sm$coefficients[2, 2]
alpha = 0.05
df = df.residual(lr)
tc = qt(1-alpha/2, df)
lb = b2 - tc*seb2
ub = b2 + tc*seb2
c(lb, ub)

#regression in (b) and plot them against INCOME
res = resid(lr)
hist(res)
plot(x, res, xlab = "Income", ylab = "Residuals", main = "Residual Plot")
abline(h = 0, col = "red")
jb_res = jarque.bera.test(res)
jb_res$p.value

#both a point estimate and a 95% interval estimate of the elasticity of food expenditure
#INCOME = 19, 65, and 160
income = c(19, 65, 160)
food = b1 + b2*income
var_elas = (income/food)^2*(seb2^2)
se_elas = sqrt(var_elas)
point_estimate = b2*(income/food)
lb = point_estimate - tc*se_elas
ub = point_estimate + tc*se_elas
results <- data.frame(
  Income = income,
  elas_Estimate = point_estimate,
  Lower_Bound = lb,
  Upper_Bound = ub
)
print(results)

#ln(FOOD) = γ1 + γ2ln(INCOME) + e
log_log = lm(log(y)~log(x))
r1 = coef(log_log)[1]
r2 = coef(log_log)[2]
plot(log(x), log(y), xlab = "log(income)", ylab = "log(food)")
abline(r1, r2, col = "blue")
sm1 = summary(lr)
sm1$r.squared
sm2 = summary(log_log)
sm2$r.squared

#point and 95% interval estimate of the elasticity for the log-log model
elas = r2
sm2 = summary(log_log)
ser2 = sm2$coefficients[2, 2]
income = c(19, 65, 160)
point_estimate = r2
lb = r2 - tc*ser2
ub = r2 + tc*ser2
results <- data.frame(
  Income = income,
  elas_Estimate = point_estimate,
  Lower_Bound = lb,
  Upper_Bound = ub
)
print(results)

#least squares residuals from the log-log model and plot them against ln(INCOME)
res = resid(log_log)
hist(res)
plot(log(x), res, xlab = "log(income)", ylab = "residuals")
abline(h = 0, col="red")
jb_log_log = jarque.bera.test(res)
jb_log_log$p.value

#linear-log relationship FOOD = α1 + α2ln(INCOME) + e
lr_log = lm(y~log(x))
a1 = coef(lr_log)[1]
a2 = coef(lr_log)[2]
plot(log(x), y, xlab = "log_income", ylab = "food")
abline(a1, a2, col="red")
sm3 = summary(lr_log)
sm3$r.squared

#a point and 95% interval estimate of the elasticity for the linear-log model
income = c(19, 65, 160)
food = a1 + a2*log(income)
elas_est = a2/food
vara2 = (coef(sm3)[2,2])^2
var_elas = (1/food^2)*vara2
se_elas = sqrt(var_elas)
lb = elas_est - tc*se_elas
ub = elas_est + tc*se_elas
results <- data.frame(
  Income = income,
  elas_Estimate = elas_est,
  Lower_Bound = lb,
  Upper_Bound = ub
)
print(results)

#least squares residuals from the linear-log model and plot them against ln(INCOME)
res = resid(lr_log)
plot(log(x), res, xlab = "log(income)", ylab = "residual")
abline(h=0, col="red")
hist(res)
jb_lr_log = jarque.bera.test(res)
jb_lr_log$p.value







