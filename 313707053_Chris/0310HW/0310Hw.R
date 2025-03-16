library(PoEdata)
data("motel")
head(motel)

# a)
plot(motel$time, motel$motel_pct, type = "l", col = "blue", lwd = 2,lty = 2,
     xlab = "Time", ylab = "Occupancy Percentage", 
     main = "Occupancy Rates Over Time")


lines(motel$time, motel$comp_pct, col = "red", lwd = 2)

legend("bottom", legend = c("Percentage Motel Occupancy", "Percentage Competitor Occupancy"),
       col = c("blue", "red"), lwd = 2)  #圖例

#(a)ANS:It seems that the occupancy of motel is higher than competitors during the 25 months
# And they indeed tend to move together 

mod1 <- lm(motel_pct ~ comp_pct, data = motel)
summary(mod1)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
se_b1 <- summary(mod1)$coefficients[1,2] #先列後行 擷取SUMMARY的矩陣 
se_b2 <- summary(mod1)$coefficients[2,2]
sprintf("Motel_PCT = %.3f + %.3f COMP_PCT", b1, b2)
sprintf("(se)    (%.3f)    (%.3f)", se_b1, se_b2)
confint(mod1, level = 0.95) #是一個矩陣 直接取
up <- confint(mod1, level = 0.95)[2,1]
down <- confint(mod1, level = 0.95)[2,2]
sprintf("A 95%% interval estimate for B2 is [ %.3f , %.3f ]", up, down) #要寫成%%

#(a) 
#ANS :"Motel_PCT = 21.400 + 0.865 COMP_PCT"
#         (se)    (12.907)    (0.203)" 
# According to the summary of the regression , b2 is significant, so the relationship between comp_pct and motel_pct is significant enough
#"A 95% interval estimate for B2 is [ 0.445 , 1.284 ]"


#(b)
new_motel <- data.frame(comp_pct = 70) #估算預測值
predict(model,new_motel,interval = "confidence",level = 0.9)
lowerbound <- predict(model,new_motel,interval = "confidence",level = 0.9)[1,2] #在 R 裡，變數名稱不能是數字開頭或單純的數字
upperbound <- predict(model,new_motel,interval = "confidence",level = 0.9)[1,3]
sprintf("given cmp_pct = 70,the 90%% estimated interval  is [ %.3f , %.3f ]",lowerbound, upperbound)

#ANS:"given cmp_pct = 70,the 90% estimated interval  is [ 77.382 , 86.467 ]"

#(c)

summary(mod1)
#寫出拒絕域
reject <- qt(1-0.01,23-2)
t_value <- summary(mod1)$coefficient[2,3]
if (t_value>reject) {   #在R中，if 後面用括號是因為要判斷，{}是用來包住要執行的code
  cat("Reject H0, B2 is significantly greater than zero.\n") #cat不會自己換行，要加
}else {cat("Do not reject Ho,insufficient evidence to conclude B2 > 0.\n")}

#ANS:Reject Ho,B2 is different form zero significantly. Actually,the p-value of B2
#is already significant.

#(d)
b2 <- summary(mod1)$coefficient[2,1]
tstat <- (b2-1)/summary(mod1)$coefficient[2,2]
alpha <- 0.01
reject2 <- qt(1-alpha/2,df = 23-2)
if (abs(tstat) > reject) {    #雙尾就是把t統計量加絕對值
  cat("Reject H0, B2 is significantly different from zero.\n")
} else {cat("Do not reject H0.\n")
} 
cat("t-statistic =", round(tstat, 3), "\n")
cat("Critical value =", round(reject2, 3), "\n")

#ANS:Do not reject H0.(b2 isn't different from 1 significantly,which means that the relationship between motel_pct and comp_pct is close and move together ) t-statistic = -0.668   Critical value = 2.831 

#(e)

model <- lm(motel_pct ~ comp_pct, data = motel)
residuals <- model$residuals
plot(motel$time, residuals, type = "b", pch = 16,  #b就是點加線 pch繪出實心圈圈
     xlab = "Time", ylab = "Residuals", 
     main = "Residuals Plot")
abline(h = 0, lty = 2)
abline(v = 17, col = "red", lwd = 2)
abline(v = 23, col = "red", lwd = 2)

table(sign(residuals)) #看正負號

#ANS: For observations 17-23 all the residuals are negative but one.
#     There are several outliers, particularly at periods 2 and 22.
#     The variance of residuals appears to differ between the early and later periods, suggesting possible heteroscedasticity.





