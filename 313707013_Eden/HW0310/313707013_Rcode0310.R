#3.19
#a
library(POE5Rdata)
data("motel")
str(motel)
plot(motel$time,motel$motel_pct,pch=16,cex=0.6,xlab = 'Time',ylab = 'Occupancy rate',type="l")
points(motel$time,motel$comp_pct, pch=16,cex=0.6, col="blue",type="l")
legend("topright", legend=c("percentage motel occupancy", " percentage competitors occupancy"), 
       col=c("black", "blue"),  lty=1, cex=0.8, bty="n")
lin_reg <- lm(motel_pct~comp_pct,data=motel)
summary(lin_reg)
confint(lin_reg, "comp_pct", level = 0.95)#計算信賴區間

#b
new_data <- data.frame(comp_pct = 70)
predict(lin_reg, new_data, interval = "confidence",level = 0.9)

#c 
(t <-  0.8646/0.2027)
(t_0.01 <- qt(0.99,23))
ifelse(t > t_0.01,'reject H0','not to reject H0')


#d
(t2 <- (0.8646-1)/0.2027)
(t_0.005 <- qt(0.005,23))
ifelse(t2 > t_0.005,'not to reject H0','reject H0')

#e
residuals <- summary(lin_reg)$resid
plot(motel$time, residuals, pch=16, cex=0.6, xlab='Time', ylab='Residuals')
abline(h=0, col='red', lwd=2)
abline(v=c(17,23), col='green', lwd=2, lty=4)
lines(motel$time, residuals, col='blue', lwd=1)