#3.19
#a
data("motel")
str(motel)
plot(motel$time,motel$motel_pct,pch=16,cex=0.7,xlab = 'TIME',ylab = 'PCT',type="o")
points(motel$time,motel$comp_pct, pch=16,cex=0.7, col="red",type="o")
legend("topright", legend = c("comp", "motel"), 
       col = c("red", "black"), lwd = 2, bty = "n")
axis(1,labels=1:25,at=1:25)
tab <- lm(motel_pct~comp_pct,data=motel)
summary(tab)
confint(tab, "comp_pct", level = 0.95)

#b
new_data <- data.frame(comp_pct = 70)
predict(tab, newdata = new_data, interval = "confidence",level = 0.9)

#c 
(t <-  0.8646/0.2027)
(t_0.01 <- qt(0.01,23,lower.tail = F))
ifelse(t > t_0.01,'reject the null hypothesis','not to reject the null hypothesis')

#d
(t2 <- (0.8646-1)/0.2027)
(t_0.005 <- qt(0.005,23,lower.tail = F))
ifelse(t2 > t_0.005,'reject the null hypothesis','not to reject the null hypothesis')

#e
summary(tab)$resid
plot(motel$time,summary(tab)$resid,pch=16,cex=0.7,xlab = 'TIME',ylab = 'RESIDUAL(%)',)
axis(1,labels=1:25,at=1:25)
abline(h=0,col='red',lwd=2)
abline(v=c(17,23),col='blue',lwd=1,lty=4)
