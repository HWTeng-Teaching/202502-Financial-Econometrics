#3.19(a)

library(POE5Rdata)
data("motel")
str(motel)
plot(motel$time,motel$motel_pct,pch=16, col="blue", xlab = 'Time',ylab = 'Occpancy rate',type="o")
points(motel$time,motel$comp_pct, pch=16, col="red",type="o")
legend("topright", legend = c("comp", "motel"), 
       col = c("red", "blue"), lwd = 2, bty = "n")
model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)
conf_int <- confint(model, level = 0.95)
print(conf_int)

#3.19(b)

new_comp_pct <- data.frame(comp_pct = 70)
predict(model, newdata = new_comp_pct, interval = "confidence",level = 0.9)

#3.19(c)

summary(model)$coefficients
t_stat <- summary(model)$coefficients[2, 3]  # β2 的 t 統計量
t_0.01 <- qt(0.01, 23, lower.tail = F)
if (t_0.01 < t_stat) {
  cat("Reject H0: There is evidence that β2 > 0.\n")
} else {
  cat("Fail to reject H0: There is insufficient evidence to suggest that β2 > 0.\n")
}

#3.19(d)

beta2 <- summary(model)$coefficients[2, 1]
se_beta2 <- summary(model)$coefficients[2, 2]
t_stat_d <- (beta2 - 1) / se_beta2
cat(t_stat_d)
t_0.005 <- qt(0.005, 23, lower.tail = FALSE)
cat(t_0.005)
if (t_0.005 < t_stat_d) {
  cat("Reject H0: There is evidence that β2 > 0.\n")
} else {
  cat("Fail to reject H0: There is insufficient evidence to suggest that β2 > 0.\n")
}

#3.19(e)

summary(model)$resid
plot(motel$time,summary(tab)$resid,pch=16,xlab = 'Time',ylab = 'Residuals',)
abline(h=0,col='blue',lwd=2)
abline(v=c(17,23),col='red',lwd=1,lty=4)
