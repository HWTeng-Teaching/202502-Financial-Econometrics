# Download and load the dataset
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
file_path <- "motel.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(motel)

## 3.19.1
# the plot
plot(motel$time, motel$motel_pct, type="l", col="blue", lwd=2,
     ylim=c(min(motel$motel_pct, motel$comp_pct), max(motel$motel_pct, motel$comp_pct)),
     xlab="time", ylab="Occupancy Rate (%)", main="motel_pct and comp_pct Over time")

lines(motel$time, motel$comp_pct, col="red", lwd=2)

legend("topright", legend=c("Motel Occupancy (motel_pct)", "Competitor Occupancy (comp_pct)"),
       col=c("blue", "red"), lwd=2, bty="n")

# the regression model
model <- lm(motel_pct ~ comp_pct, data=motel)
summary(model)

# the confidence level
confint(model, level=0.95)

# check the precision
summary(model)$coefficients


## 3.19.2 Confidence Interval 90%
new_data <- data.frame(comp_pct = 70)
predict(model, newdata = new_data, interval = "confidence", level = 0.90)

## 3.19.3
beta2_hat <- summary(model)$coefficients["comp_pct", "Estimate"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]
t_stat <- beta2_hat / se_beta2
df <- nrow(motel) - 2
t_critical <- qt(1 - 0.01, df)
reject_H0 <- t_stat > t_critical

cat("Test Statistic (t):", t_stat, "\n")
cat("Critical Value (t_critical):", t_critical,"\n")
cat("Reject H0?:", reject_H0, "\n")

## 3.19.4
beta2_hat <- summary(model)$coefficients["comp_pct", "Estimate"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]

t_stat <- (beta2_hat - 1) / se_beta2
cat("Test Statistic (t):", t_stat, "\n")

df <- nrow(motel) - 2
t_critical <- qt(1 - 0.01 / 2, df)
cat("Critical Value (t_critical):", t_critical,"\n")

reject_H0 <- abs(t_stat) > t_critical
cat("Reject H0?:", reject_H0, "\n")


## 3.19.5
motel$residuals <- residuals(model)

plot(motel$time, motel$residuals, type="o", col="blue",
     xlab="time", ylab="Residuals", main="Residuals Over time")

abline(h=0, col="red", lty=2)
points(motel$time[motel$time >= 17 & motel$time <= 23], motel$residuals[motel$time >= 17 & motel$time <= 23], col="red", pch=19)



