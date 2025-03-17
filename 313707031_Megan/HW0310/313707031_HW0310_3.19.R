#3.19 (a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data(motel)

install.packages("ggplot2")
library(ggplot2)

plot(motel$time, motel$motel_pct, type = "l", col = "purple",
     ylim = range(c(motel$motel_pct, motel$comp_pct)),
     xlab = "Time", ylab = "Occupancy Rate (%)",
     main = "Motel vs Competitor Occupancy Rate Over Time")
lines(motel$time, motel$comp_pct, col = "pink")

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

confint(model, level = 0.95)

#3.19 (b)
newdata <- data.frame(comp_pct = 70)
predict(model, newdata, interval = "confidence", level = 0.90)

#3.19 (c)
beta2 <- summary(model)$coefficients["comp_pct", "Estimate"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]

t_value <- beta2 / se_beta2

alpha <- 0.01
df <- df.residual(model)
critical_value <- qt(1 - alpha, df)

t_value
critical_value

if (t_value > critical_value) {
  print("Reject H0: There is significant evidence that β2 > 0 at α = 0.01.")
} else {
  print("Fail to reject H0.")
}

#3.19 (d)
t_value2 <- (beta2 - 1) / se_beta2
critical_value2 <- qt(1 - alpha / 2, df)

t_value2
critical_value2

if (abs(t_value2) > critical_value2) {
  print("Reject H0: β2 is not equal to 1 at α = 0.01.")
} else {
  print("Do not reject H0.")
}

#3.19 (e)
residuals <- resid(model)

plot(motel$time, residuals, type = "o",
     xlab = "Time", ylab = "Residuals",
     main = "Residuals from Regression of MOTEL_PCT on COMP_PCT")
abline(h = 0, col = "purple", lty = 2)

subset_residuals <- residuals[motel$time >= 17 & motel$time <= 23]
subset_residuals

sign(subset_residuals)  #若大部分為負，表示這段時間 motel 的入住率比模型預測低
