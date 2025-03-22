library(ggplot2)
library(POE5Rdata)
data("motel")


# (a)
ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel")) +
  geom_line(aes(y = comp_pct, color = "Competitor")) +
  labs(title = "Occupancy Rate Over Time",
       x = "Time", y = "Occupancy Percentage") +
  scale_color_manual(values = c("Motel" = "blue", "Competitor" = "red")) +
  theme_minimal()

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

beta2 <- coef(model)["comp_pct"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]

alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = model$df.residual)
CI_lower <- beta2 - t_crit * se_beta2
CI_upper <- beta2 + t_crit * se_beta2

cat("95% CI: [", CI_lower, ",", CI_upper, "]\n")

# (b)
predicted_wage <- predict(model, newdata = data.frame(comp_pct = 70), interval = "confidence", level = 0.90)
print(predicted_wage)

# (c)
alpha <- 0.01
t_value <- beta2 / se_beta2
t_critical <- qt(1 - alpha, df = model$df.residual)

cat("t 統計量:", t_value)
cat("臨界值:", t_critical)

if (t_value > t_critical) {
  cat("拒絕 H0，beta_2 顯著大於 0。")
} else {
  cat("無法拒絕 H0")
}

# (d)
t_value_1 <- (beta2 - 1) / se_beta2
t_critical_1 <- qt(1 - alpha/2, df = model$df.residual)

cat("t 統計量:", t_value_1, "\n")
cat("臨界值:", t_critical_1, "\n")

if (abs(t_value_1) > t_critical_1) {
  cat("拒絕 H0，beta_2 與 1 有顯著差異\n")
} else {
  cat("無法拒絕 H0")
}

# (e)
motel$residuals <- residuals(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals over Time",
       x = "Time", y = "Residuals") +
  theme_minimal()