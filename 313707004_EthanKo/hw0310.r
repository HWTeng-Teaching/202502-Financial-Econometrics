## 3.19(a)
library(POE5Rdata)
library(ggplot2)
data(motel)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy"), linewidth = 1) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy"), linewidth = 1, linetype = "dashed") +
  labs(title = "Occupancy Rates Over Time",
       x = "Time",
       y = "Occupancy Percentage",
       color = "Legend") +
  theme_minimal()
## It seems like they tend to move together, also it looks like the motel occupancy rates are slightly higher.

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)
confint(model, level = 0.95)
## We see that this is not precise since 95% of this interval will contain the true parameter.

##(b)
new_data <- data.frame(comp_pct = 70)
predict(model, new_data, interval = "confidence", level = 0.90)

##(c)
beta_2 <- coef(model)["comp_pct"]
se_beta_2 <- summary(model)$coefficients["comp_pct", "Std. Error"]

t_stat <- beta_2 / se_beta_2

alpha <- 0.01
df <- df.residual(model) 
t_critical <- qt(1 - alpha, df) 
cat("t-statistic:", t_stat, "\n")
cat("Critical value (alpha = 0.01):", t_critical, "\n")

if (t_stat > t_critical) {
  cat("Reject H0: There is strong evidence that β2 > 0.\n")
} else {
  cat("Fail to reject H0: No strong evidence that β2 > 0.\n")
}

##(d)
t_stat <- (beta_2 - 1) / se_beta_2

alpha <- 0.01
df <- df.residual(model) 
t_critical <- qt(1 - alpha/2, df)  

cat("t-statistic:", t_stat, "\n")
cat("Critical value (alpha = 0.01): ±", t_critical, "\n")

if (abs(t_stat) > t_critical) {
  cat("Reject H0: There is strong evidence that β2 ≠ 1.\n")
} else {
  cat("Fail to reject H0: No strong evidence that β2 differs from 1.\n")
}

##(e)
motel$residuals <- resid(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

subset_residuals <- motel[motel$time >= 17 & motel$time <= 23, c("time", "residuals")]


print(subset_residuals)
summary(subset_residuals$residuals)


sign_table <- table(sign(subset_residuals$residuals))
print(sign_table)
## This means that the predominant sign is negative.
