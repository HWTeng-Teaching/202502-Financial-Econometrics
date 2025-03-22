#Q3.19.a
library(POE5Rdata)
data <- POE5Rdata::motel

par(mfrow=c(2,1))
plot(data$time, data$motel_pct,
     xlab = 'Time',
     ylab = 'Motel(%)')
plot(data$time, data$comp_pct,
     xlab = 'Time',
     ylab = 'Competitors(%)')
par(mfrow=c(1,1))

cor(data$motel_pct, data$comp_pct , use = "everything",
    method = c("pearson"))
mean(data$motel_pct)
mean(data$comp_pct)

mod1 <- lm(motel_pct ~ comp_pct, data = data)
summary(mod1)
ci <- confint(mod1, level = 0.95)

#Q3.19.b
predict(mod1,
        interval = "confidence",
        level = 0.90,
        newdata = data.frame(comp_pct = 70))
#3.19.c

# Degrees of freedom
df <- df.residual(mod1)

# Critical t value at alpha = 0.01, one-tailed
t_critical <- qt(0.99, df)

# t-statistic for comp_pct
t_stat <- summary(mod1)$coefficients["comp_pct", "t value"]

# Conclusion
if (t_stat > t_critical) {
  print("Reject H0: There is strong evidence that beta2 > 0.")
} else {
  print("Fail to reject H0: No strong evidence that beta2 > 0.")
}

#Q3.19.d

# Extract coefficients and standard errors
beta2_hat <- summary(mod1)$coefficients["comp_pct", "Estimate"]
se_beta2 <- summary(mod1)$coefficients["comp_pct", "Std. Error"]

# Hypothesized value
beta2_0 <- 1

# t statistic
t_stat <- (beta2_hat - beta2_0) / se_beta2

# Degrees of freedom
df <- df.residual(mod1)

# Critical t value for two-tailed test at alpha = 0.01
t_critical <- qt(1 - 0.01/2, df)

# Print results
t_stat
t_critical

if (abs(t_stat) > t_critical) {
  print("Reject H0: beta2 is significantly different from 1.")
} else {
  print("Fail to reject H0: No significant difference from 1.")
}

#Q3.19.e

residual = resid(mod1)
plot(data$time,residual)



