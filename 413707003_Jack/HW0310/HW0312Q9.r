#3.19
# c

beta2_hat <- coef(model)["comp_pct"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]

t_stat <- beta2_hat / se_beta2

t_critical <- qt(0.99, df = model$df.residual)

t_stat
# 4.26536
t_critical
# 2.499867

# Since t_stat > t_critical, we reject the null hypothesis, indicating that there is a significant positive correlation between the competitor's occupancy rate and the motel's occupancy rate.


# d

t_stat_1 <- (beta2_hat - 1) / se_beta2

t_critical_1 <- qt(0.995, df = model$df.residual)

t_stat_1
# -0.6677491
t_critical_1
# 2.807336

# Since t_stat < t_critical, we do not reject the null hypothesis, indicating that the motel's occupancy rate and the competitor's occupancy rate vary approximately in proportion.


# e

motel$residuals <- resid(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# I did not find any unusual features in the plot.

subset_residuals <- motel[motel$time >= 17 & motel$time <= 23, c("time", "residuals")]
print(subset_residuals)
#    time  residuals
# 17   17 -12.707328
# 18   18 -11.543226
# 19   19  -8.456225
# 20   20   2.279673
# 21   21  -2.958191
# 22   22 -13.293015
# 23   23 -23.875603



table(sign(subset_residuals$residuals))
# -1  1 
#  6  1 

# Only one residual is positive, while all others are negative.



