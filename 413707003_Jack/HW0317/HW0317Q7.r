# 4.29
# e

loglog_model <- lm(log(food) ~ log(income), data = cex5_small)
summary(loglog_model)

ggplot(cex5_small, aes(x = log(income), y = log(food))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Log(FOOD) vs Log(INCOME)", x = "log(INCOME)", y = "log(FOOD)")

fitted_values <- fitted(loglog_model)
correlation <- cor(cex5_small$food, fitted_values)
generalized_R2 <- correlation^2  
generalized_R2
# 0.03799984

# The R^2 of the linear model is higher than that of the log-log model, so the linear model fits the data better.


# f
coef(loglog_model)["log(income)"]
# log(income) 
# 0.1863054 

confint(loglog_model, "log(income)", level = 0.95)
#     2.5 %    97.5 %
# 0.1293432 0.2432675

# confident interval [0.1293432, 0.2432675]

# The interval of the log-log model includes the interval of the linear model at income = 65, but overall, they are dissimilar.


