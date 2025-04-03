# 5.31
# d

t_stat_trains <- (coef(model)["trains"] - 3) / summary(model)$coefficients["trains", "Std. Error"]
qt(0.05, 245)
# -1.651097
qt(0.95, 245)
# 1.651097
t_stat_trains
# 0.03737444 

# Since qt(0.05, 245) < t-value < qt(0.95, 245), we fail to reject the null hypothesis, the delay caused by a train is equal to 3 minutes.


# e

t_stat_depart <- (coef(model)["depart"] - 1 / 3) / (summary(model)$coefficients["depart", "Std. Error"])
qt(0.05, 245)
# -1.651097
t_stat_depart
# 0.9911646

# Since t-value < qt(0.05, 245), we reject the null hypothesis, the increase in travel time when departing at 7:30 AM is less than 10 minutes.


# f

t_stat_ratio <- (coef(model)["trains"] - 3 * coef(model)["reds"]) / sqrt(
  (summary(model)$coefficients["trains", "Std. Error"])^2 +
    (3^2 * (summary(model)$coefficients["reds", "Std. Error"])^2)
)
qt(0.05, 245)
# -1.651097
t_stat_ratio
# -1.830017 

# Since t-value < qt(0.05, 245), we reject the null hypothesis, the delay caused by a train is less than three times the delay caused by a red light.









