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

b1 <- coef(model)[[1]]
b2 <- coef(model)[[2]]
b3 <- coef(model)[[3]]
b4 <- coef(model)[[4]]
varb3 <- vcov(model)[3,3]
varb4 <- vcov(model)[4,4]
covb3b4 <- vcov(model)[3,4]
t = (b4 - 3 * b3)/sqrt(varb4 + 9 * varb3 - 2 * 3 * covb3b4)

qt(0.05, 245)
# -1.651097
t
# -1.825027

# Since t-value < qt(0.05, 245), we reject the null hypothesis, the delay caused by a train is less than three times the delay caused by a red light.









