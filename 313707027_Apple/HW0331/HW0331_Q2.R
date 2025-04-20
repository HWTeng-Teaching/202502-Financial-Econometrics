# 5.31
library(POE5Rdata)
data("commute5")

# (a)
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

# (b)
confint(model, level = 0.95)

# (c)
qt(0.05, 245)


# (f) se(\beta_4 - 3\beta_3)
vcov_matrix <- vcov(model)
c_vec <- c(0, 0, -3, 1)  
(sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec))

# (g) E(TIME|X)
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
(predicted_TIME <- predict(model, newdata = new_data, se.fit = TRUE))