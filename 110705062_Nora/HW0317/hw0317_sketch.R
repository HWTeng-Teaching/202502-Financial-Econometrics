## 4.4.1
intercept <- 64.289
slope <- 0.990
EXPER <- seq(0, 30, by = 1)
RATING_hat <- intercept + slope * EXPER

plot(EXPER, RATING_hat, type = "l", col = "blue", lwd = 2,
     main = "Fitted Values from Model 1",
     xlab = "Years of Experience (EXPER)",
     ylab = "Predicted Performance Rating",
     ylim = c(60, 100))

points(EXPER, RATING_hat, pch = 19, col = "blue")

## 4.4.2
intercept2 <- 39.464
slope2 <- 15.312
EXPER2 <- 1:30
RATING_hat2 <- intercept2 + slope2 * log(EXPER2)

plot(EXPER2, RATING_hat2, type = "l", col = "darkgreen", lwd = 2,
     main = "Fitted Curve from Model 2",
     xlab = "Years of Experience (EXPER)",
     ylab = "Predicted Performance Rating",
     ylim = c(min(RATING_hat2) - 2, max(RATING_hat2) + 2))

points(EXPER2, RATING_hat2, pch = 19, col = "darkgreen")



