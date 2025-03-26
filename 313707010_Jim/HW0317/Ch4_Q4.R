# model 1
exper_vals <- 0:30
rating_vals <- 64.289 + 0.990 * exper_vals
plot(exper_vals, rating_vals, type="l", col="blue", lwd=2,
     main="Model 1: Predicted RATING vs. EXPERIENCE",
     xlab="EXPERIENCE (Years)", ylab="Predicted RATING")

# model 2
exper_vals2 <- 1:30
rating_vals2 <- 39.464 + 15.312 * log(exper_vals2)
plot(exper_vals2, rating_vals2, type="l", col="red", lwd=2,
     main="Model 2: Predicted RATING vs. EXPERIENCE",
     xlab="EXPERIENCE (Years)", ylab="Predicted RATING")