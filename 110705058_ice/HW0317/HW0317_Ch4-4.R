# Define experience range (0 to 30 years)
EXPER <- seq(0, 30, length.out = 100)

# Model 1: Linear
RATING1 <- 64.289 + 0.990 * EXPER

# Model 2: Logarithmic (avoiding log(0) by starting at 1)
EXPER2 <- seq(1, 30, length.out = 100)
RATING2 <- 39.464 + 15.312 * log(EXPER2)

# Plot Model 1 (Linear)
plot(EXPER, RATING1, type = "l", col = "blue", lwd = 2,
     xlab = "Years of Experience", ylab = "RATING",
     main = "Comparison of Fitted Models",
     ylim = c(min(RATING1, RATING2), max(RATING1, RATING2)))

# Add Model 2 (Logarithmic)
lines(EXPER2, RATING2, col = "red", lwd = 2, lty = 2)

# Add legend
legend("bottomright", legend = c("Model 1: Linear", "Model 2: Logarithmic"),
       col = c("blue", "red"), lwd = 2, lty = c(1, 2))
