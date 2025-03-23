#(a)
# Coefficients for Model 1
intercept <- 64.289
slope <- 0.990

# Experience from 0 to 30 years
EXPER <- 0:30

# Compute fitted values
fitted_values <- intercept + slope * EXPER

# Plot
plot(EXPER, fitted_values, type = "o", col = "blue", pch = 16,
     main = "Fitted Values from Model 1",
     xlab = "Years of Experience", ylab = "Predicted Performance Rating",
     ylim = c(min(fitted_values), max(fitted_values)))
grid()
#(b)
# Coefficients for Model 2
intercept <- 39.464
slope <- 15.312

# Experience from 1 to 30 years (log(EXPER) is undefined at 0)
EXPER <- 1:30

# Compute fitted values
fitted_values <- intercept + slope * log(EXPER)

# Plot
plot(EXPER, fitted_values, type = "o", col = "darkgreen", pch = 16,
     main = "Fitted Values from Model 2",
     xlab = "Years of Experience", ylab = "Predicted Performance Rating",
     ylim = c(min(fitted_values), max(fitted_values)))
grid()
