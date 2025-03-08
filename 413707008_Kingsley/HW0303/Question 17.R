# Question 17
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# Load necessary libraries
library(ggplot2)

# a. Plot house price versus the number of hundreds of square feet
plot_a <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  labs(title = "House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()

# b. Estimate the linear regression model
linear_model <- lm(price ~ sqft, data = collegetown)
beta1 <- coef(linear_model)[1]  # Intercept
beta2 <- coef(linear_model)[2]  # Slope

# Create plot with fitted line for part b
plot_b <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)",
       subtitle = paste("PRICE =", round(beta1, 2), "+", 
                        round(beta2, 2), "× SQFT")) +
  theme_minimal()

# c. Estimate the quadratic regression model (only squared term as specified in the problem)
quadratic_model <- lm(price ~ I(sqft^2), data = collegetown)
alpha1 <- coef(quadratic_model)[1]  # Intercept
alpha2 <- coef(quadratic_model)[2]  # Coefficient for squared term

# Calculate marginal effect at 2000 square feet (20 in hundreds)
sqft_value <- 20
marginal_effect <- 2 * alpha2 * sqft_value

# d. Graph the fitted curve for the quadratic model with tangent line
sqft_seq <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
predicted_values <- predict(quadratic_model, newdata = data.frame(sqft = sqft_seq))
plot_data <- data.frame(sqft = sqft_seq, price = predicted_values)

# Calculate point and slope for tangent line at 2000 sqft
price_at_2000 <- alpha1 + alpha2 * 20^2
tangent_slope <- 2 * alpha2 * 20
tangent_intercept <- price_at_2000 - tangent_slope * 20

# Create tangent line data
tangent_x <- c(max(10, 20-5), min(30, 20+5))  # Range around 20, within reasonable bounds
tangent_y <- tangent_intercept + tangent_slope * tangent_x
tangent_data <- data.frame(sqft = tangent_x, price = tangent_y)

plot_d <- ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(alpha = 0.5) +
  geom_line(data = plot_data, aes(x = sqft, y = price), color = "blue", linewidth = 1) +
  geom_line(data = tangent_data, aes(x = sqft, y = price), 
            color = "red", linetype = "dashed", linewidth = 1) +
  annotate("point", x = 20, y = price_at_2000, color = "red", size = 3) +
  annotate("text", x = 20, y = price_at_2000, 
           label = "Tangent at 2000 sqft", color = "red", hjust = -0.1, vjust = -0.5) +
  labs(title = "Quadratic Model with Tangent Line at 2000 sqft",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)",
       subtitle = paste("PRICE =", round(alpha1, 2), "+", 
                        round(alpha2, 4), "× SQFT²")) +
  theme_minimal()

# e. Calculate elasticity at 2000 square feet
elasticity <- (2 * alpha2 * sqft_value) * (sqft_value / price_at_2000)

# f. Calculate residuals and create plots - removed blue lines as requested
linear_residuals <- residuals(linear_model)
quadratic_residuals <- residuals(quadratic_model)

plot_f1 <- ggplot(data.frame(sqft = collegetown$sqft, residuals = linear_residuals), 
                  aes(x = sqft, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Removed geom_smooth() line as requested
  labs(title = "Residuals for Linear Model",
       x = "House Size (hundreds of square feet)",
       y = "Residuals") +
  theme_minimal()

plot_f2 <- ggplot(data.frame(sqft = collegetown$sqft, residuals = quadratic_residuals), 
                  aes(x = sqft, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Removed geom_smooth() line as requested
  labs(title = "Residuals for Quadratic Model (SQFT² only)",
       x = "House Size (hundreds of square feet)",
       y = "Residuals") +
  theme_minimal()

# g. Compare SSE for both models
sse_linear <- sum(linear_residuals^2)
sse_quadratic <- sum(quadratic_residuals^2)

# Print all plots
print(plot_a)
print(plot_b)
print(plot_d)
print(plot_f1)
print(plot_f2)

# Results summary
cat("\nResults:\n\n")

cat("a. Scatter plot of house price against house size created.\n\n")

cat("b. Linear Model: PRICE =", round(beta1, 2), "+", round(beta2, 2), "× SQFT\n")
cat("   Interpretation: For each additional 100 square feet, house price increases by approximately", 
    round(beta2, 2), "thousand dollars.\n\n")

cat("c. Quadratic Model: PRICE =", round(alpha1, 2), "+", round(alpha2, 4), "× SQFT²\n")
cat("   Marginal effect at 2000 square feet:", round(marginal_effect, 2), 
    "thousand dollars per 100 square feet\n\n")

cat("d. Quadratic model with tangent line at 2000 square feet plotted.\n\n")

cat("e. Elasticity at 2000 square feet:", round(elasticity, 3), "\n")
cat("   Interpretation: A 1% increase in house size at 2000 square feet results in approximately a", 
    round(elasticity * 100, 1), "% increase in house price.\n\n")

cat("f. Residual plots created for both models.\n")
cat("   Examining the residual plots for patterns:\n")
# In both models, the residual patterns do not appear random.
# The variation in the residuals increases as SQFT increases.
# This suggests that the homoskedasticity assumption may be violated.

cat("g. Sum of Squared Errors (SSE):\n")
cat("   Linear Model SSE:", round(sse_linear, 2), "\n")
cat("   Quadratic Model (SQFT² only) SSE:", round(sse_quadratic, 2), "\n")
if(sse_quadratic < sse_linear) {
  cat("   The quadratic model has a lower SSE, indicating a better fit to the data.\n")
  cat("   A lower SSE means that the model's predictions are closer to the actual values,\n")
  cat("   resulting in smaller residuals. This suggests that the relationship between\n")
  cat("   house size and price might be better described by a quadratic function.\n")
} else {
  cat("   The linear model has a lower SSE, indicating a better fit to the data.\n")
  cat("   A lower SSE means that the model's predictions are closer to the actual values,\n")
  cat("   resulting in smaller residuals. This suggests that the relationship between\n")
  cat("   house size and price might be adequately described by a linear function.\n")
}
