library(POE5Rdata)
data(collegetown)
?collegetown

#a.
plot(collegetown$sqft, collegetown$price, 
     xlab = "House Size (hundreds of sq ft)", 
     ylab = "Price (thousands of dollars)", 
     main = "Scatter Plot of Price vs. Size")

#b.
# Fit linear model
linear_model <- lm(price ~ sqft, data = collegetown)
summary(linear_model)

# Extract coefficients
beta1 <- coef(linear_model)[1]  # Intercept
beta2 <- coef(linear_model)[2]  # Slope

# Plot scatter with fitted line
plot(collegetown$sqft, collegetown$price, 
     xlab = "House Size (hundreds of sq ft)", 
     ylab = "Price (thousands of dollars)", 
     main = "Linear Regression: Price vs. Size")
abline(linear_model, col = "blue")

#c.
# Create squared term
collegetown$sqft2 <- collegetown$sqft^2

# Fit quadratic model
quad_model <- lm(price ~ sqft2, data = collegetown)
summary(quad_model)

# Extract coefficients
alpha1 <- coef(quad_model)[1]  # Intercept
alpha2 <- coef(quad_model)[2]  # Coefficient of SQFT²

# Marginal effect at SQFT = 20 (2000 sq ft)
marginal_effect <- 2 * alpha2 * 20  # Derivative: dPRICE/dSQFT = 2 * α2 * SQFT


#d.
# Generate predicted values
SQFT_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
predicted <- alpha1 + alpha2 * SQFT_range^2

# Plot curve
plot(collegetown$sqft, collegetown$price, 
     xlab = "House Size (hundreds of sq ft)", 
     ylab = "Price (thousands of dollars)", 
     main = "Quadratic Regression with Tangent at 2000 sq ft")
lines(SQFT_range, predicted, col = "red", lwd = 3)

# Tangent line at SQFT = 20
x0 <- 20
y0 <- alpha1 + alpha2 * x0^2  # Predicted price at SQFT = 20
slope <- 2 * alpha2 * x0       # Slope of tangent
tangent <- function(x) y0 + slope * (x - x0)
x_tangent <- c(10, 35)  # Range around 20
y_tangent <- tangent(x_tangent)
lines(x_tangent, y_tangent, col = "blue", lty = 1, lwd = 2)


#e.
# At SQFT = 20
price_at_20 <- alpha1 + alpha2 * 20^2
elasticity <- (2 * alpha2 * 20) * (20 / price_at_20)
cat("Elasticity at 2000 sqft:", elasticity, "\n")


#f.
# Linear model residuals
linear_resid <- residuals(linear_model)
plot(collegetown$sqft, linear_resid, 
     xlab = "SQFT", ylab = "Residuals", 
     main = "Linear Model Residuals vs. SQFT")
abline(h = 0, col = "red")

# Quadratic model residuals
quad_resid <- residuals(quad_model)
plot(collegetown$sqft, quad_resid, 
     xlab = "SQFT", ylab = "Residuals", 
     main = "Quadratic Model Residuals vs. SQFT")
abline(h = 0, col = "red")


#g.
# SSE for linear model
SSE_linear <- sum(residuals(linear_model)^2)

# SSE for quadratic model
SSE_quad <- sum(residuals(quad_model)^2)

cat("SSE Linear:", SSE_linear, "\n")
cat("SSE Quadratic:", SSE_quad, "\n")

# Compare
if (SSE_linear < SSE_quad) {
  cat("Linear model has lower SSE\n")
} else {
  cat("Quadratic model has lower SSE\n")
}




