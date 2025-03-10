# If your data is in a data frame named 'collegetown':
# - SQFT is the house size in hundreds of square feet
# - PRICE is the sale price in thousands of dollars

# Basic scatter plot:
plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Sale Price (thousands of dollars)",
     main = "Scatter Plot of House Price vs. House Size")

# Optionally, add a simple linear regression line:
model <- lm(price ~ sqft, data = collegetown)
abline(model, col = "red", lwd = 2)



model_quadratic <- lm(price ~ I(sqft^2), data = collegetown)


summary(model_quadratic)


plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Sale Price (thousands of dollars)",
     main = "Quadratic Regression: House Price vs. House Size")


sqft_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
predicted_price <- predict(model_quadratic, newdata = data.frame(sqft = sqft_range))
lines(sqft_range, predicted_price, col = "blue", lwd = 2)



alpha2 <- coef(model_quadratic)["I(sqft^2)"]


marginal_effect_2000 <- 2 * alpha2 * 20  
marginal_effect_2000







collegetown$sqft2 <- collegetown$sqft^2


model_quadratic <- lm(price ~ sqft + sqft2, data = collegetown)


sqft_range <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)


predicted_price <- predict(model_quadratic, newdata = data.frame(sqft = sqft_range, sqft2 = sqft_range^2))


plot(collegetown$sqft, collegetown$price,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Sale Price (thousands of dollars)",
     main = "Quadratic Regression: House Price vs. House Size",
     pch = 16, col = "blue")


lines(sqft_range, predicted_price, col = "red", lwd = 2)


alpha1 <- coef(model_quadratic)["sqft"]
alpha2 <- coef(model_quadratic)["sqft2"]
sqft_2000 <- 20  
slope_2000 <- alpha1 + 2 * alpha2 * sqft_2000  


price_2000 <- predict(model_quadratic, newdata = data.frame(sqft = sqft_2000, sqft2 = sqft_2000^2))


abline(a = price_2000 - slope_2000 * sqft_2000, b = slope_2000, col = "black", lwd = 2, lty = 2)





elasticity_2000 <- (slope_2000 * sqft_2000) / price_2000
elasticity_2000




residuals_linear <- residuals(model_linear)




plot(collegetown$sqft, residuals_linear,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Residuals",
     main = "Residual Plot: Linear Model",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2) 



residuals_quadratic <- residuals(model_quadratic)

plot(collegetown$sqft, residuals_quadratic,
     xlab = "House Size (hundreds of square feet)",
     ylab = "Residuals",
     main = "Residual Plot: Quadratic Model",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)  






SSE_linear <- sum(residuals_linear^2)


SSE_quadratic <- sum(residuals_quadratic^2)


SSE_linear
SSE_quadratic
