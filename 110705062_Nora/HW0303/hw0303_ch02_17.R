# Download and load the dataset
url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
file_path <- "collegetown.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(collegetown)

## 2.17.1 Scatter Plot
plot(collegetown$sqft, collegetown$price,
     main = "Scatter Plot of House Price vs. House Size",
     xlab = "House Size (Hundreds of Sq. Ft.)",
     ylab = "House Price (Thousands of Dollars)",
     pch = 19, col = rgb(0, 0, 1, 0.5))  # Semi-transparent blue points

## 2.17.2 Linear Regression Model and Fitted Line
model <- lm(price ~ sqft, data = collegetown)
summary(model)

plot(collegetown$sqft, collegetown$price,
     main = "House Price vs. House Size with Fitted Line",
     xlab = "House Size (Hundreds of Sq. Ft.)",
     ylab = "House Price (Thousands of Dollars)",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(model, col = "red", lwd = 2)

## 2.17.3 Quadratic Regression Model
model_quad <- lm(price ~ I(sqft^2), data = collegetown)
summary(model_quad)

alpha1 <- coef(model_quad)[1] 
alpha2 <- coef(model_quad)[2]  

sqft_value <- 20  
marginal_effect <- 2 * alpha2 * sqft_value
cat("The marginal effect of an additional 100 square feet at 2000 sqft is:", marginal_effect, "\n")

##2.17.4 The Fitted Curve and the Tangent Line
price_at_2000 <- alpha1 + alpha2 * sqft_value^2
slope_tangent <- 2 * alpha2 * sqft_value  

sqft_range <- seq(min(collegetown$sqft, na.rm = TRUE), 
                  max(collegetown$sqft, na.rm = TRUE), 
                  length.out = 100)
fitted_price <- alpha1 + alpha2 * sqft_range^2
tangent_line <- price_at_2000 + slope_tangent * (sqft_range - sqft_value)

plot(collegetown$sqft, collegetown$price,
     main = "Quadratic Regression Fit with Tangent Line",
     xlab = "House Size (Hundreds of Sq. Ft.)",
     ylab = "House Price (Thousands of Dollars)",
     pch = 19, col = rgb(0, 0, 1, 0.5))

lines(sqft_range, fitted_price, col = "red", lwd = 2)
lines(sqft_range, tangent_line, col = "green", lwd = 2, lty = 2)
points(sqft_value, price_at_2000, col = "black", pch = 16, cex = 1.5)
text(sqft_value, price_at_2000, labels = "Tangent Point", pos = 3, cex = 0.8)

##2.17.5 The elacity
price_at_2000 <- alpha1 + alpha2 * sqft_value^2
marginal_effect <- 2 * alpha2 * sqft_value
elasticity <- (marginal_effect * sqft_value) / price_at_2000
cat("The elasticity of PRICE with respect to SQFT at 2000 sqft is:", elasticity, "\n")

##2.17.6 Least Squares Residuals
collegetown$residuals_linear <- resid(model)
collegetown$residuals_quad <- resid(model_quad)

plot(collegetown$sqft, collegetown$residuals_linear,
     main = "Residuals of Linear Model",
     xlab = "House Size (Hundreds of Sq. Ft.)",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)

plot(collegetown$sqft, collegetown$residuals_quad,
     main = "Residuals of Quadratic Model",
     xlab = "House Size (Hundreds of Sq. Ft.)",
     ylab = "Residuals",
     pch = 19, col = rgb(0, 0, 1, 0.5))
abline(h = 0, col = "red", lwd = 2)

##2.17.7 SSE
SSE_linear <- sum(resid(model)^2)
SSE_quad <- sum(resid(model_quad)^2)
cat("SSE for Linear Model:", SSE_linear, "\n")
cat("SSE for Quadratic Model:", SSE_quad, "\n")

