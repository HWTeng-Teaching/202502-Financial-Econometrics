# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("collegetown")

# a
plot(collegetown$sqft, collegetown$price, main = "Relationship between price & size", xlab = "size (百平方英尺)", ylab = "price (千美元)", pch = 20)

# b
lm_model <- lm(price ~ sqft, data = collegetown)
summary(lm_model)
abline(lm_model, col = "blue", lwd = 2)

# c
quad_model <- lm(price ~ I(sqft^2), data = collegetown)
summary(quad_model)

marginal_effect_20 <- 2 * coef(quad_model)["I(sqft^2)"] * 20
print(marginal_effect_20)

# d
x_vals <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
y_vals <- predict(quad_model, newdata = data.frame(sqft = x_vals))

plot(collegetown$sqft, collegetown$price, pch = 20, col = "grey", main = "quadratic regression: price & size",
     xlab = "size (百平方英尺)", ylab = "price (千美元)")
lines(x_vals, y_vals, col = "red", lwd = 2)

sqft_2000 <- 20  
alpha1 <- coef(quad_model)[1]
alpha2 <- coef(quad_model)[2]
predicted_price_2000 <- predict(quad_model, newdata = data.frame(sqft = sqft_2000))
tangent_slope <- 2 * alpha2 * sqft_2000

tangent_x_vals <- seq(sqft_2000 - 50, sqft_2000 + 50, length.out = 100)
tangent_y_vals <- predicted_price_2000 + tangent_slope * (tangent_x_vals - sqft_2000)

lines(tangent_x_vals, tangent_y_vals, col = "purple", lwd = 2, lty = 2)

# e
Elasticity <- (2 * alpha2 * sqft_2000) * (sqft_2000 / predicted_price_2000)
print(Elasticity)

# f
par(mfrow = c(1, 2))
plot(collegetown$sqft, resid(lm_model), main = "線性回歸殘差", xlab = "面積", ylab = "殘差", pch = 20)

plot(collegetown$sqft, resid(quad_model), main = "二次回歸殘差", xlab = "面積", ylab = "殘差", pch = 20)

# g
SSE_lm <- sum(resid(lm_model)^2)
SSE_quad <- sum(resid(quad_model)^2)
print(paste("linear regression model SSE:", SSE_lm))
print(paste("quadratic regression model SSE:", SSE_quad))

if (SSE_lm < SSE_quad) {
  print("linear regression model is better")
} else {
  print("quadratic regression model is better")
}

