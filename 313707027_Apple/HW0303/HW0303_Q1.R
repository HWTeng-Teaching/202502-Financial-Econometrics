# 先下載套件和 data
install.packages("remotes")
library(remotes)
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)

# 2.17
# (a)
data("collegetown")
mod1 <- lm(price~sqft, data = collegetown)
summary(mod1)
plot(collegetown$sqft, collegetown$price, 
     xlab = "House Area", ylab = "House Price", 
     pch = 16, col = "blue", cex = 0.5, 
     main = "Linear Regression Model")
# (b)
abline(mod1, col = "red", lwd = 1.5)

# (c)
quad_model <- lm(price~I(sqft^2), data = collegetown)
summary(quad_model)
sqft_value <- 2000/100
marginal_effect <- 2 * coef(quad_model)["I(sqft^2)"] * sqft_value
marginal_effect

#(d)
plot(collegetown$sqft, collegetown$price, 
     xlab = "House Area", ylab = "House Price", 
     pch = 16, col = "blue", cex = 0.5, 
     main = "Quadratic Regression Model")
quad_fun <- function(x){
  y=coef(mod2)[1]+coef(mod2)[2]*x^2
  return(y)
}
curve(quad_fun, col = "red",add = TRUE,lwd=1.5)

# tangent line
x0 <- 2000/100 
y0 <- coef(mod2)[1] + coef(mod2)[2] * x0^2

slope_at_x0 <- 2 * coef(mod2)[2] * x0

tangent_fun <- function(x) {
  return(y0 + slope_at_x0 * (x - x0))
}
curve(tangent_fun, col = "black", add = TRUE, lwd = 2)
legend("topleft", legend = c("Fitted Curve", "Tangent Line"), 
       col = c("red", "black"), lwd = 2)
# (e)
price_2000 <- coef(quad_model)[1] + coef(quad_model)[2] * sqft_value^2
elasticity <- (marginal_effect * sqft_value) / price_2000
elasticity

# (f) 
residuals_linear <- resid(mod1)
residuals_quadratic <- resid(quad_model)

par(mfrow=c(1,2))  
plot(collegetown$sqft, residuals_linear, 
     xlab = "House Area (SQFT)", ylab = "Residuals", 
     main = "LR Residuals", 
     pch = 16, col = "blue", cex = 0.5)

plot(collegetown$sqft, residuals_quadratic, 
     xlab = "House Area (SQFT)", ylab = "Residuals", 
     main = "Quad Residuals", 
     pch = 16, col = "green", cex = 0.5)


#g
SSE_linear <- sum(residuals_linear^2)
SSE_quad <- sum(residuals_quadratic^2)

cat("SSE for Linear Regression: ", SSE_linear, "\n")
cat("SSE for Quadratic Regression: ", SSE_quad, "\n")

