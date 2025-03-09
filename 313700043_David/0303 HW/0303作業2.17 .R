rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
collegetown

#2.17 a b
#PRICE = B_1, + B_2*SQFT + e

price <- collegetown$price
sqrt <- collegetown$sqft
model <- lm(price ~ sqrt, data = collegetown)
summary(model)

b1 <- coef(model)[1]
b2 <- coef(model)[2]
b1
b2
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = paste0("Price = ", round(b1, 2), " + ", round(b2, 2), " * SQFT") ,
    x = "House Size(hundred of square feet) " , 
    y = "Price(thousands of dollars)"
    )

#2.17 c d e
#PRICE = a_1 + a_2*SQFT^2 + e

model2 <- lm(price ~ I(sqrt^2), data = collegetown)
summary(model2)
a1 <- coef(model2)[1]
a2 <- coef(model2)[2]
a1
a2

ggplot(collegetown, aes(x = sqrt, y = price)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", formula = y ~ I(x^2), color = "red", se = FALSE) +
  labs(
    title = paste0("Price = ", round(a1, 2), " + ", round(a2, 2), " * SQFT^2") ,
    x = "House Size(hundred of square feet" , 
    y = "Price(thousands of dollars)"
  )

sqft_tangent <- 20  
slope_tangent <- 2 * a2 * sqft_tangent  
price_at_20 <- a1 + a2 * (sqft_tangent^2)  

tangent_data <- data.frame(
  sqrt = seq(sqft_tangent - 10, sqft_tangent + 70, length.out = 50),  
  price = price_at_20 + slope_tangent * (seq(sqft_tangent - 10, sqft_tangent + 50, length.out = 50) - sqft_tangent)  # y = m(x - x0) + y0
)

# Plot the quadratic regression and tangent line
ggplot(collegetown, aes(x = sqrt, y = price)) +
  geom_point(color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ I(x^2), color = "red", se = FALSE) +  
  geom_line(data = tangent_data, aes(x = sqrt, y = price), color = "grey", linetype = "dashed", size = 1) +
  geom_point(aes(x = sqft_tangent, y = price_at_20), color = "black", size = 3) +  
  labs(
    title = paste0("Quadratic Regression with Tangent Line at SQFT = ", sqft_tangent),
    x = "House Size (hundred of square feet)", 
    y = "Price (thousands of dollars)"
  ) +
  theme_minimal()

model_linear <- lm(price ~ sqrt, data = collegetown)
model_quadratic <- lm(price ~ I(sqrt^2), data = collegetown)

collegetown$residual_linear <- resid(model_linear)      
collegetown$residual_quadratic <- resid(model_quadratic)  

ggplot(collegetown, aes(x = sqrt, y = residual_linear)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(title = "Residuals from Simple Linear Regression",
       x = "House Size (hundred of square feet)",
       y = "Residuals") +
  theme_minimal()

ggplot(collegetown, aes(x = sqrt, y = residual_quadratic)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(title = "Residuals from Quadratic Regression",
       x = "House Size (hundred of square feet)",
       y = "Residuals") +
  theme_minimal()

#2.17 f g

predicted_price <- fitted(model)  
residuals <- price - predicted_price  
SSE <- sum(residuals^2)  
print(paste0("SSE for the simple regression model: ", round(SSE, 4)))

predicted_price_quad <- fitted(model2)  
residuals_quad <- collegetown$price - predicted_price_quad  
SSE_quad <- sum(residuals_quad^2)  
print(paste0("SSE for the quadratic regression model: ", round(SSE_quad, 4)))


