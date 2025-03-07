# e

sqft_2000 <- 20
price_2000 <- predict(quad_model, newdata = data.frame(sqft = sqft_2000))

elasticity <- marginal_effect * (sqft_2000 / price_2000)
print(elasticity)

# 0.8819511


# f

collegetown$resid_lm <- resid(lm_model)
collegetown$resid_quad <- resid(quad_model)

p1 <- ggplot(collegetown, aes(x = sqft, y = resid_lm, color = "Linear Model")) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Linear Model" = "blue")) +
  labs(title = "Residuals of Linear Regression",
       x = "House Size (hundreds of square feet)",
       y = "Residuals",
       color = "Model") +
  theme_minimal()

p2 <- ggplot(collegetown, aes(x = sqft, y = resid_quad, color = "Quadratic Model")) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Quadratic Model" = "red")) +
  labs(title = "Residuals of Quadratic Regression",
       x = "House Size (hundreds of square feet)",
       y = "Residuals",
       color = "Model") +
  theme_minimal()

print(p1)
print(p2)

# It seems that as SQFT increases, the residuals also increase rather than being random.


# g

sse_lm <- sum(resid(lm_model)^2)
sse_quad <- sum(resid(quad_model)^2)

print(c(SSE_Linear = sse_lm, SSE_Quadratic = sse_quad))

# SSE_Linear SSE_Quadratic 
# 5262847       4222356 

# Quadratic model has lower SSE. 
# Lower SSE indicates that the data points are closer to the regression line.







