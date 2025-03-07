# a

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/collegetown.rdata"
load(url(url))


library(ggplot2)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  labs(title = "House Price vs. House Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()


# b

lm_model <- lm(price ~ sqft, data = collegetown)
summary(lm_model)  


ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: House Price vs. Size",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()

# beta_1 = -115.4236  (0 square feet -> price = -115423.6)
# beta_2 = 13.4029  (area increase 100 square feet-> price increase 13402.9)


# c

quad_model <- lm(price ~ I(sqft^2), data = collegetown)
summary(quad_model) 

sqft_2000 <- 20 
alpha2 <- coef(quad_model)["I(sqft^2)"]

marginal_effect <- 2 * alpha2 * sqft_2000
print(marginal_effect)
# 7.38076  (area increase 100 square feet-> price increase 7380.76)


# d

sqft_2000 <- 20
price_2000 <- predict(quad_model, newdata = data.frame(sqft = sqft_2000))


slope_2000 <- 2 * coef(quad_model)["I(sqft^2)"] * sqft_2000


ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  geom_point(aes(x = sqft_2000, y = price_2000), color = "blue", size = 3) +  
  geom_abline(intercept = price_2000 - slope_2000 * sqft_2000, 
              slope = slope_2000, color = "blue", linetype = "dashed") +  
  labs(title = "Quadratic Regression with Tangent at 2000 sqft",
       x = "House Size (hundreds of square feet)",
       y = "Price (thousands of dollars)") +
  theme_minimal()



