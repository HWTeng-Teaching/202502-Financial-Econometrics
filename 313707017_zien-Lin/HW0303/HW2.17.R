# 安裝並載入必要的套件
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入 CAPM 數據
library(POE5Rdata)
data("collegetown")

# 安裝並載入 ggplot2 以繪製回歸圖
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# ## a.b.原本數據
# y <- collegetown$price
# x <- collegetown$sqft
# 
# print(
#   ggplot(collegetown, aes(x = sqft, y = price)) +
#     geom_point(color = "blue", alpha = 0.6) +  # 散點圖
#     geom_smooth(method = "lm", se = FALSE, color = "red") +  # 回歸線
#     labs(title = "price & sqrt",
#          x = expression(sqft(hundreds)),  # 正確顯示數學符號
#          y = expression(price(thousands))) +
#     theme_minimal() +
#     theme(text = element_text(family = "Arial"))
# )
# 
# results <- list()  # 用於存儲回歸結果
# tab <- lm(y ~ x)
# 
# # 存儲結果
# results$summary <- summary(tab)
# 
# # 顯示回歸結果
# cat("Intercept (Alpha):", coef(tab)[1], "\n")
# cat("Slope (Beta):", coef(tab)[2], "\n")
# print(results$summary)

# ## c.sqft變成平方
# # Define variables
# y <- collegetown$price  # Price
# x <- collegetown$sqft  # Square footage
# 
# # Fit the quadratic regression model
# model <- lm(y ~ I(x^2))  # I(x^2) ensures x is squared in the model
# 
# # View the model summary
# summary(model)
# 
# # Get the coefficient for the squared term (alpha_2)
# alpha_2 <- coef(model)[2]
# 
# # Calculate the marginal effect at 2000 sqft
# sqft_2000 <- 20
# marginal_effect_2000 <- 2 * alpha_2 * sqft_2000  # Marginal effect for 2000 sqft
# 
# # Output the marginal effect
# cat("The marginal effect of an additional 100 square feet at 2000 square feet is:", marginal_effect_2000, "\n")

# ## d.畫切線
# # Define variables
# y <- collegetown$price  # Price
# x <- collegetown$sqft  # Square footage

# # Fit the quadratic regression model
# model <- lm(y ~ I(x^2))  # I(x^2) ensures x is squared in the model
# 
# # View the model summary
# summary(model)
# alpha_1 <- coef(model)[1]  # Intercept
# alpha_2 <- coef(model)[2]  # Coefficient for x^2
# 
# # Predict the prices based on the fitted model (Price = alpha1 + alpha2 * sqft^2)
# price_fitted <- predict(model, newdata = data.frame(x = sqft_values))
# 
# # Calculate the marginal effect (slope of the tangent line at 2000 sqft)
# sqft_at_2000 <- 20
# marginal_effect_at_2000 <- 2 * alpha_2 * sqft_at_2000  # Marginal effect for 2000 sqft
# 
# # Calculate the predicted price at 2000 sqft
# price_at_2000 <- predict(model, newdata = data.frame(x = sqft_at_2000))
# 
# # Calculate the tangent line (using the equation of the line: y = mx + b)
# tangent_line <- price_at_2000 + marginal_effect_at_2000 * (sqft_values - sqft_at_2000)
# 
# # Plot the fitted quadratic curve and the tangent line at 2000 sqft
# ggplot() +
#   # Fitted quadratic curve
#   geom_line(aes(x = sqft_values, y = price_fitted), color = "red", linewidth = 1.2) +  # 使用 linewidth 替代 size
#   # Tangent line at 2000 sqft
#   geom_line(aes(x = sqft_values, y = tangent_line), color = "black", linetype = "dashed", linewidth = 1) +  # 使用 linewidth 替代 size
#   # Points from the data
#   geom_point(data = collegetown, aes(x = sqft, y = price), color = "blue", alpha = 0.6) +
#   labs(title = "Fitted Quadratic Curve with Tangent Line at 2000 sqft",
#        x = "Square Footage",
#        y = "Price") +
#   theme_minimal() +
#   theme(text = element_text(family = "Arial"))

# ## e.彈性
# y <- collegetown$price  # Price
# x <- collegetown$sqft  # Square footage
# # Fit the quadratic regression model
# model <- lm(y ~ I(x^2))  # I(x^2) ensures x is squared in the model
# 
# # Extract coefficients
# alpha_2 <- coef(model)[2]  # Coefficient for x^2
# 
# # Calculate the predicted price at 2000 sqft
# sqft_at_2000 <- 20
# price_at_2000 <- predict(model, newdata = data.frame(x = sqft_at_2000))
# 
# # Calculate the marginal effect (slope) at 2000 sqft
# marginal_effect_at_2000 <- 2 * alpha_2 * sqft_at_2000
# 
# # Compute elasticity
# elasticity_at_2000 <- marginal_effect_at_2000 * (sqft_at_2000 / price_at_2000)
# 
# # Output the result
# cat("The elasticity of PRICE with respect to SQFT at 2000 square feet is:", elasticity_at_2000, "\n")

# ## f.殘差
# # 計算一次項回歸的殘差
# model_linear <- lm(price ~ sqft, data = collegetown)
# residuals_linear <- residuals(model_linear)
# 
# # 計算二次項回歸的殘差
# model_quadratic <- lm(price ~ I(sqft^2), data = collegetown)
# residuals_quadratic <- residuals(model_quadratic)
# 
# # 繪製一次項回歸的殘差
# ggplot() +
#   geom_point(aes(x = collegetown$sqft, y = residuals_linear), color = "blue", alpha = 0.6) +
#   labs(title = "Residuals for Linear Regression (Price ~ SQFT)",
#        x = "Square Footage",
#        y = "Residuals") +
#   theme_minimal() +
#   theme(text = element_text(family = "Arial"))
# 
# # 繪製二次項回歸的殘差
# ggplot() +
#   geom_point(aes(x = collegetown$sqft, y = residuals_quadratic), color = "red", alpha = 0.6) +
#   labs(title = "Residuals for Quadratic Regression (Price ~ SQFT^2)",
#        x = "Square Footage",
#        y = "Residuals") +
#   theme_minimal() +
#   theme(text = element_text(family = "Arial"))

## g.計算殘差平方和(SSE)
# 1. 定義變數
y <- collegetown$price  # 觀察值（價格）
x <- collegetown$sqft  # 自變數（面積）

# 2. 模型1：線性回歸模型
model1 <- lm(y ~ x)  # 線性回歸模型
pred1 <- predict(model1)  # 預測值
residuals1 <- y - pred1  # 殘差
rss1 <- sum(residuals1^2)  # 殘差平方和

# 3. 模型2：二次回歸模型
model2 <- lm(y ~ I(x^2))  # 二次回歸模型
pred2 <- predict(model2)  # 預測值
residuals2 <- y - pred2  # 殘差
rss2 <- sum(residuals2^2)  # 殘差平方和

# 4. 顯示結果
cat("Model 1 (Linear) Residual Sum of Squares (RSS):", rss1, "\n")
cat("Model 2 (Quadratic) Residual Sum of Squares (RSS):", rss2, "\n")






