#CH2 2.17
#(a) 

remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("collegetown")

collegetown <- na.omit(collegetown)

if (nrow(collegetown) > 0) {
  plot(collegetown$sqft, collegetown$price, 
       main = "Regression of House Price vs. House Size",
       xlab = "House Size (hundreds of sq. ft.)", 
       ylab = "Price (thousands of dollars)",
       pch = 19, col = "purple")
} else {
  print("錯誤：數據集中所有值皆為 NA，無法繪製散點圖！")
}

#(b)
linear_model <- lm(price ~ sqft, data = collegetown)
print(summary(linear_model))
abline(linear_model, col = "red", lwd = 2)

#(c)
# 計算邊際影響
sqft_2000 <- 20  
marginal_effect <- coef(quadratic_model)[2] * 2 * sqft_2000
print(paste("Marginal Effect at 2000 sqft:", marginal_effect))

#(d)
colnames(collegetown) <- tolower(colnames(collegetown))
if (!"sqft" %in% colnames(collegetown) | !"price" %in% colnames(collegetown)) {
  stop("錯誤：數據集沒有 sqft 或 price 變數！請檢查數據集。")
}
if (max(collegetown$sqft) < 100) {
  collegetown$sqft <- collegetown$sqft * 100  # 轉換為平方英尺
}
collegetown <- na.omit(collegetown)
if (nrow(collegetown) == 0) {
  stop("錯誤：數據集中所有值皆為 NA，無法繪製散點圖！")
}
quadratic_model <- lm(price ~ poly(sqft, 2, raw=TRUE), data = collegetown)
predicted_data <- data.frame(sqft = seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100))
predicted_data$price <- predict(quadratic_model, newdata = predicted_data)
y_2000 <- predict(quadratic_model, newdata = data.frame(sqft = 2000))
coefficients <- coef(quadratic_model)
slope_2000 <- coefficients[2] + 2 * coefficients[3] * 2000  # f'(2000)
intercept <- y_2000 - slope_2000 * 2000  # b = y - m*x
tangent_data <- data.frame(
  sqft = seq(1500, 2500, length.out = 100),
  price = intercept + slope_2000 * seq(1500, 2500, length.out = 100)
)
p <- ggplot() +
  geom_point(data = collegetown, aes(x = sqft, y = price), color = "green", alpha = 0.6) +
  geom_line(data = predicted_data, aes(x = sqft, y = price), color = "red", size = 1) +
  geom_line(data = tangent_data, aes(x = sqft, y = price), color = "black", linetype = "dashed", size = 1) +
  geom_point(aes(x = 2000, y = y_2000), color = "black", size = 3) +
  labs(,
       x = "房屋面積（平方英尺）",
       y = "房價（千美元）") +
  theme_minimal()
print(p)

#(f)
# 繪製線性回歸的殘差圖
b <-ggplot(collegetown, aes(x = sqft, y = residuals_linear)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
       x = "房屋面積（平方英尺）",
       y = "殘差") +
  theme_minimal()
print(b)

# 繪製二次回歸的殘差圖
a <- ggplot(collegetown, aes(x = sqft, y = residuals_quadratic)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
       x = "房屋面積（平方英尺）",
       y = "殘差") +
  theme_minimal()
print(a)

#(g)

linear_model <- lm(price ~ sqft, data = collegetown)
quadratic_model <- lm(price ~ poly(sqft, 2, raw=TRUE), data = collegetown)
residuals_linear <- resid(linear_model)
residuals_quadratic <- resid(quadratic_model)
SSE_linear <- sum(residuals_linear^2)
SSE_quadratic <- sum(residuals_quadratic^2)
print(paste("SSE (Linear Model):", SSE_linear))
print(paste("SSE (Quadratic Model):", SSE_quadratic))

if (SSE_quadratic < SSE_linear) {
  print("The quadratic model has a lower SSE, indicating a better fit.")
} else {
  print("The linear model has a lower SSE, indicating a better fit.")
}

