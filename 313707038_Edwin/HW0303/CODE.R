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

##CH2 2.25
#(a)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("POE5Rdata")) remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(ggplot2)
library(POE5Rdata)
data("cex5_small")

if (all(is.na(cex5_small$foodaway))) {
  stop("錯誤：foodaway 無法轉換為數值型，可能包含非數字值。")
}

cex5_small <- cex5_small[!is.na(cex5_small$foodaway), ]

p1 <- ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.6, color = "black") +
  labs(title = "(a) Histogram of foodaway", x = "foodaway ($ per person per month)", y = "Count") +
  theme_minimal()
print(p1)

mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)
percentile_25 <- quantile(cex5_small$foodaway, 0.25, na.rm = TRUE)
percentile_75 <- quantile(cex5_small$foodaway, 0.75, na.rm = TRUE)

print(c("(a) Mean" = mean_foodaway, "Median" = median_foodaway, "25th Percentile" = percentile_25, "75th Percentile" = percentile_75))

#(b)

if ("college" %in% colnames(cex5_small) & "advanced" %in% colnames(cex5_small) & "foodaway" %in% colnames(cex5_small)) {
  results <- data.frame(Education = character(), Mean_Foodaway = numeric(), Median_Foodaway = numeric(), stringsAsFactors = FALSE)
  subset_advanced <- subset(cex5_small, advanced == 1)
  if (nrow(subset_advanced) > 0) {
    mean_advanced <- mean(subset_advanced$foodaway, na.rm = TRUE)
    median_advanced <- median(subset_advanced$foodaway, na.rm = TRUE)
    results <- rbind(results, data.frame(Education = "Advanced Degree", Mean_Foodaway = round(mean_advanced, 2), Median_Foodaway = round(median_advanced, 2)))
  }
  subset_college <- subset(cex5_small, college == 1 & advanced == 0)
  if (nrow(subset_college) > 0) {
    mean_college <- mean(subset_college$foodaway, na.rm = TRUE)
    median_college <- median(subset_college$foodaway, na.rm = TRUE)
    results <- rbind(results, data.frame(Education = "College Degree", Mean_Foodaway = round(mean_college, 2), Median_Foodaway = round(median_college, 2)))
  }
  subset_no_college <- subset(cex5_small, college == 0 & advanced == 0)
  if (nrow(subset_no_college) > 0) {
    mean_no_college <- mean(subset_no_college$foodaway, na.rm = TRUE)
    median_no_college <- median(subset_no_college$foodaway, na.rm = TRUE)
    results <- rbind(results, data.frame(Education = "No College Degree", Mean_Foodaway = round(mean_no_college, 2), Median_Foodaway = round(median_no_college, 2)))
  }
  print("按教育程度分類的 foodaway 均值與中位數:")
  print(results)
  
} else {
  print("college、advanced 或 foodaway 變數不存在")
}

#(c)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("POE5Rdata")) remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(ggplot2)
library(POE5Rdata)
data("cex5_small")
colnames(cex5_small)[colnames(cex5_small) == "FOODAWAY"] <- "foodaway"
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
p1 <- ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.5, fill = "red", alpha = 0.6, color = "black") +
  labs( x = "ln(foodaway)", y = "Count") +
  theme_minimal()
print(p1)
mean_ln_foodaway <- mean(cex5_small$ln_foodaway, na.rm = TRUE)
median_ln_foodaway <- median(cex5_small$ln_foodaway, na.rm = TRUE)
percentile_25_ln <- quantile(cex5_small$ln_foodaway, 0.25, na.rm = TRUE)
percentile_75_ln <- quantile(cex5_small$ln_foodaway, 0.75, na.rm = TRUE)

print(c("(c) Mean ln(foodaway)" = mean_ln_foodaway, 
        "Median ln(foodaway)" = median_ln_foodaway, 
        "25th Percentile" = percentile_25_ln, 
        "75th Percentile" = percentile_75_ln))

#(d)
cex5_small <- subset(cex5_small, foodaway > 0 & !is.na(foodaway))  # 移除 foodaway = 0 和 NA
cex5_small$ln_foodaway <- log(cex5_small$foodaway)  # 轉換為 ln(foodaway)

cex5_small <- subset(cex5_small, !is.na(income))

cex5_small <- subset(cex5_small, !is.na(ln_foodaway) & is.finite(ln_foodaway))

regression_model <- lm(ln_foodaway ~ income, data = cex5_small)
print(summary(regression_model))

#(e)

colnames(cex5_small)[colnames(cex5_small) == "FOODAWAY"] <- "foodaway"
cex5_small <- subset(cex5_small, foodaway > 0 & !is.na(foodaway))
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
cex5_small <- subset(cex5_small, !is.na(income))
regression_model <- lm(ln_foodaway ~ income, data = cex5_small)

p <- ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(
       x = "Household Monthly Income ($100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

print(p)  

#(f)
cex5_small <- subset(cex5_small, foodaway > 0 & !is.na(foodaway))

cex5_small$ln_foodaway <- log(cex5_small$foodaway)

cex5_small <- subset(cex5_small, !is.na(income))

regression_model <- lm(ln_foodaway ~ income, data = cex5_small)

cex5_small$residuals <- resid(regression_model)
p <- ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(color = "purple", alpha = 0.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(
       x = "Household Monthly Income ($100 units)",
       y = "Residuals") +
  theme_minimal()
print(p)  
