#2.17(a)

library(POE5Rdata)
data(collegetown)
summary(collegetown)

library(ggplot2)
ggplot(collegetown, aes(x = sqft, y = price )) +
  geom_point(color = "blue") +
  labs(title = "Scatter Plot of House Price vs. House Size",
       x = "House Size (hundreds of sqft)", 
       y = "Price (thousands of dollars)") +
  theme_minimal()

#2.17(b)

model <- lm(price ~ sqft, data = collegetown)
summary(model)
intercept <- coef(model)[1]  # 截距
slope <- coef(model)[2]      # 斜率
# 顯示回歸方程式
cat("regression model: PRICE = ", round(intercept, 4), " + ", round(slope, 4), " * SQFT\n")

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue") +  # 繪製散點圖
  geom_smooth(method = "lm",se = FALSE, color = "red") +  # 繪製迴歸線
  labs(title = "Regression: House Price vs. Size",
       x = "House Size (hundreds of sq ft)", 
       y = "Price (thousands of dollars)") +
  theme_minimal()

#2.17(c)

collegetown$sqft2 <- collegetown$sqft^2
model_quadratic <- lm(price ~ sqft2, data = collegetown)
summary(model_quadratic)
alpha1 <- coef(model_quadratic)[1]  # SQFT 的係數
alpha2 <- coef(model_quadratic)[2]  # SQFT^2 的係數
# 計算邊際效應（假設 SQFT = 20）
SQFT_value <- 20
marginal_effect <- 2 * alpha2 * SQFT_value
cat("Margin effect (when SQFT = 2000) : ", marginal_effect, "\n")

#2.17(d)

price_at_2000 <- alpha1 + alpha2 * SQFT_value^2
tangent_line <- data.frame(
  sqft = seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100),
  price = marginal_effect * (seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100) - SQFT_value) + price_at_2000)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  # 原始資料的散點圖
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +  # 二次回歸曲線
  geom_line(data = tangent_line, aes(x = sqft, y = price), color = "green") +  # 切線
  annotate("point", x = SQFT_value, y = price_at_2000, color = "black", size = 3) +  # 標註2000平方英尺處的點
  labs(title = "Quadratic Regression with Tangent at 2000 sq ft",
       x = "House Size (Hundreds of Square Feet)",
       y = "Sale Price (Thousands of Dollars)") +
  theme_minimal()

#2.17(d)

elasticity <- marginal_effect * SQFT_value / price_at_2000
elasticity

#2.17(f)

# 計算線性回歸模型的誤差
pred_linear <- predict(model)
residuals_linear <- collegetown$price - pred_linear
# 計算二次回歸模型的誤差
pred_quadratic <- predict(model_quadratic)
residuals_quadratic <- collegetown$price - pred_quadratic

# 繪製線性回歸模型的誤差圖（對應於SQFT）
ggplot(data = collegetown, aes(x = sqft, y = residuals_linear)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_hline(yintercept = 0, color = "red") +  # 添加 y=0 的線
  labs(title = "Residual Plot for Linear Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

# 繪製二次回歸模型的誤差圖（對應於SQFT）
ggplot(data = collegetown, aes(x = sqft, y = residuals_quadratic)) +
  geom_point(color = "green", alpha = 0.6) +  # 散點圖
  geom_hline(yintercept = 0, color = "red") +  # 添加 y=0 的線
  labs(title = "Residual Plot for quadratic Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

#2.17(g)

# 計算線性模型的 SSE
SSE_linear <- sum(residuals_linear^2)
# 計算二次回歸模型的 SSE
SSE_quadratic <- sum(residuals_quadratic^2)
# 輸出兩個模型的 SSE
cat("SSE of linear model: ", SSE_linear, "\n")
cat("SSE of quadratic model: ", SSE_quadratic, "\n")

#2.25(a)

library(POE5Rdata)  
data(cex5_small)     
library(ggplot2)
summary(cex5_small)

ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of FOODAWAY Expenditure",
       x = "Food Away from Home Expenditure ($)",
       y = "Frequency") +
  theme_minimal()

summary(cex5_small$foodaway)

#2.25(b)
advanced_households <- subset(cex5_small, advanced == 1)
college_households <- subset(cex5_small, college == 1 & advanced == 0)
no_degree_households <- subset(cex5_small, advanced == 0 & college == 0)

mean_advanced <- mean(advanced_households$foodaway, na.rm = TRUE)
median_advanced <- median(advanced_households$foodaway, na.rm = TRUE)
mean_college <- mean(college_households$foodaway, na.rm = TRUE)
median_college <- median(college_households$foodaway, na.rm = TRUE)
mean_no_degree <- mean(no_degree_households$foodaway, na.rm = TRUE)
median_no_degree <- median(no_degree_households$foodaway, na.rm = TRUE)
count_advanced <- nrow(advanced_households)
count_college <- nrow(college_households)
count_no_degree <- nrow(no_degree_households)

install.packages("dplyr")
install.packages("knitr")
library(dplyr)
library(knitr)

result_table <- data.frame(
  Group = c("Advanced Degree", "College Degree", "No Degree"),
  Numbers = c(count_advanced, count_college, count_no_degree),
  Mean_Foodaway = c(mean_advanced, mean_college, mean_no_degree),
  Median_Foodaway = c(median_advanced, median_college, median_no_degree)
)

library(knitr)
kable(result_table, caption = "Summary of FOODAWAY by Education Level")

#2.25(c)

library(dplyr)
library(ggplot2)

cex5_small <- cex5_small %>% mutate(ln_foodaway = ifelse(foodaway > 0, log(foodaway), NA))

# 繪製 ln(FOODAWAY) 直方圖
ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)", x = "ln(FOODAWAY)", y = "Frequency") +
  theme_minimal()

summary_stats <- cex5_small %>%
  summarise(
    mean_ln = mean(ln_foodaway, na.rm = TRUE),
    median_ln = median(ln_foodaway, na.rm = TRUE),
    q25_ln = quantile(ln_foodaway, 0.25, na.rm = TRUE),
    q75_ln = quantile(ln_foodaway, 0.75, na.rm = TRUE),
    Numbers_ln = sum(!is.na(ln_foodaway)),
    Numbers_original = n()
  )

print(summary_stats)

#2.25(d)

model_ln_foodaway <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model_ln_foodaway)
beta1 <- coef(model_ln_foodaway)[1] 
beta2 <- coef(model_ln_foodaway)[2]  
cat("Regression equation: ln(FOODAWAY) =", round(beta1, 4), "+", round(beta2, 4), "* INCOME\n")

#2.25(e)

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Regression line
  labs(title = "Log of FOODAWAY vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

#2.25(f)

cex5_small_clean <- na.omit(cex5_small[, c("ln_foodaway", "income")])
model_ln_foodaway <- lm(ln_foodaway ~ income, data = cex5_small_clean)
cex5_small_clean$residuals <- residuals(model_ln_foodaway)

ggplot(cex5_small_clean, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 參考線
  labs(title = "Residuals vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "Residuals") +
  theme_minimal()

#2.28(a)

data(cps5_small)  
summary(cps5_small)

ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of WAGE", x = "Hourly Wage Rate ($)", y = "Frequency") +
  theme_minimal()
summary(cps5_small$wage)


ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of EDUC", x = "Years of Education", y = "Frequency") +
  theme_minimal()
summary(cps5_small$educ)

#2.28(b)

model_wage_educ <- lm(wage ~ educ, data = cps5_small)
summary(model_wage_educ)

beta1 <- coef(model_wage_educ)[1]  
beta2 <- coef(model_wage_educ)[2]  
cat("Estimated regression equation: WAGE =", round(beta1, 4), "+", round(beta2, 4), "* EDUC\n")

#2.28(e)

cps5_small$residuals <- residuals(model_wage_educ)

ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Reference line at zero
  labs(title = "Residuals vs. EDUC",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()

#2.28(d)

model_male <- lm(wage ~ educ, data = cps5_small, subset = (female == 0))
model_female <- lm(wage ~ educ, data = cps5_small, subset = (female == 1))
model_black <- lm(wage ~ educ, data = cps5_small, subset = (black == 1))
model_white <- lm(wage ~ educ, data = cps5_small, subset = (black == 0))
summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

coef_male <- coef(model_male)
coef_female <- coef(model_female)
coef_black <- coef(model_black)
coef_white <- coef(model_white)

comparison_table <- data.frame(
  Group = c("Male", "Female", "Black", "White"),
  Intercept = c(coef_male[1], coef_female[1], coef_black[1], coef_white[1]),
  Slope = c(coef_male[2], coef_female[2], coef_black[2], coef_white[2])
)

cat("Male Model: WAGE =", round(coef_male[1], 4), "+", round(coef_male[2], 4), "* EDUC\n")
cat("Female Model: WAGE =", round(coef_female[1], 4), "+", round(coef_female[2], 4), "* EDUC\n")
cat("Black Model: WAGE =", round(coef_black[1], 4), "+", round(coef_black[2], 4), "* EDUC\n")
cat("White Model: WAGE =", round(coef_white[1], 4), "+", round(coef_white[2], 4), "* EDUC\n")

#2.28(e)

cps5_small <- cps5_small %>% mutate(EDUC2 = educ^2)
model_quadratic <- lm(wage ~ EDUC2, data = cps5_small)
summary(model_quadratic)
alpha1 <- coef(model_quadratic)[1]  
alpha2 <- coef(model_quadratic)[2]  
cat("Quadratic Regression Equation: WAGE =", round(alpha1, 4), "+", round(alpha2, 4), "* EDUC^2\n")

EDUC_12 <- 12
EDUC_16 <- 16
marginal_effect_12 <- 2 * alpha2 * EDUC_12
marginal_effect_16 <- 2 * alpha2 * EDUC_16
cat("Marginal Effect when EDUC = 12: ", round(marginal_effect_12, 4), "\n")
cat("Marginal Effect when EDUC = 16: ", round(marginal_effect_16, 4), "\n")

#2.28(f)
fitted_linear <- predict(model_wage_educ, newdata = cps5_small)
fitted_quadratic <- predict(model_quadratic, newdata = cps5_small)

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_line(aes(y = fitted_linear), color = "red", linetype = "solid", size = 1) +  
  geom_line(aes(y = fitted_quadratic), color = "orange", size = 1) +  
  labs(title = "Comparison of Linear and Quadratic Models",
       x = "Years of Education (EDUC)",
       y = "Wage (WAGE)") +
  theme_minimal() +
  theme(legend.position = "none")
