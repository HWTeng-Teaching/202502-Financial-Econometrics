remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("collegetown")

#2.17.a
library(ggplot2)
summary(collegetown)
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Scatter Plot of House Price vs. House Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "Sale Price (Thousands of Dollars)") +
  theme_minimal()

#2.17.b
model <- lm(price ~ sqft, data = collegetown) 
summary(model)
ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(title = "House Price vs. House Size with Fitted Regression Line",
       x = "House Size (Hundreds of Square Feet)",
       y = "Sale Price (Thousands of Dollars)") +
  theme_minimal()

coefficients <- coef(model)
intercept <- coefficients[1]
slope <- coefficients[2]
cat("回歸方程式: PRICE = ", round(intercept, 4), " + ", round(slope, 4), " * SQFT\n")

#2.17.c

collegetown$sqft2 <- collegetown$sqft^2 #SQFT 平方項
quad_model <- lm(price ~ sqft2, data = collegetown)
summary(quad_model)
# 提取迴歸係數
alpha1 <- coef(quad_model)[1]
alpha2 <- coef(quad_model)[2]
cat("回歸方程式: PRICE = ", round(alpha1, 4), " + ", round(alpha2, 4), " * SQFT^2\n")

sqft_value <- 20 

marginal_effect <-  2 * alpha2 * sqft_value 
marginal_effect

#2.17.d
price_at_2000 <- alpha1 + alpha2 * sqft_value^2
tangent_line <- data.frame(
  sqft = seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100),
  price = marginal_effect * (seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100) - sqft_value) + price_at_2000
)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.6) 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") + 
  geom_line(data = tangent_line, aes(x = sqft, y = price), color = "green", linetype = "dashed") + 
  annotate("point", x = sqft_value, y = price_at_2000, color = "black", size = 3) +  
  labs(title = "Quadratic Regression with Tangent at 2000 sq ft",
       x = "House Size (Hundreds of Square Feet)",
       y = "Sale Price (Thousands of Dollars)") +
  theme_minimal()

#2.17.e
elasticity <- marginal_effect * (sqft_value / price_at_2000)
elasticity

#2.17.f
collegetown$linear_residuals <- resid(model)
collegetown$quad_residuals <- resid(quad_model)
ggplot(collegetown, aes(x = sqft, y = linear_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Linear Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

ggplot(collegetown, aes(x = sqft, y = quad_residuals)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot for Quadratic Regression",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

#2.17.g
sse_linear <- sum(collegetown$linear_residuals^2)
sse_quad <- sum(collegetown$quad_residuals^2)
sse_linear
sse_quad

#2.25.a
data("cex5_small")
summary(cex5_small)

str(cex5_small$foodaway)
mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)
q1_foodaway <- quantile(cex5_small$foodaway, 0.25, na.rm = TRUE)
q3_foodaway <- quantile(cex5_small$foodaway, 0.75, na.rm = TRUE)

cat("平均值 (Mean):", mean_foodaway, "\n")
cat("中位數 (Median):", median_foodaway, "\n")
cat("25th 百分位數 (Q1):", q1_foodaway, "\n")
cat("75th 百分位數 (Q3):", q3_foodaway, "\n")

library(ggplot2)

ggplot(cex5_small, aes(x = foodaway, y = after_stat(count) / sum(after_stat(count)) * 100)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_foodaway), color = "red", linetype = "dashed", linewidth = 1) +  
  geom_vline(aes(xintercept = median_foodaway), color = "green", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of FOODAWAY Expenditure",
       x = "FOODAWAY (Dollars per Month per Person)",
       y = "Percent") +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  theme_minimal()

#2.25.b
library(dplyr)
colnames(cex5_small)
cex5_small <- cex5_small %>%
  mutate(
    advanced = as.numeric(as.character(advanced)),
    college = as.numeric(as.character(college))
  )

# 創建Education變數
cex5_small <- cex5_small %>%
  mutate(
    Education = case_when(
      advanced == 1 ~ "Advanced Degree",
      college == 1 ~ "College Degree",
      TRUE ~ "No Degree"
    )
  )

table(cex5_small$Education)
summary_stats <- cex5_small %>%
  group_by(Education) %>%
  summarise(
    Mean_FOODAWAY = mean(foodaway, na.rm = TRUE),
    Median_FOODAWAY = median(foodaway, na.rm = TRUE)
  )
print(summary_stats)
summary_stats <- cex5_small %>%
  group_by(Education) %>%
  summarise(
    Mean_FOODAWAY = mean(foodaway, na.rm = TRUE),
    Median_FOODAWAY = median(foodaway, na.rm = TRUE)
  )

print(summary_stats)

#2.25.c
cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
sum(!is.na(cex5_small$ln_foodaway))
summary(cex5_small$ln_foodaway)

hist_data_log <- hist(cex5_small$log_foodaway, 
                      main = "Histogram of ln(FOODAWAY)", 
                      xlab = "ln(FOODAWAY)", 
                      ylab = "Frequency", 
                      col = "lightblue", 
                      border = "black",
                      freq = TRUE,  
                      breaks = 30)  

#2.25.d
model <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model)

#2.25.e
ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatter Plot of ln(FOODAWAY) vs. INCOME",
       x = "Household Income (in $100)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

#2.25.f
cex5_small <- cex5_small %>%
  mutate(residuals = residuals(model)) 

# 繪製殘差 vs. INCOME
ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  labs(title = "Residuals vs. INCOME",
       x = "Household Income (in $100)",
       y = "Residuals") +
  theme_minimal()

#2.28.a
summary(cps5_small$wage)
summary(cps5_small$educ)

ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of WAGE",
       x = "Hourly Wage ($)",
       y = "Frequency") +
  theme_minimal()

ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of EDUC",
       x = "Years of Education",
       y = "Frequency") +
  theme_minimal()

#2.28.b
wage_model <- lm(wage ~ educ, data = cps5_small)
summary(wage_model)

#2.28.c
cps5_small <- cps5_small %>%
  mutate(residuals = residuals(wage_model)) 

# 繪製殘差 vs. EDUC
ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  labs(title = "Residuals vs. EDUC",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()
#2.28.d
summary(cps5_small)
data_male <- subset(cps5_small, female == 0)
data_female <- subset(cps5_small, female == 1)
data_black <- subset(cps5_small, black == 1)
data_white <- subset(cps5_small, black == 0)

model_male <- lm(wage ~ educ, data = data_male)
model_female <- lm(wage ~ educ , data = data_female)
model_black <- lm(wage ~ educ , data = data_black)
model_white <- lm(wage ~ educ , data = data_white)

summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)

get_equation <- function(model, group) {
  intercept <- round(coef(model)[1], 4)
  educ_coef <- round(coef(model)[2], 4)
  
  cat(group, "回歸方程:\n")
  cat("wage =", intercept, "+", educ_coef, "* educ\n\n")
}

get_equation(model_male, "男性")
get_equation(model_female, "女性")
get_equation(model_black, "黑人")
get_equation(model_white, "白人")

#2.28.e
# Quadratic Regression
cps5_small$educ2 <- cps5_small$educ^2 
quad_model <- lm(wage ~ educ2, data = cps5_small)
summary(quad_model)

# 提取回歸係數
alpha_2 <- coef(quad_model)["educ2"]

# 邊際影響
marginal_effect_12 <- 2 * alpha_2 * 12
marginal_effect_16 <- 2 * alpha_2 * 16

cat("邊際影響（EDUC = 12）:", marginal_effect_12, "\n")
cat("邊際影響（EDUC = 16）:", marginal_effect_16, "\n")

#2.28.f
cps5_small$linear_fit <- predict(wage_model, newdata = cps5_small)
cps5_small$quadratic_fit <- predict(quad_model, newdata = cps5_small)
ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5, color = "gray") + 
  geom_line(aes(y = linear_fit), color = "blue", linetype = "dashed", size = 1.2) + 
  geom_line(aes(y = quadratic_fit), color = "red", size = 1.2) + 
  labs(title = "Comparison of Linear and Quadratic Regression Models",
       x = "Years of Education",
       y = "Wage",
       caption = "Blue Dashed: Linear Model, Red: Quadratic Model") +
  theme_minimal()
