library(POE5Rdata)
library(ggplot2)
library(dplyr)

##2.17(a)
data("collegetown")
y <- collegetown[[1]]
x <- collegetown[[2]]

ggplot(collegetown, aes(x, y )) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Scatter Plot of House Price vs. House Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal()

## (b)
model <- lm(y ~ x, data = collegetown)
summary(model)
# 繪製散點圖和擬合回歸線
ggplot(collegetown, aes(x , y )) +
  geom_point(color = "blue", alpha = 0.5) +  # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 擬合的回歸線
  labs(title = "Linear Regression: House Price vs. House Size",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal()

## This shows that by controlling other variables, one unit increase in house size will increase 13,4029 thousands of dollars in price; also if house size is 0, then the expected price is  -115.4236 thousands of dollars.


##(c)
x2 <- x^2
model_quadratic <- lm(y ~ x + x2, data = collegetown)
summary(model_quadratic)

alpha2 <- coef(model_quadratic)[2]
alpha3 <- coef(model_quadratic)[3]

SQFT_value <- 20  
marginal_effect <- alpha2 + 2 * alpha3 * SQFT_value

marginal_effect
m <- marginal_effect*1
## This shows that an additional 100 square feet has a marginal effect of 6.45

##(d)
alpha1 <- coef(model_quadratic)[1]
alpha2 <- coef(model_quadratic)[2]
alpha3 <- coef(model_quadratic)[3]


sqft_range <- seq(min(x), max(x), length.out = 500)


fitted_curve <- alpha1 + alpha2 * sqft_range + alpha3 * sqft_range^2

SQFT_value <- 20
slope_tangent <- alpha2 + 2 * alpha3 * SQFT_value
price_at_2000 <- alpha1 + alpha2 * SQFT_value + alpha3 * SQFT_value^2

tangent_line <- slope_tangent * (sqft_range - SQFT_value) + price_at_2000


ggplot(collegetown, aes(x , y )) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_line(aes(x = sqft_range, y = fitted_curve), color = "red") +  
  geom_line(aes(x = sqft_range, y = tangent_line), color = "black", linetype = "dashed") +  
  labs(title = "Quadratic Regression: House Price vs. House Size with Tangent Line",
       x = "House Size (Hundreds of Square Feet)",
       y = "House Price (Thousands of Dollars)") +
  theme_minimal() +
  annotate("text", x = 20, y = price_at_2000 + 2, label = "Tangent Line", color = "black")

##(e)
SQFT_value <- 20  
price_at_2000 <- alpha1 + alpha2 * SQFT_value + alpha3 * SQFT_value^2
marginal_effect <- alpha2 + 2 * alpha3 * SQFT_value  


elasticity <- (marginal_effect * SQFT_value) / price_at_2000

cat("房價對於房屋面積的彈性在 2000 平方英尺時為:", elasticity, "\n")

##(f)
collegetown$residuals <- resid(model) 
collegetown$residuals_quadratic <- resid(model_quadratic) 


p1 <- ggplot(collegetown, aes(x, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression Residuals vs. SQFT",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()


p2 <- ggplot(collegetown, aes(x , y = residuals_quadratic)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Quadratic Regression Residuals vs. SQFT",
       x = "House Size (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()


print(p1)
print(p2)
qqnorm(collegetown$residuals)
qqline(collegetown$residuals, col = "red")

qqnorm(collegetown$residuals_quadratic)
qqline(collegetown$residuals_quadratic, col = "red")
## It seems like neither of them violate the assumptions, except that it seems like there are either heavy tails or outliers.

##(g)
SSE_linear <- sum(resid(model)^2)

SSE_quadratic <- sum(resid(model_quadratic)^2)

cat("SSE for Linear Model:", SSE_linear, "\n")
cat("SSE for Quadratic Model:", SSE_quadratic, "\n")
## This shows that the quadratic model has lower SSE and hence show that the sum of square residuals are smaller and hence fits better compared to the linera model.

##2.25(a)
data(cex5_small)
f <- cex5_small[[7]]
ggplot(cex5_small, aes(f)) +
  geom_histogram(binwidth = 10, color = "black", fill = "lightblue", alpha = 0.7) +
  labs(title = "Histogram of FOODAWAY",
       x = "Food Away from Home Expenditure (per person, per month)",
       y = "Frequency") +
  theme_minimal()


summary_stats <- summary(f)


mean_foodaway <- mean(f, na.rm = TRUE)
median_foodaway <- median(f, na.rm = TRUE)
q1_foodaway <- quantile(f, 0.25, na.rm = TRUE)  
q3_foodaway <- quantile(f, 0.75, na.rm = TRUE)  

cat("Mean FOODAWAY:", mean_foodaway, "\n")
cat("Median FOODAWAY:", median_foodaway, "\n")
cat("25th Percentile (Q1):", q1_foodaway, "\n")
cat("75th Percentile (Q3):", q3_foodaway, "\n")

##(b)
cex5_small <- cex5_small %>%
  mutate(education_group = case_when(
    advanced == 1 ~ "advanced degree",
    college == 1 & advanced == 0 ~ "college degree",
    college == 0 & advanced == 0 ~ "no college/advanced degree"
  ))


foodaway_stats <- cex5_small %>%
  group_by(education_group) %>%
  summarise(
    count = n(),
    mean_foodaway = mean(foodaway, na.rm = TRUE),
    median_foodaway = median(foodaway, na.rm = TRUE)
  )

print(foodaway_stats)

##(c)
cex5_small <- cex5_small %>%
  mutate(ln_foodaway = ifelse(foodaway > 0, log(foodaway), NA))

ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(color = "black", fill = "blue", bins = 30) +
  labs(title = "Histogram of ln(FOODAWAY)", x = "ln(FOODAWAY)", y = "Count") +
  theme_minimal()

summary_stats <- cex5_small %>%
  summarise(
    count_ln = sum(!is.na(ln_foodaway)),
    mean_ln = mean(ln_foodaway, na.rm = TRUE),
    median_ln = median(ln_foodaway, na.rm = TRUE),
    sd_ln = sd(ln_foodaway, na.rm = TRUE),
    min_ln = min(ln_foodaway, na.rm = TRUE),
    max_ln = max(ln_foodaway, na.rm = TRUE),
    quantile_25 = quantile(ln_foodaway, 0.25, na.rm = TRUE),
    quantile_75 = quantile(ln_foodaway, 0.75, na.rm = TRUE)
  )

print(summary_stats)
## There are less observations because there are 178 observations =0, making ln(o) undefined.

##(d)
model_ln<- lm(ln_foodaway ~ income, data = cex5_small)
summary(model_ln)
## This shows that a 100$ increase in income will increase around 0.69% increase in foodaway.

##(e)
ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(alpha = 0.5, color = "blue") +   # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(title = "ln(FOODAWAY) vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

##(f)
cex5_small_clean <- cex5_small %>%
  filter(!is.na(ln_foodaway) & !is.na(income)) 

model_ln <- lm(ln_foodaway ~ income, data = cex5_small_clean)

cex5_small_clean <- cex5_small_clean %>%
  mutate(residuals = resid(model_ln))  

ggplot(cex5_small_clean, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  
  labs(title = "Residuals vs. INCOME",
       x = "Household Monthly Income ($100 units)",
       y = "Residuals") +
  theme_minimal()
## They seem completely random.

##2.28(a)
data(cps5_small)

summary_stats <- cps5_small %>%
  summarise(
    count_wage = sum(!is.na(wage)),
    mean_wage = mean(wage, na.rm = TRUE),
    median_wage = median(wage, na.rm = TRUE),
    sd_wage = sd(wage, na.rm = TRUE),
    min_wage = min(wage, na.rm = TRUE),
    max_wage = max(wage, na.rm = TRUE),
    quantile_25_wage = quantile(wage, 0.25, na.rm = TRUE),
    quantile_75_wage = quantile(wage, 0.75, na.rm = TRUE),
    
    count_educ = sum(!is.na(educ)),
    mean_educ = mean(educ, na.rm = TRUE),
    median_educ = median(educ, na.rm = TRUE),
    sd_educ = sd(educ, na.rm = TRUE),
    min_educ = min(educ, na.rm = TRUE),
    max_educ = max(educ, na.rm = TRUE),
    quantile_25_educ = quantile(educ, 0.25, na.rm = TRUE),
    quantile_75_educ = quantile(educ, 0.75, na.rm = TRUE)
  )

print(summary_stats)

ggplot(cps5_small, aes(x = wage)) +
  geom_histogram(color = "black", fill = "blue", bins = 30) +
  labs(title = "Histogram of WAGE", x = "WAGE", y = "Count") +
  theme_minimal()
## This gives a right skewed distribution.
ggplot(cps5_small, aes(x = educ)) +
  geom_histogram(color = "black", fill = "green", bins = 20) +
  labs(title = "Histogram of EDUC", x = "Years of Education", y = "Count") +
  theme_minimal()
## This gives a slightly left skewed distribution.

##(b)

model_wage <- lm(wage ~ educ, data = cps5_small)
summary(model_wage)
## This shows that as education increases by 1, the estimated wage increase by 2.3968, and if education = 0, then the expected wage is -10.4.

##(c)
residuals_wage <- resid(model_wage)
cps5_small$residuals <- residuals_wage

ggplot(cps5_small, aes(x = educ, y = residuals)) +
  geom_point(alpha = 0.5) +  
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +  
  labs(title = "Residuals vs. Education",
       x = "Years of Education",
       y = "Residuals") +
  theme_minimal()
## We see that it seems  like the residuals have positive relation with education, which violates the assumptions that residuals and x should be uncorrelated.

##(d)
model_male <- lm(wage ~ educ, data = cps5_small, subset = (female ==  0))
model_female <- lm(wage ~ educ, data = cps5_small, subset = (female ==  1))
model_black <- lm(wage ~ educ, data = cps5_small, subset = (black == 1))
model_white <- lm(wage ~ educ, data = cps5_small, subset = (black == 0))

# Display regression results
summary(model_male)
summary(model_female)
summary(model_black)
summary(model_white)
## We see that it seems like female benefits more from education comparing to male, similarly, white benefits more from education comparing to black.

##(e)
model_quad <- lm(wage ~ I(educ^2), data = cps5_small)

summary(model_quad)
## This shows that holding other constant, if education increases by 1, then wage will increase by 0.089*1^2.
alpha_2 <- coef(model_quad)[2]

marginal_12 <- 2 * alpha_2 * 12
marginal_16 <- 2 * alpha_2 * 16

cat("Marginal effect at 12 years of education:", marginal_12, "\n")
cat("Marginal effect at 16 years of education:", marginal_16, "\n")
## Where the marginal effect of the linear regression model is simply the slope 2.3968

##(f)
cps5_small$pred_linear <- predict(model_wage)
cps5_small$pred_quad <- predict(model_quad)

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.4, color = "gray") + 
  geom_line(aes(y = pred_linear), color = "blue", linewidth = 1, linetype = "dashed") +  
  geom_line(aes(y = pred_quad), color = "red", linewidth = 1) + 
  labs(title = "Linear vs. Quadratic Model: Wage vs. Education",
       x = "Years of Education",
       y = "Wage") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Linear Model", "Quadratic Model")) +
  theme(legend.position = "bottom")
## This seems like the quadratic model fits the best.
