installed.packages("POE5Rdata")
installed.packages("ggplot2")
library(POE5Rdata)
library(ggplot2)

#第一題
#2.17(a)
data("collegetown", package = "POE5Rdata")
print(class(collegetown))
collegetown = as.data.frame(collegetown)


ggplot(collegetown, aes(x = sqft, y = price)) + 
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "房價與房屋面積散點圖",
    x = "房屋面積 (百平方英尺)",
    y = "房價 (千美元)") +
  theme_minimal()

#2.17(b)
model = lm(price ~sqft, data = collegetown)
summary(model)

ggplot(collegetown,aes(x = sqft, y = price))+
  geom_point(color = "blue", alpha = 0.5)+
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  labs(title = "線性回歸: 房價 vs 房屋面積",
       x = "房屋面積 (百平方英尺)",
       y = "房價 (千美元)")+
  theme_minimal()

#2.17(c)(d)
collegetown$sqft2 = collegetown$sqft^2

quad_model = lm(price~sqft2, data = collegetown)
summary(quad_model)

sqft_2000 = 20
mar_eff = coef(quad_model)["sqft2"] * 2 *sqft_2000
print(paste("Marginal Effect at 2000 sqft:", round(mar_eff, 2),"thousands of dollars"))
price_2000 = coef(quad_model)["(Intercept)"] + coef(quad_model)["sqft2"] * sqft_2000^2

slope_tangent = 2*coef(quad_model)["sqft2"] * sqft_2000

x_vals <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 100)
y_tangent <- price_2000 + slope_tangent * (x_vals - sqft_2000)  

tangent_data <- data.frame(sqft = x_vals, price = y_tangent)

ggplot(collegetown, aes(x = sqft, y = price)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ I(x^2), color = "red", se = FALSE) +  
  geom_line(data = tangent_data, aes(x = sqft, y = price), color = "green", linetype = "dashed", size = 1) +  
  geom_point(aes(x = sqft_2000, y = price_2000), color = "black", size = 3) +  
  labs(title = "二次回歸: 房價 vs 房屋面積 (含切線)",
       x = "房屋面積 (百平方英尺)",
       y = "房價 (千美元)") +
  theme_minimal()

#2.17(e)
a1 = coef(quad_model)["(Intercept)"]
a2 = coef(quad_model)["sqft2"]

sqft_2000 = 20
price_2000 = a1 + a2 * sqft_2000^2

marginal_effect_at_2000 = 2 * a2 * sqft_2000

elasticity = marginal_effect_at_2000 * (sqft_2000 / price_2000)

print(paste("Elasticity of PRICE with respect to SQFT at 2000 sqft:", round(elasticity, 4)))

#2.17(f)
linear_residuals = resid(model)
quadratic_residuals = resid(quad_model)

ggplot(data = collegetown, aes(x = sqft, y = linear_residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Residuals of Linear Model vs SQFT",
       x = "SQFT (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

ggplot(data = collegetown, aes(x = sqft, y = quadratic_residuals)) +
  geom_point(color = "red", alpha = 0.5) +
  labs(title = "Residuals of Quadratic Model vs SQFT",
       x = "SQFT (Hundreds of Square Feet)",
       y = "Residuals") +
  theme_minimal()

#2.17(g)
linear_sse <- sum(linear_residuals^2)
quadratic_sse <- sum(quadratic_residuals^2)

print(paste("SSE for Linear Model:", round(linear_sse, 2)))
print(paste("SSE for Quadratic Model:", round(quadratic_sse, 2)))

#第二題
#2.25(a)
data(cex5_small)
foodaway = cex5_small$foodaway

hist(foodaway, breaks = 20, col = "lightblue", main = "Histogram of FOODAWAY",
     xlab = "foodaway (Monthly Expenditure per Person)", border = "black")

summary_stats = summary(foodaway)
mean_foodaway = mean(foodaway, na.rm = TRUE)
median_foodaway = median(foodaway, na.rm = TRUE)
quantiles_foodaway = quantile(foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

summary_stats
cat("Mean:", mean_foodaway, "\n")
cat("Median:", median_foodaway, "\n")
cat("25th percentile:", quantiles_foodaway[1], "\n")
cat("75th percentile:", quantiles_foodaway[2], "\n")

#2.25(b)
library(dplyr)
cex5_small = cex5_small %>% rename_with(tolower)

if (all(c("advanced", "college") %in% names(cex5_small))) {
  
  adv_degree = cex5_small %>% filter(advanced == 1)
  mean_adv = mean(adv_degree$foodaway, na.rm = TRUE)
  median_adv = median(adv_degree$foodaway, na.rm = TRUE)

  college_degree = cex5_small %>% filter(college == 1 & advanced == 0)
  mean_college = mean(college_degree$foodaway, na.rm = TRUE)
  median_college = median(college_degree$foodaway, na.rm = TRUE)
  
  no_degree = cex5_small %>% filter(college == 0 & advanced == 0)
  mean_none = mean(no_degree$foodaway, na.rm = TRUE)
  median_none = median(no_degree$foodaway, na.rm = TRUE)
  
  cat("Mean and Median FOODAWAY:\n")
  cat("Advanced Degree (advanced = 1) - Mean:", mean_adv, " Median:", median_adv, "\n")
  cat("College Degree but No Advanced (college = 1 & advanced = 0) - Mean:", mean_college, " Median:", median_college, "\n")
  cat("No College or Advanced Degree (college = 0 & advanced = 0) - Mean:", mean_none, " Median:", median_none, "\n")
  
} else {
  cat("Error: 'advanced' or 'college' variable not found in dataset.\n")
}

#2.25(c)
library(dplyr)
library(ggplot2)

cex5_small = cex5_small %>% rename_with(tolower)

cex5_small = cex5_small %>% filter(foodaway > 0)

cex5_small = cex5_small %>% mutate(ln_foodaway = log(foodaway))

ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "lightblue", color = "black") +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(FOODAWAY)",
       y = "Frequency") +
  theme_minimal()

summary_ln_foodaway = summary(cex5_small$ln_foodaway)
mean_ln_foodaway = mean(cex5_small$ln_foodaway, na.rm = TRUE)
median_ln_foodaway = median(cex5_small$ln_foodaway, na.rm = TRUE)
quantiles_ln_foodaway = quantile(cex5_small$ln_foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

summary_ln_foodaway
cat("Mean of ln(FOODAWAY):", mean_ln_foodaway, "\n")
cat("Median of ln(FOODAWAY):", median_ln_foodaway, "\n")
cat("25th percentile of ln(FOODAWAY):", quantiles_ln_foodaway[1], "\n")
cat("75th percentile of ln(FOODAWAY):", quantiles_ln_foodaway[2], "\n")

#2.25(d)
library(dplyr)
library(ggplot2)

cex5_small = cex5_small %>% rename_with(tolower)
cex5_small = cex5_small %>% filter(foodaway > 0)
cex5_small = cex5_small %>% mutate(ln_foodaway = log(foodaway))
reg_model = lm(ln_foodaway ~ income, data = cex5_small)

summary(reg_model)

#2.25(e)
ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(alpha = 0.5, color = "blue") +  # 散點圖
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 加入回歸線
  labs(title = "Scatter Plot of ln(FOODAWAY) vs. INCOME",
       x = "Household Income (100 USD)",
       y = "ln(FOODAWAY)") +
  theme_minimal()

#2.25(f)
cex5_small = cex5_small %>% mutate(residuals = residuals(reg_model))

ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5, color = "blue") +  # 散點圖
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 加入 y=0 的虛線
  labs(title = "Residual Plot: Residuals vs. INCOME",
       x = "Household Income (100 USD)",
       y = "Residuals") +
  theme_minimal()


#第三題
#2.28(a)
library(POE5Rdata)
cps5_small = as.data.frame(cps5_small)

summary(data$wage)
summary(data$educ)

par(mfrow=c(1,2))
hist(data$wage, main="Histogram of WAGE", xlab="WAGE", col="lightblue", border="black")
hist(data$educ, main="Histogram of EDUC", xlab="EDUC", col="lightgreen", border="black")

#2.28(b)
linear_model = lm(wage ~ educ, data=cps5_small)
summary(linear_model)

#2.28(c)
residuals = residuals(linear_model)

plot(data$educ, residuals, main="Residuals vs EDUC", xlab="EDUC", ylab="Residuals", 
     pch=19, col="red")
abline(h=0, col="blue", lty=2)

#2.28(d)
male_data = subset(data, female == 0)
female_data = subset(data, female == 1)

male_model = lm(wage ~ educ, data=male_data)
female_model = lm(wage ~ educ, data=female_data)

summary(male_model)
summary(female_model)

black_data = subset(data, black == 1)
white_data = subset(data, black == 0)

black_model = lm(wage ~ educ, data=black_data)
white_model = lm(wage ~ educ, data=white_data)

summary(black_model)
summary(white_model)

#2.28(e)
educ2 = cps5_small$educ^2
quad_modle = lm(cps5_small$wage~educ2)
summary(quad_modle)

educ12 = 12
educ16 = 16
me_edcu12 = 2*coef(quad_modle)[2]*educ12
me_educ16 = 2*coef(quad_modle)[2]*educ16
me_edcu12
me_educ16


#2.28(f)
library(ggplot2)

cps5_small$linear_fit = predict(linear_model)
cps5_small$quad_fit = predict(quad_model)

ggplot(cps5_small, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.5, color = "gray") +  
  geom_line(aes(y = linear_fit), color = "blue", linetype = "dashed", size = 1) +
  geom_line(aes(y = quad_fit), color = "red", size = 1) + 
  labs(title = "Comparison of Linear and Quadratic Fit",
       x = "Years of Education",
       y = "Hourly Wage") +
  theme_minimal()
