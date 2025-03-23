#a.

# 安裝並加載數據集
install.packages("remotes")  
remotes::install_github("ccolonescu/POE5Rdata")  
library(POE5Rdata)

# 加載數據
data(cex5_small)
df <- cex5_small

# 查看數據結構
str(df)

# 檢查前幾行數據
head(df)

# 計算統計描述
summary_stats <- data.frame(
  Variable = c("FOOD", "INCOME"),
  Mean = c(mean(df$food), mean(df$income)),
  Median = c(median(df$food), median(df$income)),
  Min = c(min(df$food), min(df$income)),
  Max = c(max(df$food), max(df$income)),
  Std_Dev = c(sd(df$food), sd(df$income))
)

# 顯示統計結果
print(summary_stats)

# 加載繪圖庫
install.packages("ggplot2")  
library(ggplot2)

# FOOD 直方圖
ggplot(df, aes(x = food)) +
  geom_histogram(color="black", fill="green2", bins=30, alpha=0.7) +
  geom_vline(aes(xintercept = mean(food)), color="tan3", linetype="dashed") +
  geom_vline(aes(xintercept = median(food)), color="blue", linetype="dotted") +
  labs(title="Histogram of FOOD", x="FOOD Expenditure", y="Count")

# INCOME 直方圖
ggplot(df, aes(x = income)) +
  geom_histogram(color="black", fill="green2", bins=30, alpha=0.7) +
  geom_vline(aes(xintercept = mean(income)), color="tan3", linetype="dashed") +
  geom_vline(aes(xintercept = median(income)), color="blue", linetype="dotted") +
  labs(title="Histogram of INCOME", x="Household Income", y="Count")

# 安裝並加載 tseries 套件
install.packages("tseries")
library(tseries)

# 進行 Jarque-Bera 檢驗
jb_food <- jarque.bera.test(df$food)
jb_income <- jarque.bera.test(df$income)

# 顯示結果
print(jb_food)
print(jb_income)



#b.

# 建立線性回歸模型
model_linear <- lm(food ~ income, data = df)

# 顯示回歸結果
summary(model_linear)

# 計算 95% 信賴區間
confint(model_linear, level = 0.95)

library(ggplot2)

# Scatter plot of FOOD vs. INCOME with regression line
ggplot(df, aes(x = income, y = food)) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "tan3", se = TRUE) +  # Regression line
  labs(title = "Scatter Plot of FOOD vs. INCOME",
       x = "Household Income",
       y = "Food Expenditure") +
  theme_minimal()

#c.

# Extract residuals
df$residuals <- residuals(model_linear)

# Plot residuals vs. income
ggplot(df, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "tan4", linetype = "dashed") +
  labs(title = "Residuals vs. Income",
       x = "Household Income",
       y = "Residuals") +
  theme_minimal()

#Histogram of Residuals
ggplot(df, aes(x = residuals)) +
  geom_histogram(color="black", fill="green3", bins=30, alpha=0.7) +
  geom_vline(aes(xintercept = mean(residuals)), color="brown", linetype="dashed") +
  labs(title="Histogram of Residuals", x="Residuals", y="Count") +
  theme_minimal()

#Jarque–Bera Test for Normality

install.packages("tseries")  # Install package if not already installed
library(tseries)

# Perform Jarque–Bera test
jb_test_residuals <- jarque.bera.test(df$residuals)

# Display test results
print(jb_test_residuals)

#d.
# Define income values for elasticity calculation
income_values <- data.frame(income = c(19, 65, 160))

# Predict food expenditure at these income levels
predicted_food <- predict(model_linear, newdata = income_values)

# Compute elasticity
elasticity <- 0.3587 * (income_values$income / predicted_food)

# Display results
elasticity_df <- data.frame(income_values$income, predicted_food, elasticity)
colnames(elasticity_df) <- c("Income", "Predicted FOOD", "Elasticity")
print(elasticity_df)

# Lower and upper bound for elasticity
elasticity_lower <- 0.2619 * (income_values$income / predicted_food)
elasticity_upper <- 0.4555 * (income_values$income / predicted_food)

# Add confidence intervals to the table
elasticity_df$Lower_95CI <- elasticity_lower
elasticity_df$Upper_95CI <- elasticity_upper

# Display final table
print(elasticity_df)

#e.

# Log-log model estimation
model_loglog <- lm(log(food) ~ log(income), data = df)

# Display regression summary
summary(model_loglog)

# Scatter plot of log(FOOD) vs. log(INCOME)
ggplot(df, aes(x = log(income), y = log(food))) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "tan3", se = TRUE) +  # Regression line
  labs(title = "Scatter Plot of log(FOOD) vs. log(INCOME)",
       x = "log(Household Income)",
       y = "log(Food Expenditure)") +
  theme_minimal()


#f.
# Extract elasticity estimate (coefficient from log-log model)
elasticity_loglog <- coef(model_loglog)["log(income)"]
print(elasticity_loglog)

# Compute 95% confidence interval for elasticity
confint(model_loglog, level = 0.95)


#g.

# Extract residuals from the log-log model
df$residuals_loglog <- residuals(model_loglog)

# Plot residuals vs. log(INCOME)
ggplot(df, aes(x = log(income), y = residuals_loglog)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "green4", linetype = "dashed") +
  labs(title = "Residuals vs. log(INCOME)",
       x = "log(Household Income)",
       y = "Residuals") +
  theme_minimal()

# Histogram of Residuals
ggplot(df, aes(x = residuals_loglog)) +
  geom_histogram(color="black", fill="green4", bins=30, alpha=0.7) +
  geom_vline(aes(xintercept = mean(residuals_loglog)), color="tan4", linetype="dashed") +
  labs(title="Histogram of Residuals (Log-Log Model)", x="Residuals", y="Count") +
  theme_minimal()

# Perform Jarque–Bera test
jb_test_residuals_loglog <- jarque.bera.test(df$residuals_loglog)

# Display test results
print(jb_test_residuals_loglog)

#h.

# Linear-Log model estimation
model_linlog <- lm(food ~ log(income), data = df)

# Display regression summary
summary(model_linlog)

# Scatter plot of FOOD vs. log(INCOME)
ggplot(df, aes(x = log(income), y = food)) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", color = "tan2", se = TRUE) +  # Regression line
  labs(title = "Scatter Plot of FOOD vs. log(INCOME)",
       x = "log(Household Income)",
       y = "Food Expenditure") +
  theme_minimal()


#i.

# Define income values for elasticity calculation
income_values <- data.frame(income = c(19, 65, 160))

# Compute predicted food expenditure (FOOD_hat)
income_values$predicted_food <- 23.568 + 22.187 * log(income_values$income)

# Compute elasticity using the correct formula
income_values$elasticity <- 22.187 / income_values$predicted_food

# Display results
print(income_values)

# Compute lower and upper bound for elasticity using confidence intervals
income_values$elasticity_lower <- 15.867 / income_values$predicted_food
income_values$elasticity_upper <- 28.507 / income_values$predicted_food

# Display final table
print(income_values)


#j.

# Extract residuals from the linear-log model
df$residuals_linlog <- residuals(model_linlog)

# Plot residuals vs. log(INCOME)
ggplot(df, aes(x = log(income), y = residuals_linlog)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "tan4", linetype = "dashed") +
  labs(title = "Residuals vs. log(INCOME)",
       x = "log(Household Income)",
       y = "Residuals") +
  theme_minimal()

# Plot Histogram of Residuals
ggplot(df, aes(x = residuals_linlog)) +
  geom_histogram(color="black", fill="tan3", bins=30, alpha=0.7) +
  geom_vline(aes(xintercept = mean(residuals_linlog)), color="red", linetype="dashed") +
  labs(title="Histogram of Residuals (Linear-Log Model)", x="Residuals", y="Count") +
  theme_minimal()

# Perform Jarque–Bera test
jb_test_residuals_linlog <- jarque.bera.test(df$residuals_linlog)

# Display test results
print(jb_test_residuals_linlog)

