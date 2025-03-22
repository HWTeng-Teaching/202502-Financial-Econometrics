# C03Q29

rm(list = ls())  # 清除工作環境

# 載入必要套件，其中 dplyr 不顯示衝突警告
library(devtools)
library(knitr)
library(xtable)
library(printr)
library(stargazer)
library(dplyr, warn.conflicts = FALSE)
# install.packages("car")  # 如未安裝
library(car)
# install.packages("ggplot2")  
library(ggplot2)

# 載入 Rdata 資料檔案
#temp_file <- tempfile(fileext = ".rdata")
#download.file(
#  url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata",
#  destfile = temp_file, 
#  mode = "wb"
#)
#load(temp_file)

library(POE5Rdata)

######### 檢查資料結構 #####################
str(cex5_small)
summary(cex5_small)
head(cex5_small)
tail(cex5_small)
nrow(cex5_small)


# -----------------------------------------
# (a) Descriptive Statistics & Histograms
# -----------------------------------------
mean_food = mean(cex5_small$food)
median_food = median(cex5_small$food)
min_food = min(cex5_small$food)
max_food = max(cex5_small$food)
sd_food = sd(cex5_small$food)
mean_income = mean(cex5_small$income)
median_income = median(cex5_small$income)
min_income = min(cex5_small$income)
max_income = max(cex5_small$income)
sd_income = sd(cex5_small$income)

summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Min", "Max", "SD"),
  Food = c(mean(cex5_small$food), median(cex5_small$food), 
           min(cex5_small$food), max(cex5_small$food), sd(cex5_small$food)),
  Income = c(mean(cex5_small$income), median(cex5_small$income), 
             min(cex5_small$income), max(cex5_small$income), sd(cex5_small$income))
)
print(summary_table)


library(ggplot2)
ggplot(cex5_small, aes(x = food)) + 
  geom_histogram(bins = 30, color = "blue", alpha = 0.9) + 
  geom_vline(xintercept = mean_food, color = "red", linetype = "dashed", linewidth = 0.5) + 
  geom_vline(xintercept = median_food, color = "green", linetype = "dashed", linewidth = 0.5) + 
  labs(x = "Food Expenditure", y="Count") + 
  annotate("text", x = mean_food, y = max(table(cut(cex5_small$food, breaks = 30))) * 0.9, 
           label = "Mean", color = "red", angle = 90, vjust = -0.5) + 
  annotate("text", x = median_food, y = max(table(cut(cex5_small$food, breaks = 30))) * 0.8, 
           label = "Median", color = "green", angle = 90, vjust = -0.5) +
  theme_minimal()

ggplot(cex5_small, aes(x = income)) + 
  geom_histogram(bins = 30, color = "blue", alpha = 0.9) + 
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed", linewidth = 0.5) + 
  geom_vline(xintercept = median_income, color = "green", linetype = "dashed", linewidth = 0.5) + 
  labs(x = "Income", y="Count") + 
  annotate("text", x = mean_income, y = max(table(cut(cex5_small$income, breaks = 30))) * 0.9, 
           label = "Mean", color = "red", angle = 90, vjust = -0.5) + 
  annotate("text", x = median_income, y = max(table(cut(cex5_small$income, breaks = 30))) * 0.8, 
           label = "Median", color = "green", angle = 90, vjust = -0.5) +
  theme_minimal()

jb_food <- jarque.bera.test(cex5_small$food)
jb_income <- jarque.bera.test(cex5_small$income)
print(jb_food)
print(jb_income)

# -----------------------------------------
# (b) Linear Model: food = β1 + β2*income + e
# -----------------------------------------
model_linear <- lm(food ~ income, data = cex5_small)
summary(model_linear)

plot(cex5_small$income, cex5_small$food,
     main = "food vs income (Linear Model)",
     xlab = "income", ylab = "food Expenditure", pch = 16)
abline(model_linear, col = "red", lwd = 2)

# 95% CI for β2
confint_linear <- confint(model_linear, level = 0.95)
confint_linear["income", ]

# -----------------------------------------
# (c) Residual Analysis for Linear Model
# -----------------------------------------
res_linear <- resid(model_linear)
plot(cex5_small$income, res_linear,
     main = "Residuals vs income (Linear Model)",
     xlab = "income", ylab = "Residuals", pch = 16)
abline(h = 0, col = "blue", lwd = 2, lty = 2)

# -----------------------------------------
# (d) Elasticity Estimation for Linear Model
#     E = (β2 * income) / (β1 + β2 * income)
# -----------------------------------------

coef_lin <- coef(model_linear)
income_given <- data.frame(income = c(19, 65, 160))
predictions_linear <- predict(model_linear, newdata = income_given, interval = "confidence", level = 0.95)
beta2 <- coef(model_linear)["income"]  
beta2_se <- summary(model_linear)$coefficients["income", "Std. Error"]  
elasticity_point <- beta2 * income_given$income / predictions_linear[, "fit"]  
elasticity_se <- beta2_se * income_given$income / predictions_linear[, "fit"]  
z_value <- qnorm(0.975) 
elasticity_lower <- elasticity_point - z_value * elasticity_se
elasticity_upper <- elasticity_point + z_value * elasticity_se

elasticity_table <- data.frame(
  Income = income_given$income,
  Food_hat = predictions_linear[, "fit"],
  ε = elasticity_point,
  se_ε = elasticity_se,
  ε_lower_bound = elasticity_lower,
  ε_upper_bound = elasticity_upper
)
print(elasticity_table)

# -----------------------------------------
# (e) Log-Log Model: ln(food) = γ1 + γ2*ln(income) + e
# -----------------------------------------
model_loglog <- lm(log(food) ~ log(income), data = cex5_small)
summary(model_loglog)

plot(log(cex5_small$income), log(cex5_small$food),
     main = "Log-Log Model: log(food) vs log(income)",
     xlab = "log(income)", ylab = "log(food)", pch = 16)
abline(model_loglog, col = "blue", lwd = 2)

# -----------------------------------------
# (f) Elasticity in Log-Log Model
#     係數 γ2 即為彈性
# -----------------------------------------
elasticity_loglog <- coef(model_loglog)["log(income)"]
elasticity_loglog_CI <- confint(model_loglog, level = 0.95)["log(income)", ]
cat("Elasticity (log-log) =", elasticity_loglog, "\n95% CI =",
    elasticity_loglog_CI[1], elasticity_loglog_CI[2], "\n")

# -----------------------------------------
# (g) Residual Analysis for Log-Log Model
# -----------------------------------------
res_loglog <- resid(model_loglog)
plot(log(cex5_small$income), res_loglog,
     main = "Residuals vs log(income) (Log-Log Model)",
     xlab = "log(income)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "red", lwd = 2, lty = 2)

# -----------------------------------------
# (h) Linear-Log Model: food = α1 + α2*ln(income) + e
# -----------------------------------------
model_linlog <- lm(food ~ log(income), data = cex5_small)
summary(model_linlog)

plot(log(cex5_small$income), cex5_small$food,
     main = "Linear-Log Model: food vs log(income)",
     xlab = "log(income)", ylab = "food Expenditure", pch = 16)
abline(model_linlog, col = "green", lwd = 2)

# -----------------------------------------
# (i) Elasticity for Linear-Log Model
#     E = α2 / [ α1 + α2 * ln(income) ]
# -----------------------------------------
coef_linlog <- coef(model_linlog)

# Getting the model oefficients and their confidence intervals
beta0_hat3 <- coef(model_linlog)[1]
beta1_hat3 <- coef(model_linlog)[2]
beta1_CI3 <- confint(model_linlog, level = 0.95)[2,]

# Define the income values for which we want to compute elasticity
income_values <- c(19, 65, 160)

# Function to compute elasticity and the confidence interval
compute_values <- function(income) {
  # Fitted food expenditure value for the given income
  Y_hat3 <- beta0_hat3 + beta1_hat3 * log(income)
  
  # Elasticity: beta1_hat3 / income * (income / Y_hat3)
  elasticity3 <- beta1_hat3 / income * (income / Y_hat3)  
  
  # 95% CI for elasticity
  elasticity_CI3 <- beta1_CI3 / income * (income / Y_hat3)  
  
  # Compute Standard Error (SE) from the confidence interval
  SE_elasticity <- (elasticity_CI3[2] - elasticity_CI3[1]) / (2 * 1.96)
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat3, 
                    Elasticity = elasticity3, 
                    Elasticity_Lower = elasticity_CI3[1], 
                    Elasticity_Upper = elasticity_CI3[2],
                    SE = SE_elasticity))  
}

# Compute the results for each income level
results <- do.call(rbind, lapply(income_values, compute_values))

# Print the results
print(results)

# -----------------------------------------
# (j) Residual Analysis for Linear-Log Model
# -----------------------------------------
res_linlog <- resid(model_linlog)
plot(log(cex5_small$income), res_linlog,
     main = "Residuals vs log(income) (Linear-Log Model)",
     xlab = "log(income)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "purple", lwd = 2, lty = 2)

# -----------------------------------------
# (k) Model Comparison (Summary)
# -----------------------------------------
cat("Linear Model Summary:\n")
print(summary(model_linear))
cat("\nLog-Log Model Summary:\n")
print(summary(model_loglog))
cat("\nLinear-Log Model Summary:\n")
print(summary(model_linlog))

