remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("wa_wheat")
library(ggplot2)

#4.04.a
experience1 <- seq(0, 30, by = 1)  # Model 1 range (0 to 30)
experience2 <- seq(1, 30, by = 1)  # Model 2 range (1 to 30, log(0) undefined)

predicted_rating1 <- 64.289 + 0.990 * experience1
predicted_rating2 <- 39.464 + 15.312 * log(experience2)

model1_data <- data.frame(EXPER = experience1, RATING = predicted_rating1, Model = "Model 1: Linear")
model2_data <- data.frame(EXPER = experience2, RATING = predicted_rating2, Model = "Model 2: Log-Linear")

combined_data <- rbind(model1_data, model2_data)

ggplot(combined_data, aes(x = EXPER, y = RATING, color = Model)) +
  geom_line(linewidth = 1.2) +  # Line plot for each model
  labs(title = "Fitted Values from Model 1 and Model 2",
       x = "Years of Experience (EXPER)",
       y = "Predicted RATING",
       color = "Model Type") +
  scale_color_manual(values = c("blue", "red")) +  # Custom colors
  theme_minimal()

#4.28.a
summary(wa_wheat)
wa_wheat$time2 <- wa_wheat$time^2  
wa_wheat$yield<- wa_wheat$northampton

model1 <- lm(yield ~ time, data=wa_wheat)               # 線性模型
model2 <- lm(yield ~ log(time), data=wa_wheat)          # 對數模型
model3 <- lm(yield ~ time2, data=wa_wheat)              # 二次模型
model4 <- lm(log(yield) ~ time, data=wa_wheat)          # 指數模型

summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared

par(mfrow=c(2,2))
plot(wa_wheat$time, wa_wheat$yield, main="Model 1: YIELD ~ TIME")
abline(model1, col="red")

plot(wa_wheat$time, wa_wheat$yield, main="Model 2: YIELD ~ log(TIME)")
lines(wa_wheat$time, fitted(model2), col="blue")

plot(wa_wheat$time, wa_wheat$yield, main="Model 3: YIELD ~ TIME^2")
lines(wa_wheat$time, fitted(model3), col="green")

plot(wa_wheat$time, log(wa_wheat$yield), main="Model 4: log(YIELD) ~ TIME")
abline(model4, col="purple")

par(mfrow=c(2,2))
plot(model1$residuals, main="Residuals for Model 1")
plot(model2$residuals, main="Residuals for Model 2")
plot(model3$residuals, main="Residuals for Model 3")
plot(model4$residuals, main="Residuals for Model 4")

shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model4$residuals)

#4.28.b
summary(model3)

#4.28.c
studentized_residuals <- rstudent(model3)
plot(1:48,studentized_residuals ,xlab = 'INDEX',main = 'Studentized Residuals')
abline(h =c(-2,2), col = "red", lwd=2) #95%C.I.

leverage_values <- hatvalues(model3)
plot(1:48,leverage_values,xlab = 'INDEX',main = 'LEVERAGE')
abline(h =2*2/48, col = "red", lwd=2) #h_bar=2/48

plot(dffits(model3), main = "DFFITS")
abline(h=2*sqrt(2/48),col='red',lwd=2)
abline(h=-2*sqrt(2/48),col='red',lwd=2)

dfbetas_plot <- dfbetas(model3)
matplot(dfbetas_plot, type = "h", main = "DFBETAS")
abline(h=2/sqrt(48),col='red',lwd=2)
abline(h=-2/sqrt(48),col='red',lwd=2)                           

#4.28.d
train <- wa_wheat[1:47, ]
model_train <- lm(yield ~ I(time^2), data = train)
newdata <- data.frame(time = 48)
(pred <- predict(model_train, newdata, interval = "prediction",
                 level = 0.95))
(origin <- wa_wheat[48, 1])

#4.29.a
data("cex5_small")
summary(cex5_small)

summary_food <- summary(cex5_small$food)
summary_income <- summary(cex5_small$income)
sd_food <- sd(cex5_small$food)
sd_income <- sd(cex5_small$income)
list(summary_food = summary_food, summary_income = summary_income, sd_food = sd_food, sd_income = sd_income)

mean_food <- mean(cex5_small$food)
median_food <- median(cex5_small$food)

ggplot(cex5_small, aes(x = food)) +
  geom_histogram(aes(y = (..count..)/sum(..count..) * 100), 
                 binwidth = 18, color = "black", fill = "skyblue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  labs(title = "Histogram of FOOD", x = "FOOD", y = "Percent (%)") +
  geom_vline(aes(xintercept = mean_food), color = "red", linetype = "dashed", linewidth = 1) + # 平均值
  geom_vline(aes(xintercept = median_food), color = "blue", linetype = "dashed", linewidth = 1) + # 中位數
  annotate("text", x = mean_food, y = 15, label = paste0("Mean: ", round(mean_food, 1)), color = "red", hjust = -0.1) +
  annotate("text", x = median_food, y = 13, label = paste0("Median: ", round(median_food, 1)), color = "blue", hjust = -0.1) +
  theme_minimal()

mean_income <- mean(cex5_small$income)
median_income <- median(cex5_small$income)

ggplot(cex5_small, aes(x = income)) +  
  geom_histogram(aes(y = (..count..)/sum(..count..) * 100), 
                 binwidth = 10, color = "black", fill = "lightgreen") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  
  labs(title = "Histogram of INCOME", x = "INCOME", y = "Percent (%)") +
  geom_vline(aes(xintercept = mean_income), color = "red", linetype = "dashed", linewidth = 1) + # 平均值
  geom_vline(aes(xintercept = median_income), color = "blue", linetype = "dashed", linewidth = 1) + # 中位數
  annotate("text", x = mean_income, y = 10, label = paste0("Mean: ", round(mean_income, 1)), color = "red", hjust = -0.1) +
  annotate("text", x = median_income, y = 8, label = paste0("Median: ", round(median_income, 1)), color = "blue", hjust = -0.1) +
  theme_minimal()

install.packages("tseries")
library(tseries)  

jb_food <- jarque.bera.test(cex5_small$food)
jb_income <- jarque.bera.test(cex5_small$income)

list(jb_food = jb_food, jb_income = jb_income)

#4.29.b
model <- lm(food ~ income, data = cex5_small)

summary(model)
confint(model, level = 0.95)

ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Fitted regression line with confidence interval
  labs(title = "Scatter Plot of FOOD vs INCOME with Fitted Line",
       x = "Household Income",
       y = "Food Expenditure") +
  theme_minimal()
#4.29.c
residuals <- resid(model)
residual_data <- data.frame(INCOME = cex5_small$income, Residuals = residuals)

ggplot(residual_data, aes(x = INCOME, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs INCOME", x = "Household Income", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(aes(y = (..count..)/sum(..count..) * 100), bins = 30, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Percent") +
  theme_minimal()

jb_residuals <- jarque.bera.test(model$residuals)
jb_residuals 

#4.29.d
beta_2 <- coef(model)["income"]
conf_int <- confint(model, "income", level = 0.95)
income_levels <- c(19, 65, 160)
predicted_food1 <- predict(model, newdata = data.frame(income = income_levels))
predicted_food1

elasticity <- beta_2 * income_levels / predicted_food1
elasticity

summary(model)
elasticity_lower <- conf_int[1] * income_levels / predicted_food1
elasticity_upper <- conf_int[2] * income_levels / predicted_food1

elasticity_results <- data.frame(
  INCOME = income_levels,
  Predicted_FOOD = predicted_food1,
  Elasticity = elasticity,
  Lower_CI = elasticity_lower,
  Upper_CI = elasticity_upper
)
print(elasticity_results)

#4.29.e
cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

log_model <- lm(log_food ~ log_income, data = cex5_small)
summary(log_model)

ggplot(cex5_small, aes(x = log_income, y = log_food)) +
  geom_point(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Fitted regression line
  labs(title = "Scatter Plot of log(FOOD) vs log(INCOME)",
       x = "log(INCOME)",
       y = "log(FOOD)") +
  theme_minimal()

summary(log_model)$r.squared
summary(model)$r.squared

#4.29.f
summary(log_model)
elasticity_loglog <-coef(log_model)[2]
elasticity_loglog
conf_int <- confint(log_model, "log_income", level = 0.95)
conf_int

#4.29.g
residuals_log <- resid(log_model)
residual_data <- data.frame(
  log_income = log(cex5_small$income),
  Residuals = residuals_log
)
ggplot(residual_data, aes(x = log_income, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs log(INCOME)", x = "log(INCOME)", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Residuals = residuals_log), aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Residuals (Log-Log Model)", x = "Residuals", y = "Density") +
  theme_minimal()

jb_test_log <- jarque.bera.test(residuals_log)

print(jb_test_log)

install.packages("moments")
library(moments)
skewness(residuals_log)

#4.29.h
lin_log_model <- lm(food ~ log_income, data = cex5_small)
summary(lin_log_model)

ggplot(cex5_small, aes(x = log_income, y = food)) +
  geom_point(alpha = 0.5) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Fitted regression line
  labs(title = "Scatter Plot of FOOD vs log(INCOME)",
       x = "log(INCOME)",
       y = "FOOD") +
  theme_minimal()

summary(lin_log_model)$r.squared

#4.29.i
income_levels <- c(19, 65, 160)
predicted_food2 <- predict(lin_log_model, newdata = data.frame(log_income = log(income_levels)))

beta_2_linlog <- coef(lin_log_model)["log_income"]  # Extract coefficient
elasticity2 <- beta_2_linlog / predicted_food2  # Elasticity formula

conf_int_linlog <- confint(lin_log_model, "log_income", level = 0.95)
elasticity_lower2 <- conf_int_linlog[1] / predicted_food2 
elasticity_upper2 <- conf_int_linlog[2] / predicted_food2 

elasticity_results2 <- data.frame(
  INCOME = income_levels,
  Predicted_FOOD = predicted_food2,
  Elasticity = elasticity2,
  Lower_CI = elasticity_lower2,
  Upper_CI = elasticity_upper2
)

print(elasticity_results2)

#4.29.j
residuals_lin_log <- resid(lin_log_model)
residual_data <- data.frame(
  log_income = log(cex5_small$income),
  Residuals = residuals_lin_log
)

ggplot(residual_data, aes(x = log_income, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs log(INCOME)", x = "log(INCOME)", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Residuals = residuals_lin_log), aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Residuals (Linear-Log Model)", 
       x = "Residuals", 
       y = "Density") +
  theme_minimal()

jb_test_lin_log <- jarque.bera.test(residuals_lin_log)

print(jb_test_lin_log)