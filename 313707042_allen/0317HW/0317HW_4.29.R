library(POE5Rdata)
library(ggplot2)
install.packages("tseries")
library(tseries)


data("cex5_small")
?cex5_small

#a summary stats
summary_stats <- data.frame(
  Variable = c("food", "income"),
  Mean = c(mean(cex5_small$food), mean(cex5_small$income)),
  Median = c(median(cex5_small$food), median(cex5_small$income)),
  Min = c(min(cex5_small$food), min(cex5_small$income)),
  Max = c(max(cex5_small$food), max(cex5_small$income)),
  Std_Dev = c(sd(cex5_small$food), sd(cex5_small$income))
)

print(summary_stats)
#food
ggplot(cex5_small,aes(x=food))+
  geom_histogram(binwidth=10,fill="blue",color="black")+
  geom_vline(aes(xintercept = mean(food)),color="red",linetype="dashed",size=1)+
  geom_vline(aes(xintercept = median(food)), color = "purple", linetype = "dotted", size = 1) +
  labs(x = "FOOD", y = "Frequency") +
  theme_minimal()
#income
ggplot(cex5_small,aes(x=income))+
  geom_histogram(binwidth=10,fill="blue",color="black")+
  geom_vline(aes(xintercept = mean(income)),color="red",linetype="dashed",size=1)+
  geom_vline(aes(xintercept = median(income)), color = "purple", linetype = "dotted", size = 1) +
  labs(x = "INCOME", y = "Frequency") +
  theme_minimal()
#Jarque-Bera
jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)

#b
mod<-lm(food~income,data=cex5_small)
summary(mod)
confint(mod,level=0.95)
ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "blue") +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(x = "INCOME",
       y = "FOOD") +
  theme_minimal()

#c
cex5_small$residuals<-resid(mod)
ggplot(cex5_small,aes(x=income,y=residuals))+
  geom_point(color="blue")+
  geom_hline(yintercept=0,color="red",linetype="dashed")+
  labs(x="INCOME",
       y="Residuals")+
  theme_minimal()

ggplot(cex5_small,aes(x=residuals))+
  geom_histogram(binwidth = 20,fill="blue",color="black")+
  geom_vline(aes(xintercept = mean(residuals)),color="red",linetype="dashed")+
  labs(x="Residuals",
       y="Frequency")+
  theme_minimal()

jb_test <- jarque.bera.test(cex5_small$residuals)

cat("\n=== Jarque-Bera Test for Residuals ===\n")
print(jb_test)

#d
income <- c(19, 65, 160)
coeff_income <- coef(mod)["income"]

confint_income <- confint(mod, "income", level = 0.95)
elasticity <- coeff_income * income / predict(mod, newdata = data.frame(income = income))


elasticity_lower <- confint_income[1] * income / predict(mod, newdata = data.frame(income = income))
elasticity_upper <- confint_income[2] * income / predict(mod, newdata = data.frame(income = income))


elasticity_results <- data.frame(
  income = income,
  Elasticity = elasticity,
  Lower_Bound = elasticity_lower,
  Upper_Bound = elasticity_upper
)


print(elasticity_results)
#e
log_mod<-lm(log(food)~log(income),data=cex5_small)
summary(log_mod)

ggplot(cex5_small,aes(x=log(income),y=log(food)))+
  geom_point(color="blue")+
  geom_smooth(method="lm",se=FALSE,color="red")+
  labs(x="ln(INCOME)",
       y="ln(FOOD)")+
  theme_minimal()

y_hat <- predict(log_mod)
y_actual <- log(cex5_small$food)
generalized_r2 <- 1 - sum((y_hat - y_actual)^2) / sum((y_actual - mean(y_actual))^2)

cat("Generalized R^2 for log-log model:", generalized_r2, "\n")
linear_r2 <- summary(mod)$r.squared
cat("R^2 for linear model:", linear_r2, "\n")

#f
log_mod <- lm(log(food) ~ log(income), data = cex5_small)
summary(log_mod)
elasticity_log_log <- coef(log_mod)["log(income)"]

confint_log_log <- confint(log_mod, "log(income)", level = 0.95)
elasticity_results <- data.frame(
  Model = "Log-Log Model",
  Elasticity = elasticity_log_log,
  Lower_Bound = confint_log_log[1],
  Upper_Bound = confint_log_log[2]
)

print(elasticity_results)

#g
cex5_small$residuals_log_log <- resid(log_mod)
ggplot(cex5_small, aes(x = log(income), y = residuals_log_log)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs ln(INCOME) for Log-Log Model",
       x = "ln(INCOME)",
       y = "Residuals") +
  theme_minimal()

ggplot(cex5_small, aes(x = residuals_log_log)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  geom_vline(aes(xintercept = mean(residuals_log_log)), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals (Log-Log Model)",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

jb_test <- jarque.bera.test(cex5_small$residuals_log_log)

cat("\n=== Jarque-Bera Test for Residuals (Log-Log Model) ===\n")
print(jb_test)

#h
lin_log_mod <- lm(food ~ log(income), data = cex5_small)
summary(lin_log_mod)
ggplot(cex5_small, aes(x = log(income), y = food)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear-Log Model: FOOD vs ln(INCOME)",
       x = "ln(INCOME)",
       y = "FOOD") +
  theme_minimal()

lin_log_r2 <- summary(lin_log_mod)$r.squared
cat("\nR² for Linear-Log Model:", lin_log_r2, "\n")

linear_r2 <- summary(mod)$r.squared
log_log_r2 <- summary(log_mod)$r.squared

cat("R² for Linear Model:", linear_r2, "\n")
cat("R² for Log-Log Model:", log_log_r2, "\n")

#i
income <- c(19, 65, 160)
predicted_food <- predict(lin_log_mod, newdata = data.frame(income = income))

coeff_log_income <- coef(lin_log_mod)["log(income)"]

confint_log_income <- confint(lin_log_mod, "log(income)", level = 0.95)


elasticity <- coeff_log_income / predicted_food


elasticity_lower <- confint_log_income[1] / predicted_food
elasticity_upper <- confint_log_income[2] / predicted_food


elasticity_results <- data.frame(
  INCOME = income,
  Elasticity = elasticity,
  Lower_Bound = elasticity_lower,
  Upper_Bound = elasticity_upper
)

print(elasticity_results)

#j
cex5_small$residuals_lin_log <- resid(lin_log_mod)

ggplot(cex5_small, aes(x = log(income), y = residuals_lin_log)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs ln(INCOME) for Linear-Log Model",
       x = "ln(INCOME)",
       y = "Residuals") +
  theme_minimal()

ggplot(cex5_small, aes(x = residuals_lin_log)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  geom_vline(aes(xintercept = mean(residuals_lin_log)), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals (Linear-Log Model)",
       x = "Residuals",
       y = "Frequency") +
  theme_minimal()

jb_test <- jarque.bera.test(cex5_small$residuals_lin_log)
cat("\n=== Jarque-Bera Test for Residuals (Linear-Log Model) ===\n")
print(jb_test)

