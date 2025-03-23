install.packages("tseries")
install.packages("ggplot2")
library(ggplot2)
library(POE5Rdata)
data <- POE5Rdata::wa_wheat

mod1 <- lm(northampton ~ time, data = data)
mod2 <- lm(northampton ~ log(time), data = data)
mod3 <- lm(northampton ~ I(time^2), data = data)
mod4 <- lm(log(northampton) ~ time, data = data)
data$log_n <- log(data$northampton)

#4.28.a

# 套件gridExtra用於ggplot安排圖形排佈
library(gridExtra)

# 繪製四個 ggplot 圖形
plot1 <- ggplot(data, aes(x = time, y = northampton)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") + 
  labs(x = "Time", y = "Wheat yield")

plot2 <- ggplot(data, aes(x = time, y = northampton)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, color = "blue") +
  labs(x = "Time", y = "Wheat yield")

plot3 <- ggplot(data, aes(x = time, y = northampton)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ I(x^2), se = FALSE, color = "orange") +
  labs(x = "Time", y = "Wheat yield")

plot4 <- ggplot(data, aes(x = time, y = log_n)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "green") +
  labs(x = "Time", y = "Log wheat yield")

# 顯示四個圖形
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)  # 設置顯示為 2 列

-------------------------------------------
# 計算四個模型的殘差
data$resid_mod1 <- resid(mod1)
data$resid_mod2 <- resid(mod2)
data$resid_mod3 <- resid(mod3)
data$resid_mod4 <- resid(mod4)

# 繪製四個模型的殘差圖
plot1 <- ggplot(data, aes(x = time, y = resid_mod1)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals for linear model", x = "Time", y = "Residuals") +
  theme_minimal()

plot2 <- ggplot(data, aes(x = time, y = resid_mod2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Residuals for linear-log model", x = "Time", y = "Residuals") +
  theme_minimal()

plot3 <- ggplot(data, aes(x = time, y = resid_mod3)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "orange") +
  labs(title = "Residuals for quadratic model", x = "Time", y = "Residuals") +
  theme_minimal()

plot4 <- ggplot(data, aes(x = time, y = resid_mod4)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "green") +
  labs(title = "Residuals for log-linear model", x = "Time", y = "Residuals") +
  theme_minimal()

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)  # 2列顯示

----------------------------------------------
library(tseries)

jarque.bera.test(resid(mod1))
jarque.bera.test(resid(mod2))
jarque.bera.test(resid(mod3))
jarque.bera.test(resid(mod4))

par(mfrow=c(2,2))
qqnorm(resid(mod1))
qqline(resid(mod1), col = "red")
qqnorm(resid(mod2))
qqline(resid(mod2), col = "blue")
qqnorm(resid(mod3))
qqline(resid(mod3), col = "orange")
qqnorm(resid(mod4))
qqline(resid(mod4), col = "green")
par(mfrow=c(1,1))

summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared
summary(mod4)$r.squared

#4.28.b
coef(mod3)[2]

#4.28.c 檢測outliers
studentized_residuals <- rstudent(mod3)
leverage <- hatvalues(mod3)
dfbetas <- dfbetas(mod3)
dffits <- dffits(mod3)

outliers <- abs(studentized_residuals) > 2
high_leverage <- leverage > (2 * length(coef(mod3)) / nrow(data))
influential_dfbetas <- apply(dfbetas, 2, function(x) abs(x) > 1)
influential_dffits <- abs(dffits) > (2 * sqrt(length(coef(mod3)) / nrow(data)))

# 彙總結果
summary_df <- data.frame(
  Outliers = outliers,
  High_Leverage = high_leverage,
  Influential_DFBETAS = influential_dfbetas,
  Influential_DFFITS = influential_dffits
)

summary_df_filtered <- summary_df[!apply(summary_df, 1, function(row) all(row == FALSE)), ]

# 顯示過濾後的結果
summary_df_filtered

#4.28.d out of sample prediction
train_data <- data[1:47, ]
mod3_in_sample <- lm(northampton ~ I(time^2),data = train_data)

predict(mod3_in_sample,
        interval = "prediction",
        level = 0.95,
        newdata = data.frame(time = 48))

#4.29.a

data <- POE5Rdata::cex5_small
summary(data$food)
summary(data$income)

ggplot(data, aes(x = food)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "black") + 
  geom_vline(aes(xintercept = median(food), color = "Median"), linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = mean(food), color = "Mean"), linetype = "dashed", size = 1) + 
  labs(title = "Histogram of Food with Median and Mean",
       x = "Food", y = "Frequency") +
  theme_minimal(base_size = 14) + 
  scale_color_manual(values = c("Median" = "red", "Mean" = "blue")) +
  guides(color = guide_legend(title = "Statistics"))

ggplot(data, aes(x = income)) +
  geom_histogram(binwidth = 10, fill = "gray", color = "black") + 
  geom_vline(aes(xintercept = median(income), color = "Median"), linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = mean(income), color = "Mean"), linetype = "dashed", size = 1) + 
  labs(title = "Histogram of Income with Median and Mean",
       x = "Income", y = "Frequency") +
  theme_minimal(base_size = 14) + 
  scale_color_manual(values = c("Median" = "red", "Mean" = "blue")) +
  guides(color = guide_legend(title = "Statistics"))

jarque.bera.test(data$food)
jarque.bera.test(data$income)

#2.29.b

mod1 <- lm(food ~ income, data = data)

ggplot(data, aes(x = income, y = food))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") + 
  labs(x = "income", y = "food")

summary(mod1)
ci <- confint(mod1, level = 0.95)
ci

#4.29.c

plot(data$income , resid(mod1) , xlab = 'Income' , ylab = 'Residuals')

hist(resid(mod1) , breaks = 50 , xlab = 'Residuals')

jarque.bera.test(resid(mod1))

#4.29.d

income_values <- c(19, 65, 160)
fitted_food <- coef(mod1)[1] + coef(mod1)[2] * income_values
elasticities <- coef(mod1)[2] * income_values / fitted_food
z_value <- qnorm(0.975)
se_beta2 <- summary(mod1)$coefficients[2, 2]
lower_bound <- elasticities - z_value * se_beta2 * income_values / fitted_food
upper_bound <- elasticities + z_value * se_beta2 * income_values / fitted_food
elasticity_table <- data.frame(
  INCOME = income_values,
  Fitted_Elasticity = elasticities,
  Lower_Bound = lower_bound,
  Upper_Bound = upper_bound
)

#4.29.e
data$lnfood <- log(data$food)
data$lnincome <- log(data$income)

ggplot(data, aes(x = lnincome, y = lnfood))+
  geom_point()+
  geom_smooth(method = "lm" , formula = y~x, se = FALSE, color = "red")

mod2 <- lm(lnfood ~ lnincome , data = data)

yhat1 <- predict(mod1)
yhat2 <- exp(coef(mod2)[1]+coef(mod2)[2]*data$lnincome + summary(mod2)$sigma^2/2)
rg1 <- cor(data$food , yhat1)^2
rg2 <- cor(data$food , yhat2)^2
rg1
rg2

#4.29.f

ci2 <- confint(mod2, level = 0.95)
coef(mod2)[2]
ci2


#4.29.g
plot(data$lnincome , resid(mod2), xlab = 'Ln(INCOME)' , ylab = 'Residuals')
hist(resid(mod2), xlab = 'Residuals of loglog model',breaks = 50)
jarque.bera.test(resid(mod2))

#4.29.h
mod3 <- lm(food ~ lnincome, data = data)
ggplot(data, aes(x = lnincome , y = food))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, color = "red")

summary(mod1)$r.squared
summary(mod2)$r.squared
summary(mod3)$r.squared

#4.29.i
lnincomes <- c(log(19), log(65), log(160))
fitted_food2 <- coef(mod3)[1] + coef(mod3)[2] * lnincomes
elasticities <- coef(mod3)[2] / fitted_food2
se_beta2 <- summary(mod3)$coefficients[2, 2]
lower_bound <- elasticities - z_value * se_beta2 / fitted_food2
upper_bound <- elasticities + z_value * se_beta2 / fitted_food2
elasticity_table <- data.frame(
  LN_INCOME = lnincomes,
  Fitted_Elasticity = elasticities,
  Lower_Bound = lower_bound,
  Upper_Bound = upper_bound
)
#4.29.j
plot(data$lnincome , resid(mod3), xlab = 'Ln(INCOME)' , ylab = 'Residuals')
hist(resid(mod3), xlab = 'Residuals of linearlog model',breaks = 50)
jarque.bera.test(resid(mod3))









