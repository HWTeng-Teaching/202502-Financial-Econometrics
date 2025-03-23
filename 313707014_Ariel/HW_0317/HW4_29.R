#4.29
# 313707014 陳紀蓁

library(ggplot2)  
library(POE5Rdata)
library(dplyr)
library(car)
library(tseries)
library(moments)

data ("cex5_small")
data("cex5")


# a.敘述統計與畫直方圖

describe(cex5_small$food)
describe(cex5_small$income)

food_mean <- mean(cex5_small$food)
food_median <- median(cex5_small$food)

#  偏態判斷  mean 大於 median 代表右側有極端值

if (food_mean >= food_median) {
  cat("food_mean > food_median： right-skewed\n")
} else {
  cat("food_mean < food_median：left-skewed\n")
}

# food 結果為右偏，因為右邊有極端值



income_mean <- mean(cex5_small$income)
income_median <- median(cex5_small$income)

if (income_mean >= income_median) {
  cat("income_mean > income_median： right-skewed\n")
} else {
  cat("income_mean < income_median：left-skewed\n")
}

# income 結果為右偏，因為右邊有極端值




# 繪製直方圖 同時標注 mean median 
ggplot(cex5_small, aes(x = food)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  geom_vline(aes(xintercept = food_mean, color = "mean"), linetype = "dashed", size = 1, alpha = 0.8) + # 平均數
  geom_vline(aes(xintercept = food_median, color = "median"), linetype = "dotted", size = 1, alpha = 0.8) + # 中位數
  ggtitle("Histogram of FOOD Expenditure")

ggplot(cex5_small, aes(x = income)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  geom_vline(aes(xintercept = income_mean, color = "mean"), linetype = "dashed", size = 1, alpha = 0.8) + # 平均數
  geom_vline(aes(xintercept = income_median, color = "median"), linetype = "dotted", size = 1, alpha = 0.8) + # 中位數
  ggtitle("Histogram of INCOME")

# Jarque-Bera 正態性檢定
jb_food <- jarque.test(cex5_small$food) #測試一般變數的常態性 
jb_income <- jarque.test(cex5_small$income)
print(jb_food)
print(jb_income)




# b. 畫出回歸線
mod1 <- lm(food ~ income, data = cex5_small)
summary(mod1)

ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "linear income-food ", x="incomec", y="food") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) 

#信賴區間
confint(mod1, level = 0.95)


#c. 找出殘差

#畫出散佈圖

residual_1 <- residuals(mod1)
residual_1

residual_time <- data.frame(income = cex5_small$income,residual = residual_1)

ggplot(residual_time,aes(x = income, y = residual))+
  geom_point(color = 'gray',size = 2)+
  labs(title = "residual of linear regression", x = "income", y = "residual")

#畫出長條圖

jarque.bera.test(residuals(mod1))  #測試殘差的常態性

hist(residuals(mod1), breaks = 20, probability = TRUE, main = "Histogram of Residuals linear regression",
     xlab = "Residuals", col = "lightblue")
lines(density(residuals(mod1)), col = "red", lwd = 2)


#d. 計算彈性與 95% 信賴區間

income_values <- c(19, 65, 160)

b2 <- coef(mod1)[[2]]

predict_value <- predict(mod1, newdata = data.frame(income = income_values))

interval <- confint(mod1, level = 0.95)["income", ]
interval

se <- summary(mod1)$coefficients[2, 2]
se

for (i in seq_along(income_values)) {
  e <- b2 * income_values[i] / predict_value[i] # 彈性計算
  e_c <- interval  * income_values[i] / predict_value[i] # 95% interval estimate of the elasticity  區間估計，可以這樣算是因為interval 本身就是用b2的se 下去估計的了
  se1 <- se* income_values[i] / predict_value[i]  # standard error of elasticity

  cat('income =', income_values[i], "expected food expenditure", predict_value[i],  ', elasticity =', e,  ",se = ",se1,   ',elasticity_interval =', e_c, '\n')
}

#   幫助理解 95% interval estimate of the elasticity 計算
# tc <- qt(0.975, 1198)
# lowb <- b2-tc*se 
# upb <- b2+tc*se   
# 算出來的 lowb upb 就是 interval


#e. 估計log log 模型
df_log <- cex5_small %>% 
  mutate(ln_food = log(cex5_small$food), ln_income = log(cex5_small$income))

mod2 <- lm (ln_food ~ ln_income, df_log)
summary(mod2)

ggplot(df_log, aes(x = ln_income, y = ln_food)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "ln(food)  vs. ln(income)  ", x="ln(income)", y="ln(food)") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE)+
  theme_minimal()  


#f. ????
b2_log <- coef(mod2)[[2]]
interval_log <- confint(mod2, level = 0.95)["ln_income", ]

cat("the point of the elasticity", b2_log)
cat ("95% interval estimate of the elasticity",interval_log)





income_values <- c(19, 65, 160)
predict_value_log <- predict(mod2, newdata = data.frame(ln_income = income_values))
predict_value_log

interval <- confint(mod2, level = 0.95)["ln_income", ]
interval

se <- summary(mod2)$coefficients[2, 2]
se

for (i in seq_along(income_values)) {
  e <- b2 * income_values[i] / predict_value[i] # 彈性計算
  e_c <- interval  * income_values[i] / predict_value[i] # 95% interval estimate of the elasticity  區間估計，可以這樣算是因為interval 本身就是用b2的se 下去估計的了
  se1 <- se* income_values[i] / predict_value[i]  # standard error of elasticity
  
  cat('ln_income =', income_values[i], "expected food expenditure", predict_value[i],  ', elasticity =', e,  ",se = ",se1,   ',elasticity_interval =', e_c, '\n')
}




# g.畫出殘差圖

#散布圖
residual_2 <- residuals(mod2)
residual_2

residual_time_2 <- data.frame(ln_income = df_log$ln_income, residuals = residual_2)

ggplot(residual_time_2 , aes(x = ln_income, y = residuals))+
  geom_point(color = 'gray',size = 2)+
  labs(title = "residual of log-log regression", x = "ln(income)", y = "residuals")


#畫出長條圖

jarque.bera.test(residuals(mod2))  #測試殘差的常態性

hist(residuals(mod2), breaks = 20, probability = TRUE, main = "Histogram of Residuals log-log regression",
     xlab = "Residuals", col = "lightblue")
lines(density(residuals(mod2)), col = "red", lwd = 2)


#h.估計food ln(income) 模型


mod3 <- lm (food ~ ln_income, df_log)
summary(mod3)

ggplot(df_log, aes(x = ln_income, y = food)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "food vs. ln(income)  ", x="ln(income)", y="food") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE)+
  theme_minimal()  


# i. 計算彈性與 95% 信賴區間
income_values_3 <- c(log(19), log(65), log(160))
income_values_3

b2_3 <- coef(mod3)[[2]]

predict_value_3 <- predict(mod3, newdata = data.frame(ln_income = income_values_3))
predict_value_3

interval3 <- confint(mod3, level = 0.95)["ln_income", ]
interval3

se <- summary(mod3)$coefficients[2, 2]
se

for (i in seq_along(income_values_3)) {
  e <- b2_3 / predict_value_3[i] # 彈性計算
  e_c <- interval3 / predict_value_3[i] # 95% interval estimate of the elasticity  區間估計，可以這樣算是因為interval 本身就是用b2的se 下去估計的了
  se1 <- se / predict_value_3[i]  # standard error of elasticity
  
  cat('income =', income_values[i], "expected food expenditure", predict_value_3[i],  ', elasticity =', e,  ",se = ",se1,   ',elasticity_interval =', e_c, '\n')
}



#j 殘差畫圖.
residual_3 <- residuals(mod3)
residual_3

residual_time_3 <- data.frame(ln_income = df_log$ln_income, residuals = residual_3)

ggplot(residual_time_3 , aes(x = ln_income, y = residuals))+
  geom_point(color = 'gray',size = 2)+
  labs(title = "residual of ln(income) regression", x = "ln(income)", y = "residuals")


#畫出長條圖

jarque.bera.test(residuals(mod3))  #測試殘差的常態性

hist(residuals(mod3), breaks = 20, probability = TRUE, main = "Histogram of Residuals ln(income) regression",
     xlab = "Residuals", col = "lightblue")
lines(density(residuals(mod3)), col = "red", lwd = 2)





