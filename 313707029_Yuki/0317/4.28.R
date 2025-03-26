# 清除環境變數（可選）
rm(list=ls())

# 載入所需套件
library(POE5Rdata)
library(dplyr)
library(ggplot2)

# 載入資料
data("wa_wheat")

# 選擇 Northampton 郡的數據，並重新命名欄位以符合題目
northampton <- wa_wheat %>%
  select(TIME = time, YIELD = northampton) %>%
  mutate(Year = 1949 + TIME)  # 添加年份欄位

# 檢查數據
head(northampton)

# a. 估計四個方程並進行診斷
# 擬合四個模型
model1 <- lm(YIELD ~ TIME, data = northampton)  # 方程 1
model2 <- lm(YIELD ~ I(log(TIME)), data = northampton)  # 方程 2
model3 <- lm(YIELD ~ I(TIME^2), data = northampton)  # 方程 3
model4 <- lm(log(YIELD) ~ TIME, data = northampton)  # 方程 4

# (i) 擬合值圖
northampton <- northampton %>%
  mutate(fitted_model1 = predict(model1),
         fitted_model2 = predict(model2),
         fitted_model3 = predict(model3),
         fitted_model4 = exp(predict(model4)))

p1 <- ggplot(northampton, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted_model1), color = "blue", size = 1) +
  labs(title = "Model 1: YIELD ~ TIME", x = "TIME", y = "YIELD") +
  theme_minimal()

p2 <- ggplot(northampton, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted_model2), color = "red", size = 1) +
  labs(title = "Model 2: YIELD ~ log(TIME)", x = "TIME", y = "YIELD") +
  theme_minimal()

p3 <- ggplot(northampton, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted_model3), color = "green", size = 1) +
  labs(title = "Model 3: YIELD ~ TIME^2", x = "TIME", y = "YIELD") +
  theme_minimal()

p4 <- ggplot(northampton, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted_model4), color = "purple", size = 1) +
  labs(title = "Model 4: log(YIELD) ~ TIME (Exponentiated)", x = "TIME", y = "YIELD") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)

# (ii) 殘差圖
northampton <- northampton %>%
  mutate(resid_model1 = resid(model1),
         resid_model2 = resid(model2),
         resid_model3 = resid(model3),
         resid_model4 = resid(model4))

r1 <- ggplot(northampton, aes(x = TIME, y = resid_model1)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 1 Residuals", x = "TIME", y = "Residuals") +
  theme_minimal()

r2 <- ggplot(northampton, aes(x = TIME, y = resid_model2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 2 Residuals", x = "TIME", y = "Residuals") +
  theme_minimal()

r3 <- ggplot(northampton, aes(x = TIME, y = resid_model3)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 3 Residuals", x = "TIME", y = "Residuals") +
  theme_minimal()

r4 <- ggplot(northampton, aes(x = TIME, y = resid_model4)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Model 4 Residuals", x = "TIME", y = "Residuals") +
  theme_minimal()

print(r1)
print(r2)
print(r3)
print(r4)

# (iii) 誤差正態性檢驗 (Shapiro-Wilk 檢驗)
cat("Shapiro-Wilk Test for Normality of Residuals:\n")
cat("Model 1:", shapiro.test(resid(model1))$p.value, "\n")
cat("Model 2:", shapiro.test(resid(model2))$p.value, "\n")
cat("Model 3:", shapiro.test(resid(model3))$p.value, "\n")
cat("Model 4:", shapiro.test(resid(model4))$p.value, "\n")

# (iv) R^2 值
cat("\nR-squared Values:\n")
cat("Model 1:", summary(model1)$r.squared, "\n")
cat("Model 2:", summary(model2)$r.squared, "\n")
cat("Model 3:", summary(model3)$r.squared, "\n")
cat("Model 4:", summary(model4)$r.squared, "\n")

# b. 解釋時間相關變數的經濟意義
# 假設選擇模型 1
cat("\nModel 3 Coefficients:\n")
print(summary(model3)$coefficients)

# c. 識別異常觀測值
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

# d. 構建 1997 年的 95% 預測區間
data_train <- northampton %>% filter(TIME <= 47)
model1_train <- lm(YIELD ~ I(TIME^2), data = data_train)

new_data <- data.frame(TIME = 48)
pred <- predict(model1_train, newdata = new_data, interval = "prediction", level = 0.95)

cat("\n95% Prediction Interval for YIELD in 1997:\n")
print(pred)

true_yield_1997 <- northampton$YIELD[northampton$TIME == 48]
cat("True YIELD in 1997:", true_yield_1997, "\n")
cat("Is true value in interval?", 
    true_yield_1997 >= pred[1, "lwr"] & true_yield_1997 <= pred[1, "upr"], "\n")
