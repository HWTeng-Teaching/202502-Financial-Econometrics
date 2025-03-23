#4.28
#313707014 陳紀蓁

library(ggplot2)  
library(POE5Rdata)
library(dplyr)
library(psych)

data ("wa_wheat")
summary(wa_wheat)
head(wa_wheat)

# a.畫圖


mod1 <- lm(northampton ~ time, data = wa_wheat)
mod2 <- lm(northampton ~ log(time), data= wa_wheat)
mod3 <- lm(northampton ~ I(time^2), data = wa_wheat)
mod4 <- lm(log(northampton) ~ time, data = wa_wheat)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

# 畫出回歸圖
ggplot(wa_wheat, aes(x = time, y = northampton)) +
  geom_point(color = "grey", size = 2) +  # 設定點的顏色與大小
  labs(title = "Northampton Wheat Yield over Time", x="Time", y="Northampton") +
  geom_smooth(aes(color = "Linear (y ~ x)"), method = "lm", formula = y ~ x, se = FALSE) +        # mod1
  geom_smooth(aes(color = "Logarithmic (y ~ log(x))"), method = "lm", formula = y ~ log(x),  se = FALSE) +   #mod2
  geom_smooth(aes(color = "Quadratic (y ~ I(x^2))"), method = "lm", formula = y ~ I(x^2), se = FALSE)+   #mod3
  geom_smooth(aes(color = "Log-Linear (log(y) ~ x)"), method = "lm", formula = log(y) ~ x, se = FALSE)+
  theme_minimal()  




# 畫出殘差圖，四張畫在同一張

models <- list(mod1, mod2, mod3, mod4)
model_names <- c("Linear Model", "Log Model", "Quadratic Model", "Log-Dependent Model")
# 創建空列表來存放圖表
plots <- list()

# 迴圈產生殘差圖
for (i in 1:length(models)) {
  residuals_df <- data.frame(         #for loop每次都會創建新的資料筐，所以在畫圖時不會重複
    time = wa_wheat$time,
    residual = residuals(models[[i]])
  )
  
  p <- ggplot(residuals_df, aes(x = time, y = residual)) +
    geom_point(color = "grey", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = paste("Residuals:", model_names[i]), x = "Time", y = "Residuals") +
    theme_minimal()
  
  plots[[i]] <- p  # 把圖表存進列表
}

# 顯示所有圖表
library(gridExtra)
grid.arrange(grobs = plots, nrow = 2, ncol = 2)




# 殘差檢定  假定殘差服從常態分度，p值>0.05，無法拒絕虛無假設，殘差常態分布



#測試回歸殘差的常態性
shapiro.test(residuals(mod1))
shapiro.test(residuals(mod2))
shapiro.test(residuals(mod3))
shapiro.test(residuals(mod4))




# b.選擇model 3為最佳模型
best_model <- mod3


summary(best_model)

# 提取時間變數的估計係數及其解釋
gamma1 <- coef(best_model)["I(time^2)"]
cat("時間變數的係數為:", gamma1, "\n")



# c.尋找異常值
library(car)
n <- nrow(model.matrix(best_model))  # 觀測數
p <- length(coef(best_model))        # 模型中的參數數量（含截距）
n
p

# 計算 studentized residuals 學生化殘差：判斷觀測點是否為異常值
stud_res <- rstudent(best_model)

# 計算影響值指標
leverage <- hatvalues(best_model)  #槓桿值：判斷觀測點是否影響點
dfbetas_values <- dfbetas(best_model)  #回歸係數變化量：測試某個觀測值對回歸係數的影響
dffits_values <- dffits(best_model) #影響診斷指標：測試某個觀測值對整體預測的影響

# 找出異常觀測值
outliers <- which(abs(stud_res) > 2)
cat("異常觀測值的索引:", outliers, "\n")

# 可視化異常值
plot(stud_res, main = "Studentized Residuals", ylab = "Residual", xlab = "Observation Index")
abline(h = c(-2, 2), col = "red", lty = 2)

plot(leverage , main = "leverage", ylab = "Residual", xlab = "Observation Index")
abline(h = 2*p/n, col = "red", lty = 2)


matplot(dfbetas_values,type = "h", main = "dfbetas_values", ylab = "Residual", xlab = "Observation Index")
dfbetas_cutoff <- 2 / sqrt(n)
abline(h = c(dfbetas_cutoff,-dfbetas_cutoff), col = "red", lty = 2)

plot(dffits_values, main = "dffits_values", ylab = "Residual", xlab = "Observation Index")
dffits_cutoff <- 2 * sqrt(p / n)
abline(h = c(dffits_cutoff,-dffits_cutoff), col = "red", lty = 2)

# d.




train <- wa_wheat[1:47, ]
model_train <- lm(northampton ~ I(time^2), data = train)
newdata <- data.frame(time = 48)
pred <- predict(model_train, newdata, interval = "prediction",level = 0.95)
origin <- wa_wheat[48, 1]

pred
origin





