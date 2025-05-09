## (4)

#(a)
# 設定模型參數
intercept <- 64.289
slope <- 0.990

# 建立 EXPER 從 0 到 30 的序列
EXPER <- seq(0, 30, by = 1)

# 計算 RATING 的預測值
RATING <- intercept + slope * EXPER

# 畫圖
plot(EXPER, RATING, type = "l", col = "blue", lwd = 2,
     xlab = "Years of Experience (EXPER)",
     ylab = "Predicted Rating",
     main = "Fitted Values from Model 1 (EXPER = 0 to 30)")
grid()



#(b)
# Model 2 參數
intercept_m2 <- 39.464
slope_m2 <- 15.312

# EXPER 從 1 到 30
EXPER_m2 <- seq(1, 30, by = 1)
RATING_m2 <- intercept_m2 + slope_m2 * log(EXPER_m2)

# 畫圖
plot(EXPER_m2, RATING_m2, type = "l", col = "red", lwd = 2,
     xlab = "Years of Experience (EXPER)",
     ylab = "Predicted Rating",
     main = "Fitted Values from Model 2 (EXPER = 1 to 30)")
grid()



## (28)

#(a)
rm(list=ls())  
library(POE5Rdata)  
data("wa_wheat")

    # (1) 線性模型
    model1 <- lm(northampton ~ time, data=wa_wheat)
    
    # (2) log-linear
    model2 <- lm(northampton ~ log(time), data=wa_wheat)
    
    # (3) quadratic model
    model3 <- lm(northampton ~ I(time^2), data=wa_wheat)
    
    # (4) log-lin model
    model4 <- lm(log(northampton) ~ time, data=wa_wheat)
    
    
    par(mfrow = c(2, 2))
    plot(wa_wheat$time, wa_wheat$northampton, main="Linear", xlab="TIME", ylab="YIELD")
    abline(model1, col="blue")
    
    plot(wa_wheat$time, wa_wheat$northampton, main="Log-Linear", xlab="TIME", ylab="YIELD")
    lines(wa_wheat$time, fitted(model2), col="blue")
    
    plot(wa_wheat$time, wa_wheat$northampton, main="Quadratic", xlab="TIME", ylab="YIELD")
    lines(wa_wheat$time, fitted(model3), col="blue")
    
    plot(wa_wheat$time, log(wa_wheat$northampton), main="Log-Lin", xlab="TIME", ylab="ln(YIELD)")
    lines(wa_wheat$time, fitted(model4), col="blue")
    
    par(mfrow = c(2, 2))  # 將畫面分成 2x2
    
    # 線性模型殘差圖
    plot(wa_wheat$time, resid(model1), 
         main = "Residuals (Linear Model)", 
         xlab = "TIME", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    # Log-linear 模型殘差圖
    plot(wa_wheat$time, resid(model2), 
         main = "Residuals (Log-Linear Model)", 
         xlab = "TIME", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    # Quadratic 模型殘差圖
    plot(wa_wheat$time, resid(model3), 
         main = "Residuals (Quadratic Model)", 
         xlab = "TIME", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    # Log-lin 模型殘差圖
    plot(wa_wheat$time, resid(model4), 
         main = "Residuals (Log-Lin Model)", 
         xlab = "TIME", ylab = "Residuals")
    abline(h = 0, col = "red")
    
    
    library(tseries)
    jarque.bera.test(resid(model1))
    jarque.bera.test(resid(model2))
    jarque.bera.test(resid(model3))
    jarque.bera.test(resid(model4))
    
    summary(model1)  # 線性
    summary(model2)  # log-linear
    summary(model3)  # quadratic
    summary(model4)  # log-lin
    
    
  #(c)
    # (1) 計算學生化殘差
    stud_res <- rstudent(model3)
    which(abs(stud_res) > 2)  # 找出大於 |2| 的異常觀測值
    
    # (2) 槓桿值 (leverage)
    lev <- hatvalues(model3)
    # 通常判斷標準：2p/n，其中 p = 參數數量，n = 樣本數
    p <- length(coef(model3))
    n <- nrow(wa_wheat)
    threshold_lev <- (2 * p) / n
    which(lev > threshold_lev)
    
    # (3) DFBETAS：判斷哪些觀測值對係數影響大
    dfb <- dfbetas(model3)
    # 判斷標準：絕對值 > 1
    which(abs(dfb[,2]) > 1)  # 看 TIME² 的影響
    
    # (4) DFFITS：判斷對整體擬合影響大的觀測值
    dff <- dffits(model3)
    threshold_dffits <- 2 * sqrt(p / n)
    which(abs(dff) > threshold_dffits)
    
    
    
    #(d)
    # 篩選 1996 年以前資料
    data_1996 <- subset(wa_wheat, time < 47)
    
    # 使用 quadratic 模型重新估計
    model3_1996 <- lm(northampton ~ I(time^2), data = wa_wheat)

    # 找出 1997 年的 time 值
    time_1997 <- wa_wheat$time[wa_wheat$time == 47]
    
    # 對 1997 年進行預測
    pred_1997 <- predict(model3_1996, 
                         newdata = data.frame(time = time_1997), 
                         interval = "prediction", level = 0.95)
    print(pred_1997)
    
    
    # 1997 年的實際值
    actual_1997 <- wa_wheat$northampton[wa_wheat$time == 48]
    
    # 比較
    print(actual_1997)
    # 看 actual_1997 是否落在 pred_1997 的 Lwr 與 Upr 區間內


## (29)

#(a)
rm(list=ls())  
library(POE5Rdata)  
data("cex5_small")
library(dplyr)

summary_stats <- cex5_small %>%
  summarise(
    FOOD_mean = mean(food, na.rm = TRUE),
    FOOD_median = median(food, na.rm = TRUE),
    FOOD_min = min(food, na.rm = TRUE),
    FOOD_max = max(food, na.rm = TRUE),
    FOOD_sd = sd(food, na.rm = TRUE),
    
    INCOME_mean = mean(income, na.rm = TRUE),
    INCOME_median = median(income, na.rm = TRUE),
    INCOME_min = min(income, na.rm = TRUE),
    INCOME_max = max(income, na.rm = TRUE),
    INCOME_sd = sd(income, na.rm = TRUE)
  )
    print(summary_stats)
    
    
#(b)
    # 線性迴歸
    model_b <- lm(food ~ income, data = cex5_small)
    summary(model_b)
    
    # 95% 信賴區間
    confint(model_b, level = 0.95)
    
    # 畫散點圖與回歸線
    plot(cex5_small$income, cex5_small$FOOD, 
         main = "FOOD vs INCOME", 
         xlab = "INCOME", ylab = "FOOD")
    abline(model_b, col = "blue", lwd = 2)
  
    
    
    
    
    
#(c)
    install.packages("tseries")
    library(tseries)
    
    # 取得殘差
    residuals_b <- resid(model_b)
    
    # 殘差對 INCOME 圖
    plot(cex5_small$income, residuals_b, 
         main = "Residuals vs INCOME", 
         xlab = "INCOME", ylab = "Residuals")
    abline(h = 0, col = "red", lwd = 2)
    
    # 殘差直方圖
    hist(residuals_b, breaks = 30, 
         main = "Histogram of Residuals", 
         xlab = "Residuals")
    abline(v = mean(residuals_b), col = "blue", lwd = 2)
    
    # Jarque–Bera normality test
    jarque.bera.test(residuals_b)
    

  
    
#(d)
    # 拿出 beta2
    beta2 <- coef(model_b)["income"]
    
    # 計算 INCOME = 19, 65, 160 下的彈性
    incomes <- c(19, 65, 160)
    predicted_food <- predict(model_b, newdata = data.frame(income = incomes))
    elasticities <- beta2 * incomes / predicted_food
    
    # 計算 β₂ 的 95% 信賴區間上下限對應的彈性範圍
    confint_beta2 <- confint(model_b, level = 0.95)["income",]
    elasticity_lower <- confint_beta2[1] * incomes / predicted_food
    elasticity_upper <- confint_beta2[2] * incomes / predicted_food
    
    # 整理結果
    elasticity_table <- data.frame(
      INCOME = incomes,
      Point_Estimate = elasticities,
      Lower_95_CI = elasticity_lower,
      Upper_95_CI = elasticity_upper
    )
    
    print(elasticity_table)
    
    
    
    
    
#(e)
    # 建立 log-log 模型
    model_e <- lm(log(food) ~ log(income), data = cex5_small)
    summary(model_e)
    
    # 畫出 ln(FOOD) vs ln(INCOME) 散點圖
    plot(log(cex5_small$income), log(cex5_small$food),
         main = "log(FOOD) vs log(INCOME)",
         xlab = "ln(INCOME)", ylab = "ln(FOOD)")
    abline(model_e, col = "blue", lwd = 2)
    
    
    
    
    
#(f)
    # log-log 模型
    model_e <- lm(log(food) ~ log(income), data = cex5_small)
    summary(model_e)
    
    # 信賴區間
    confint(model_e, level = 0.95)
    
    
    
    
#(g)
    residuals_e <- resid(model_e)
    
    # 殘差對 ln(INCOME)
    plot(log(cex5_small$income), residuals_e, 
         main = "Residuals vs ln(INCOME) (log-log model)", 
         xlab = "ln(INCOME)", ylab = "Residuals")
    abline(h = 0, col = "red", lwd = 2)
    
    # 殘差直方圖
    hist(residuals_e, breaks = 30, 
         main = "Histogram of Residuals (log-log model)", xlab = "Residuals")
    
    # Jarque–Bera test
    library(tseries)
    jarque.bera.test(residuals_e)
    
    
    
    
    
 #(h)
    model_h <- lm(food ~ log(income), data = cex5_small)
    summary(model_h)
    
    model_h <- lm(food ~ log(income), data = cex5_small)
    plot(log(cex5_small$income), cex5_small$food, 
         main = "FOOD vs ln(INCOME) (linear-log model)",
         xlab = "ln(INCOME)", ylab = "FOOD")
    abline(model_h, col = "blue", lwd = 2)
    
    
    # 比較 R²
    cat("R² for linear model:", summary(model_b)$r.squared, "\n")
    cat("R² for log-log model:", summary(model_e)$r.squared, "\n")
    cat("R² for linear-log model:", summary(model_h)$r.squared, "\n")

    
    
    
    
  #(i)
    alpha2 <- coef(model_h)["log(income)"]
    income_values <- c(19, 65, 160)
    pred_food_h <- predict(model_h, newdata = data.frame(income = income_values))
    elasticities_h <- alpha2 / pred_food_h
    
    # 95% 信賴區間 (上下限對應彈性)
    confint_alpha2 <- confint(model_h, level = 0.95)["log(income)",]
    elasticity_h_lower <- confint_alpha2[1] / pred_food_h
    elasticity_h_upper <- confint_alpha2[2] / pred_food_h
    
    data.frame(INCOME = income_values,
               Elasticity = elasticities_h,
               Lower_95_CI = elasticity_h_lower,
               Upper_95_CI = elasticity_h_upper)
    
    
    
    
  #(j)
    residuals_h <- resid(model_h)
    
    # 殘差對 ln(INCOME)
    plot(log(cex5_small$income), residuals_h, 
         main = "Residuals vs ln(INCOME) (linear-log model)", 
         xlab = "ln(INCOME)", ylab = "Residuals")
    abline(h = 0, col = "red", lwd = 2)
    
    # 殘差直方圖
    hist(residuals_h, breaks = 30, 
         main = "Histogram of Residuals (linear-log model)", xlab = "Residuals")
    
    # Jarque–Bera test
    jarque.bera.test(residuals_h)
    
