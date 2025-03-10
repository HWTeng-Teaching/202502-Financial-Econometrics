# 2.17
![image](https://github.com/user-attachments/assets/7999626c-b693-46c1-b5ef-829b2736a402)
## (a)
![image](https://github.com/user-attachments/assets/4c4d50a7-ee20-4e17-b790-2ea00eff026b)

R Code:  
plot(collegetown$sqft, collegetown$price,  
     xlab = "房屋面積 (百平方英尺)",  
     ylab = "房價 (千美元)",  
     main = "房價與房屋面積散佈圖",  
     pch = 16, col = "blue")  

## (b)
![image](https://github.com/user-attachments/assets/787bfc43-db0c-4c74-b5c8-745655a535ea)

R Code:  
lm_linear <- lm(price ~ sqft, data = collegetown)  
summary(lm_linear)  
abline(lm_linear, col = "red", lwd = 2)  
legend("topleft", legend = c("資料點", "線性迴歸擬合線"),  
       col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))  

## (c)
在2,000平方英尺房屋下，每增加100平方英尺的邊際效應 = 7.38076 KUSD  

R Code:  
lm_quad <- lm(price ~ I(sqft^2), data = collegetown)  
summary(lm_quad)  

sqft_target <- 20  
alpha2 <- coef(lm_quad)["I(sqft^2)"]  
marginal_effect <- 2 * alpha2 * sqft_target  
cat("在2000平方英尺房屋下，每增加100平方英尺的邊際效應 =",  
    marginal_effect, "（千美元）\n\n")  

## (d)
![image](https://github.com/user-attachments/assets/f51ed58f-323c-40d4-aa1d-966338d32b7a)

R Code:  
sqft_seq <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 300)  
pred_quad <- predict(lm_quad, newdata = data.frame(sqft = sqft_seq))  

A. 繪製散佈圖與二次擬合曲線:  
plot(collegetown$sqft, collegetown$price,  
     xlab = "房屋面積 (百平方英尺)",  
     ylab = "房價 (千美元)",  
     main = "二次迴歸擬合曲線與2000平方英尺切線",  
     pch = 16, col = "blue")  
lines(sqft_seq, pred_quad, col = "red", lwd = 2)  

B. 計算在 SQFT_target (20) 處的預測房價:  
price_target <- predict(lm_quad, newdata = data.frame(sqft = sqft_target))  

C. 切線斜率：dPRICE/dSQFT = 2 * α₂ * SQFT_target  
tangent_slope <- 2 * alpha2 * sqft_target  

D. 定義切線函數： y - price_target = tangent_slope*(SQFT - SQFT_target)  
tangent_line <- price_target + tangent_slope * (sqft_seq - sqft_target)  
lines(sqft_seq, tangent_line, col = "green", lwd = 2, lty = 2)  
legend("topleft", legend = c("資料點", "二次迴歸擬合曲線", "2000平方英尺切線"),  
       col = c("blue", "red", "green"), pch = c(16, NA, NA),  
       lty = c(NA, 1, 2), lwd = c(NA, 2, 2))  
## (e)
在2,000平方英尺房屋下，房價相對於面積的彈性 = 0.8819511  

R Code:  
elasticity <- tangent_slope * (sqft_target / price_target)  
cat("在2000平方英尺房屋下，房價相對於面積的彈性 =", elasticity, "\n\n")  

## (f)
![image](https://github.com/user-attachments/assets/a98f85d0-7fa2-429c-9113-9e6aef0e41ec)
R Code:  
res_linear <- residuals(lm_linear)  
res_quad <- residuals(lm_quad)  
par(mfrow = c(1, 2))  
plot(collegetown$sqft, res_linear,  
     xlab = "房屋面積 (百平方英尺)",  
     ylab = "殘差",  
     main = "線性模型殘差圖",  
     pch = 16, col = "purple")  
abline(h = 0, lty = 2)  
plot(collegetown$sqft, res_quad,  
     xlab = "房屋面積 (百平方英尺)",  
     ylab = "殘差",  
     main = "二次模型殘差圖",  
     pch = 16, col = "orange")  
abline(h = 0, lty = 2)  
par(mfrow = c(1, 1))  

## (g)
線性模型 SSE = 5262847 ; 二次模型 SSE = 4222356

R Code: 
SSE_linear <- sum(res_linear^2)  
SSE_quad <- sum(res_quad^2)  
cat("線性模型 SSE =", SSE_linear, "\n")  
cat("二次模型 SSE =", SSE_quad, "\n")  

# 2.25
![image](https://github.com/user-attachments/assets/58c7c607-a636-4292-b076-b9d491660900)
## (a)
![image](https://github.com/user-attachments/assets/3b6fe6f8-20e7-4cf3-bb4c-cf3f289a3ff3)
平均值 = 49.27085  
中位數 = 32.555  
第25百分位數 = 12.04  
第75百分位數 = 67.5025  

R Code:  
hist(cex5_small$foodaway, breaks = 30,  
     main = "foodaway 的直方圖",  
     xlab = "foodaway (美元)",  
     col = "lightblue")  
foodaway_mean    <- mean(cex5_small$foodaway, na.rm = TRUE)  
foodaway_median  <- median(cex5_small$foodaway, na.rm = TRUE)  
foodaway_quant25 <- quantile(cex5_small$foodaway, probs = 0.25, na.rm = TRUE)  
foodaway_quant75 <- quantile(cex5_small$foodaway, probs = 0.75, na.rm = TRUE)  

cat("【foodaway 描述性統計】\n")  
cat("平均值 =",  foodaway_mean,   "\n")  
cat("中位數 =",  foodaway_median, "\n")  
cat("第25百分位數 =", foodaway_quant25, "\n")  
cat("第75百分位數 =", foodaway_quant75, "\n\n")  

## (b)
<img width="211" alt="image" src="https://github.com/user-attachments/assets/f01c0bbf-7afd-4b95-b76a-5e28aab01037" />

R Code:  
if("advanced" %in% names(cex5_small) & "college" %in% names(cex5_small)) {  
  cex5_small$degree <- ifelse(cex5_small$advanced == 1, "advanced",  
                              ifelse(cex5_small$college == 1, "college", "none"))  
  
  cex5_small$degree <- factor(cex5_small$degree, levels = c("none", "college", "advanced"))  
  
  group_stats <- cex5_small %>%  
    group_by(degree) %>%  
    summarize(  
      Mean_foodaway   = mean(foodaway, na.rm = TRUE),  
      Median_foodaway = median(foodaway, na.rm = TRUE)  
    )  
  
  cat("【各學歷群體 FOODAWAY 描述性統計】\n")  
  print(group_stats)  
  cat("\n")  
} else {  
  cat("【各學歷群體 FOODAWAY 描述性統計】\n")  
  cat("資料中未找到表示家庭學歷狀況的變數 'advanced' 或 'college'。請根據實際資料調整分群條件。\n\n")  
}

## (c)
![image](https://github.com/user-attachments/assets/76f64295-880f-4675-be02-852b851901cb)
<img width="512" alt="image" src="https://github.com/user-attachments/assets/5ae1344b-7ddc-44b1-b48f-cd9933845e2b" />

A. 計算 ln(FOODAWAY)（log() 僅對正數有意義）  
cex5_small$lnfoodaway <- log(cex5_small$foodaway)  
B. 將非正數 (0 或負數) 轉為 NA (log(0) 會產生 -Inf)  
cex5_small$lnfoodaway[is.infinite(cex5_small$lnfoodaway)] <- NA  

hist(cex5_small$lnfoodaway, breaks = 15,  
     main = "ln(FOODAWAY) 的直方圖",  
     xlab = "ln(FOODAWAY)",  
     col = "lightgreen")  
summary(cex5_small$lnfoodaway)  

## (d)
【迴歸模型估計】 ln(FOODAWAY) = 3.1293 + 0.0069 * INCOME + e  
當 INCOME 增加 1 單位（即100美元）時，ln(FOODAWAY) 平均變化 0.0069 單位  
這可近似解釋為 FOODAWAY 支出變化的百分比效應  

R Code:  
lm_model <- lm(lnfoodaway ~ income, data = cex5_small)  
beta2    <- coef(lm_model)["income"]  

cat("【迴歸模型估計】\n")  
cat("模型：ln(FOODAWAY) = ", round(coef(lm_model)[1], 4), " + ",   
    round(beta2, 4), " * income + e\n")  
cat("解釋：當 income 增加 1 單位（即100美元）時，ln(FOODAWAY) 平均變化",   
    round(beta2, 4), "單位。\n")  
cat("這可近似解釋為 FOODAWAY 支出變化的百分比效應。\n\n")  

## (e)
![image](https://github.com/user-attachments/assets/72e1525e-cf82-4121-a312-d43fc73f5128)

R Code:  
plot(cex5_small$income, cex5_small$lnfoodaway,  
     xlab = "income (以100美元為單位)",  
     ylab = "ln(FOODAWAY)",  
     main = "ln(FOODAWAY) 與 income 散佈圖及擬合線",  
     pch = 16, col = "blue")  
abline(lm_model, col = "red", lwd = 2)  
legend("topleft", legend = c("資料點", "擬合線"),  
       col = c("blue", "red"), pch = c(16, NA),  
       lty = c(NA, 1), lwd = c(NA, 2))  

## (f)
![image](https://github.com/user-attachments/assets/cf6bdc5a-1c58-4c3c-8c1e-9276f96b93a0)

R Code:  
residuals_model <- lm(lnfoodaway ~ income, data = cex5_small, na.action = na.exclude)  
plot(resid(residuals_model) ~ income, data = cex5_small,  
     xlab = "income (以100美元為單位)",  
     ylab = "殘差",  
     main = "income 與迴歸模型殘差圖",  
     pch = 16, col = "purple")  
abline(h = 0, lty = 2)  
print(resid(residuals_model))  

# 2.28
![image](https://github.com/user-attachments/assets/365021b9-0f66-44be-85ab-6c72f599c986)  
## (a)
![image](https://github.com/user-attachments/assets/24eb5bc8-1dd8-4217-99a7-f6a2334493ff)  
<img width="457" alt="image" src="https://github.com/user-attachments/assets/a3d921b4-3f42-4a9d-a2ae-37ecaad1ef1a" />  

R Code:  
summary_wage <- summary(cps5_small$wage)  
summary_educ <- summary(cps5_small$educ)  
cat("【Descriptive Statistics】\n")  
cat("WAGE Summary:\n"); print(summary_wage)  
cat("\nEDUC Summary:\n"); print(summary_educ)  

par(mfrow = c(1, 2))  
hist(cps5_small$wage, breaks = 10, col = "lightblue",  
     main = "Histogram of WAGE",  
     xlab = "WAGE (Hourly wage)")  
hist(cps5_small$educ, breaks = 10, col = "lightgreen",  
     main = "Histogram of EDUC",  
     xlab = "EDUC (Years of education)")  
par(mfrow = c(1, 1))  

## (b)
A. Residuals:  
<img width="371" alt="image" src="https://github.com/user-attachments/assets/d7cd8e54-203c-4887-995e-ce28fe586309" />  

B. Coefficients:  
<img width="377" alt="image" src="https://github.com/user-attachments/assets/826274f9-14ce-49b8-a39b-ba3fb92f2e7d" />  

C. Residual standard error: 13.55 on 1198 degrees of freedom  
D. Multiple R-squared: 0.2073  
E. Adjusted R-squared: 0.2067  
F. F-statistic: 313.3 on 1 and 1198 DF  
G. p-value: < 2.2e-16  

R Code:  
lm_linear <- lm(wage ~ educ, data = cps5_small)  
summary_lm_linear <- summary(lm_linear)  
cat("\n【Linear Regression Results】\n")  
print(summary_lm_linear)  

## (c)
![image](https://github.com/user-attachments/assets/fc59b678-a990-480f-a850-11c0e7be2516)

R Code:  
residuals_linear <- residuals(lm_linear)  
plot(cps5_small$educ, residuals_linear,  
     xlab = "EDUC (Years of education)",  
     ylab = "Residuals",   
     main = "Residuals vs. EDUC (Linear Model)",  
     pch = 16, col = "purple")  
abline(h = 0, lty = 2)  
cat("\n【Residual Plot】\n")  
cat("Examine the plot to see if residuals are randomly scattered (as expected under SR1–SR5).\n")  

## (d)
【 Regression for Males】
A. Residuals:  
<img width="374" alt="image" src="https://github.com/user-attachments/assets/f3d9d6bd-a581-4b1b-b01c-4b30b77dc332" />  
B. Coefficients:  
<img width="440" alt="image" src="https://github.com/user-attachments/assets/9ea6b343-a6f7-44f1-87e9-c8664255f547" />  
C. Residual standard error: 14.71 on 670 degrees of freedom  
D. Multiple R-squared: 0.1927  
E. Adjusted R-squared: 0.1915  
F. F-statistic: 160 on 1 and 670 DF  
G. p-value: < 2.2e-16  


【Regression for Females】
A. Residuals:  
<img width="388" alt="image" src="https://github.com/user-attachments/assets/58c8df7f-d8b8-4dcb-806b-39c4e1bf3ab7" />  
B. Coefficients:  
<img width="449" alt="image" src="https://github.com/user-attachments/assets/c41cf5cb-f077-4b06-9e59-c6527fb1d475" />  
C. Residual standard error: 11.5 on 526 degrees of freedom  
D. Multiple R-squared: 0.2764  
E. Adjusted R-squared: 0.275  
F.  F-statistic: 200.9 on 1 and 526 DF  
G. p-value: < 2.2e-16  

【Regression for Blacks】
A. Residuals:  
<img width="403" alt="image" src="https://github.com/user-attachments/assets/47ccfd97-6906-4554-aeb4-38f186badae6" />  
B. Coefficients:  
<img width="434" alt="image" src="https://github.com/user-attachments/assets/84b41856-4501-49f7-b533-f01d006ac981" />  
C. Residual standard error: 10.51 on 103 degrees of freedom  
D. Multiple R-squared: 0.1846  
E. Adjusted R-squared: 0.1767  
F. F-statistic: 23.32 on 1 and 103 DF  
G. p-value: 4.788e-06  

【Regression for Whites】
A. Residuals:  
<img width="437" alt="image" src="https://github.com/user-attachments/assets/12f5b0fb-62fd-4d36-9061-6e38992e3814" />   
B. Coefficients:  
<img width="440" alt="image" src="https://github.com/user-attachments/assets/19f5174f-f80e-498d-a299-89d4f7342971" />  
C. Residual standard error: 13.79 on 1093 degrees of freedom  
D. Multiple R-squared: 0.2072  
E. Adjusted R-squared: 0.2065   
F. F-statistic: 285.7 on 1 and 1093 DF  
G. p-value: < 2.2e-16  

R Code:  

*For Males  
lm_male <- lm(wage[which(cps5_small$female==0)] ~ educ[which(cps5_small$female==0)], data = cps5_small)  
summary_male <- summary(lm_male)  
cat("\n【 Regression for Males】\n")  
print(summary_male)  

*For Females  
lm_female <- lm(wage[which(cps5_small$female==1)] ~ educ[which(cps5_small$female==1)], data = cps5_small)  
summary_female <- summary(lm_female)  
cat("\n【Regression for Females】\n")  
print(summary_female)  

*For Blacks  
lm_black <- lm(wage[which(cps5_small$black==1)] ~ educ[which(cps5_small$black==1)], data = cps5_small)  
summary_black <- summary(lm_black)  
cat("\n【Regression for Blacks】\n")  
print(summary_black)  

*For Whites  
lm_white <- lm(wage[which(cps5_small$black==0)] ~ educ[which(cps5_small$black==0)], data = cps5_small)  
summary_white <- summary(lm_white)  
cat("\n【Regression for Whites】\n")  
print(summary_white)  

## (e)
【Quadratic Regression Results】
A. Residuals:  
<img width="404" alt="image" src="https://github.com/user-attachments/assets/d6c345a4-241b-4b92-9237-e17e01640f31" />
B. Coefficients:  
<img width="395" alt="image" src="https://github.com/user-attachments/assets/f05c39f4-6804-4a77-8dc9-6b3e0dcffdc1" />  
C. Residual standard error: 13.45 on 1198 degrees of freedom  
D. Multiple R-squared: 0.2194  
E. Adjusted R-squared: 0.2187  
F. F-statistic: 336.6 on 1 and 1198 DF  
G. p-value: < 2.2e-16  

Marginal effect at 12 years of education = 2.139216
Marginal effect at 16 years of education = 2.852288
*Compare these values with the linear model's estimated β2 (from part (b)) *The marginal effect in (b) for educ=12 and educ=16 are both 2.4

R Code:  
*估計二次迴歸模型並計算不同教育水準下的邊際效應  
lm_quad <- lm(wage ~ I(educ^2), data = cps5_small)  
#summary_lm_quad <- summary(lm_quad)  
cat("\n【Quadratic Regression Results】\n")  
#print(summary_lm_quad)  
summary(lm_quad)  

*Marginal effect of an extra year of education is the derivative:  
*dWAGE/dEDUC = 2 * α2 * EDUC  
alpha2 <- coef(lm_quad)["I(educ^2)"]  

*For a person with 12 years of education:  
marginal_effect_12 <- 2 * alpha2 * 12  
*For a person with 16 years of education:  
marginal_effect_16 <- 2 * alpha2 * 16    

cat("\nMarginal effect at 12 years of education =", marginal_effect_12, "\n")  
cat("Marginal effect at 16 years of education =", marginal_effect_16, "\n")  
cat("Compare these values with the linear model's estimated β2 (from part (b)).\n")  

## (f)
![image](https://github.com/user-attachments/assets/122a8142-94d8-4dcc-9177-10064dff612a)

R Code:  
plot(cps5_small$educ, cps5_small$wage,  
     xlab = "EDUC (Years of education)",  
     ylab = "WAGE (Hourly wage)",  
     main = "Fitted Lines: Linear vs. Quadratic Models",  
     pch = 16, col = "blue")  

educ_range <- seq(min(cps5_small$educ, na.rm = TRUE), max(cps5_small$educ, na.rm = TRUE), length.out = 300)  
#educ_range  
fitted_linear <- predict(lm_linear, newdata = data.frame(educ = educ_range))  
lines(educ_range, fitted_linear, col = "red", lwd = 2)  


fitted_quad <- predict(lm_quad, newdata = data.frame(educ = educ_range))  
lines(educ_range, fitted_quad, col = "green", lwd = 2, lty = 2)  
  
legend("topleft", legend = c("Data Points", "Linear Fit", "Quadratic Fit"),  
       col = c("blue", "red", "green"), pch = c(16, NA, NA),  
       lty = c(NA, 1, 2), lwd = c(NA, 2, 2))  

cat("\n【(f) Plot Comparison】\n")  
cat("Examine the plot to decide which model appears to fit the data better.\n")  
