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

## (b)

## (c)
![image](https://github.com/user-attachments/assets/76f64295-880f-4675-be02-852b851901cb)

## (d)

## (e)
![image](https://github.com/user-attachments/assets/72e1525e-cf82-4121-a312-d43fc73f5128)

## (f)
![image](https://github.com/user-attachments/assets/cf6bdc5a-1c58-4c3c-8c1e-9276f96b93a0)


# 2.28
![image](https://github.com/user-attachments/assets/365021b9-0f66-44be-85ab-6c72f599c986)
## (a)
![image](https://github.com/user-attachments/assets/24eb5bc8-1dd8-4217-99a7-f6a2334493ff)

## (b)

## (c)
![image](https://github.com/user-attachments/assets/fc59b678-a990-480f-a850-11c0e7be2516)

## (d)

## (e)

## (f)
![image](https://github.com/user-attachments/assets/122a8142-94d8-4dcc-9177-10064dff612a)

