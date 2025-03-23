#4.28 (a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("wa_wheat")

library(ggplot2)

# model1: YIELD_t = β0 + β1 TIME + e_t
model1 <- lm(northampton ~ time,data=wa_wheat)
summary(model1)

# model2: YIELD_t = α0 + α1 ln(TIME) + e_t
model2 <- lm(northampton ~ log(time), data = wa_wheat)
summary(model2)

# model3: YIELD_t = γ0 + γ1 TIME^2 + e_t
model3 <- lm(northampton ~ I(time^{2}), data = wa_wheat)
summary(model3)

# model4: ln(YIELD_t) = φ0 + φ1 TIME + e_t
model4 <- lm(log(northampton) ~ time, data = wa_wheat)
summary(model4)

# (i) fitted equation
par(mfrow=c(2,2))
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 1: YIELD ~ TIME", xlab = "TIME", ylab = "YIELD", pch = 16)
abline(model1, col = "purple", lwd = 2)
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 2: YIELD ~ ln(TIME)", xlab = "ln(TIME)", ylab = "YIELD", pch = 16)
lines(wa_wheat$time, fitted(model2), col="purple", lwd = 2)
plot(wa_wheat$time, wa_wheat$northampton, main = "Model 3: YIELD ~ TIME^{2}", xlab = "TIME^{2}", ylab = "YIELD", pch = 16)
lines(wa_wheat$time, fitted(model3), col="purple", lwd = 2)
plot(wa_wheat$time, log(wa_wheat$northampton), main = "Model 4: log(YIELD) ~ TIME", xlab = "TIME", ylab = "log(YIELD)", pch = 16)
abline(model4, col = "purple", lwd = 2)

# (ii) residuals
par(mfrow=c(2,2))
plot(model1$fitted.values, model1$residuals, main = "Model 1 Fitted vs Residuals")
abline(h = 0, col = "purple", lty = 2)
plot(model2$fitted.values, model2$residuals, main = "Model 2 Fitted vs Residuals")
abline(h = 0, col = "purple", lty = 2)
plot(model3$fitted.values, model3$residuals, main = "Model 3 Fitted vs Residuals")
abline(h = 0, col = "purple", lty = 2)
plot(model4$fitted.values, model4$residuals, main = "Model 4 Fitted vs Residuals")
abline(h = 0, col = "purple", lty = 2)

# 繪製QQ圖以檢查誤差的正態性
par(mfrow=c(2,2))
qqnorm(model1$residuals, main = "Model 1 QQ Plot")
qqline(model1$residuals)
qqnorm(model2$residuals, main = "Model 2 QQ Plot")
qqline(model2$residuals)
qqnorm(model3$residuals, main = "Model 3 QQ Plot")
qqline(model3$residuals)
qqnorm(model4$residuals, main = "Model 4 QQ Plot")
qqline(model4$residuals)

#(iii) error normality tests
cat("Model 1:\n"); print(shapiro.test(resid(model1)))
cat("Model 2:\n"); print(shapiro.test(resid(model2)))
cat("Model 3:\n"); print(shapiro.test(resid(model3)))
cat("Model 4:\n"); print(shapiro.test(resid(model4)))

#(iv) value for R^{2}
cat("R2 for Model 1: ", summary(model1)$r.squared, "\n")
cat("R2 for Model 2: ", summary(model2)$r.squared, "\n")
cat("R2 for Model 3: ", summary(model3)$r.squared, "\n")
cat("R2 for Model 4: ", summary(model4)$r.squared, "\n")

#4.28(c)
# 檢查標準化殘差
standardized_residuals <- rstandard(model3)
plot(standardized_residuals, main = "Standardized Residuals for Model 3")
abline(h = c(-2, 2), col = "purple")

# LEVERAGE, DFBETAS 和 DFFITS
leverage_values <- hatvalues(model3)
dfbetas_values <- dfbetas(model3)
dffits_values <- dffits(model3)

# 標記異常值
plot(leverage_values, main = "Leverage for Model 3")
abline(h = 2 * mean(leverage_values), col = "")

# 檢查 DFBETAS 和 DFFITS
plot(dfbetas_values[,1], main = "DFBETAS for Model 3")
plot(dffits_values, main = "DFFITS for Model 3")

#4.28(d)
# 預測1997年小麥產量（假設1997年是TIME的某個值）
predict_1997 <- predict(model3, newdata = data.frame(time = 48), interval = "prediction", level = 0.95)
cat("1997年小麥產量的95%預測區間：", predict_1997)
