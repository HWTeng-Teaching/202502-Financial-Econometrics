#C05Q23

##############################################################################
# 前置設定：清除環境、載入套件與資料
##############################################################################

rm(list = ls())  # 清除工作環境

# 載入必要套件
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(POE5Rdata)  # 資料內含 cocaine 資料集

# 載入資料
data("cocaine")

### 檢查資料結構（非必要，可保留檢查用）###
head(cocaine)
tail(cocaine)
nrow(cocaine)


# (b) 建立線性迴歸模型、產生回歸方程式並以表格呈現模型摘要
model <- lm(price ~ quant + qual + trend, data = cocaine)
coefs <- coef(model)
equation <- paste("price =", round(coefs[1], 2),
                  "+", round(coefs[2], 2), "*quant",
                  "+", round(coefs[3], 2), "*qual",
                  "+", round(coefs[4], 2), "*trend")
cat("回歸方程式：\n", equation, "\n\n")

# 產生模型摘要之表格 (係數、標準誤、t值、p值)
coeff_table <- as.data.frame(coef(summary(model)))
print(coeff_table)

# (c) 模型解釋力：R-squared 的數值與圖形呈現
r_squared <- summary(model)$r.squared
cat("\nModel R-squared =", round(r_squared, 4), "\n")
print("This means about 51% of the variation in price 
      is explained by the model(QUANT, QUAL, and TREND).")
print("*[51%] This suggests moderate explanatory power.")

# (d) 假設檢定：H₀: β₂ ≥ 0 vs. H₁: β₂ < 0 (銷量大價反低)
beta2 <- coef(summary(model))["quant", "Estimate"]
se2 <- coef(summary(model))["quant", "Std. Error"]
t2 <- beta2 / se2
pval2 <- pt(t2, df = model$df.residual)  # 單尾檢定（左尾）
cat("\n(d)[null Hypothsis] high sales, higher risk, willing to accept a 
    lower price：\nH₀: β₂ ≥ 0 vs. H₁: β₂ < 0\n")
cat("t value =", round(t2, 3), ", one-tailed p value =", round(pval2, 10), "\n")

# 利用 ggplot2 繪製 t 分布圖並標示 t2 值（左尾檢定）
t_seq <- seq(-4, 4, length.out = 300)
t_density <- dt(t_seq, df = model$df.residual)
df_t <- data.frame(t = t_seq, density = t_density)
p_d <- ggplot(df_t, aes(x = t, y = density)) +
  geom_line() +
  geom_area(data = subset(df_t, t <= t2), aes(x = t, y = density),
            fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = t2, color = "red", linetype = "dashed") +
  labs(title = "t Dist. - high sales, higher risk, willing to accept a 
       lower price (quant)",
       subtitle = paste("t value =", round(t2, 3),
                        ", p-value =", round(pval2, 10)),
       x = "t value", y = "Density") +
  theme_minimal()
print(p_d)


# (e) 假設檢定：H₀: β₃ ≤ 0 vs. H₁: β₃ > 0 (品質溢酬)
beta3 <- coef(summary(model))["qual", "Estimate"]
se3 <- coef(summary(model))["qual", "Std. Error"]
t3 <- beta3 / se3
pval3 <- 1 - pt(t3, df = model$df.residual)  # 單尾檢定（右尾）
cat("\n(e) 品質溢酬假設檢定：\nH₀: β₃ ≤ 0 vs. H₁: β₃ > 0\n")
cat("t 值 =", round(t3, 3), ", 單尾 p 值 =", round(pval3, 10), "\n")

# 利用 ggplot2 繪製 t 分布圖並標示 t3 值（右尾檢定）
t_seq2 <- seq(-4, 4, length.out = 300)
t_density2 <- dt(t_seq2, df = model$df.residual)
df_t2 <- data.frame(t = t_seq2, density = t_density2)
p_e <- ggplot(df_t2, aes(x = t, y = density)) +
  geom_line() +
  geom_area(data = subset(df_t2, t >= t3), aes(x = t, y = density),
            fill = "skyblue", alpha = 0.5) +
  geom_vline(xintercept = t3, color = "red", linetype = "dashed") +
  labs(title = "t Dist. - premium is paid for better-quality cocaine (qual)",
       subtitle = paste("t value =", round(t3, 3),
                        ", p-value =", round(pval3, 10)),
       x = "t value", y = "Density") +
  theme_minimal()
print(p_e)


# (f) 年度變化解釋：trend 係數代表每年價格平均變動量
trend_coef <- coef(model)["trend"]
cat("\n(f) average annual change in the cocaine price (β₄) =
    ", round(trend_coef, 2), "USD per year\n")

# 利用部分殘差圖呈現 trend 的效果
partial_res <- resid(model) + coef(model)["trend"] * cocaine$trend
p_f <- ggplot(cocaine, aes(x = trend, y = partial_res)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Partial Residual Plot for trend",
       subtitle = paste("Estimated annual change ="
                        , round(trend_coef, 2), "USD"),
       x = "trend", y = "Partial Residuals") +
  theme_minimal()
print(p_f)

