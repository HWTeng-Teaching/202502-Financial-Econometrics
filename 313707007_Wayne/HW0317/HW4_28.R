#(a)
# 讀取資料
df <- read.csv("wa_wheat.csv")

# 載入繪圖套件
library(ggplot2)
library(gridExtra)

# 建立變數
YIELD <- df$northampton
TIME <- df$time
log_TIME <- log(TIME)
TIME2 <- TIME^2
log_YIELD <- log(YIELD)

# 建立模型
model1 <- lm(YIELD ~ TIME)
model2 <- lm(YIELD ~ log_TIME)
model3 <- lm(YIELD ~ TIME2)
model4 <- lm(log_YIELD ~ TIME)

# 預測值
df$fitted1 <- fitted(model1)
df$fitted2 <- fitted(model2)
df$fitted3 <- fitted(model3)
df$fitted4 <- fitted(model4)

# 圖 1：Linear (YIELD ~ TIME)
p1 <- ggplot(df, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted1), color = "blue") +
  ggtitle("Model 1: YIELD ~ TIME") +
  theme_minimal()

# 圖 2：Log-Time (YIELD ~ ln(TIME))
p2 <- ggplot(df, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted2), color = "green") +
  ggtitle("Model 2: YIELD ~ ln(TIME)") +
  theme_minimal()

# 圖 3：Quadratic (YIELD ~ TIME^2)
p3 <- ggplot(df, aes(x = TIME)) +
  geom_point(aes(y = YIELD), color = "black") +
  geom_line(aes(y = fitted3), color = "red") +
  ggtitle("Model 3: YIELD ~ TIME^2") +
  theme_minimal()

# 圖 4：Log-Yield (ln(YIELD) ~ TIME)
p4 <- ggplot(df, aes(x = TIME)) +
  geom_point(aes(y = log_YIELD), color = "black") +
  geom_line(aes(y = fitted4), color = "purple") +
  ggtitle("Model 4: ln(YIELD) ~ TIME") +
  theme_minimal()

# 排列圖形
grid.arrange(p1, p2, p3, p4, ncol = 2)

#(b)
# 計算殘差
df$resid1 <- resid(model1)
df$resid2 <- resid(model2)
df$resid3 <- resid(model3)
df$resid4 <- resid(model4)

# 殘差圖：Model 1
r1 <- ggplot(df, aes(x = TIME, y = resid1)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 1: Residuals (YIELD ~ TIME)") +
  theme_minimal()

# 殘差圖：Model 2
r2 <- ggplot(df, aes(x = TIME, y = resid2)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 2: Residuals (YIELD ~ ln(TIME))") +
  theme_minimal()

# 殘差圖：Model 3
r3 <- ggplot(df, aes(x = TIME, y = resid3)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 3: Residuals (YIELD ~ TIME^2)") +
  theme_minimal()

# 殘差圖：Model 4
r4 <- ggplot(df, aes(x = TIME, y = resid4)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Model 4: Residuals (ln(YIELD) ~ TIME)") +
  theme_minimal()

# 一次排出四張圖
grid.arrange(r1, r2, r3, r4, ncol = 2)

#(c)
# 載入必要套件
library(ggplot2)
library(gridExtra)
library(car)  # 用於異方差檢定
library(nortest)  # 提供更多常態性檢定
library(stats)  # 基本統計函數

# Shapiro-Wilk Normality Test（適用於 n < 5000）
shapiro1 <- shapiro.test(resid(model1))
shapiro2 <- shapiro.test(resid(model2))
shapiro3 <- shapiro.test(resid(model3))
shapiro4 <- shapiro.test(resid(model4))

# Anderson-Darling Normality Test
ad1 <- ad.test(resid(model1))
ad2 <- ad.test(resid(model2))
ad3 <- ad.test(resid(model3))
ad4 <- ad.test(resid(model4))

# Kolmogorov-Smirnov Test（使用常態分佈比較）
ks1 <- ks.test(resid(model1), "pnorm", mean(resid(model1)), sd(resid(model1)))
ks2 <- ks.test(resid(model2), "pnorm", mean(resid(model2)), sd(resid(model2)))
ks3 <- ks.test(resid(model3), "pnorm", mean(resid(model3)), sd(resid(model3)))
ks4 <- ks.test(resid(model4), "pnorm", mean(resid(model4)), sd(resid(model4)))

# 建立結果表
normality_results <- data.frame(
  Model = c("Linear (YIELD ~ TIME)", 
            "Log-Time (YIELD ~ ln(TIME))", 
            "Quadratic-Time (YIELD ~ TIME^2)", 
            "Log-Yield (ln(YIELD) ~ TIME)"),
  Shapiro_p = c(shapiro1$p.value, shapiro2$p.value, shapiro3$p.value, shapiro4$p.value),
  AD_p = c(ad1$p.value, ad2$p.value, ad3$p.value, ad4$p.value),
  KS_p = c(ks1$p.value, ks2$p.value, ks3$p.value, ks4$p.value)
)

# 顯示結果
print(normality_results)

# 繪製 Q-Q 圖
qq1 <- ggplot(data.frame(res = resid(model1)), aes(sample = res)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot: Model 1")

qq2 <- ggplot(data.frame(res = resid(model2)), aes(sample = res)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot: Model 2")

qq3 <- ggplot(data.frame(res = resid(model3)), aes(sample = res)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot: Model 3")

qq4 <- ggplot(data.frame(res = resid(model4)), aes(sample = res)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot: Model 4")

# 顯示 Q-Q 圖
grid.arrange(qq1, qq2, qq3, qq4, ncol = 2)
#(iv)
# 計算四個模型的 R^2 值
r2_model1 <- summary(model1)$r.squared
r2_model2 <- summary(model2)$r.squared
r2_model3 <- summary(model3)$r.squared
r2_model4 <- summary(model4)$r.squared

# 建立比較表
r2_results <- data.frame(
  Model = c("Linear (YIELD ~ TIME)", 
            "Log-Time (YIELD ~ ln(TIME))", 
            "Quadratic-Time (YIELD ~ TIME^2)", 
            "Log-Yield (ln(YIELD) ~ TIME)"),
  R2_Value = c(r2_model1, r2_model2, r2_model3, r2_model4)
)

# 顯示結果
print(r2_results)
#(b)
summary(model3)  # Quadratic-Time Model (YIELD ~ TIME^2)
#(c)
# 載入必要的套件
library(ggplot2)
library(gridExtra)

# 計算診斷統計量（使用所選的 Quadratic-Time Model）
student_resid <- rstudent(model3)  # 學生化殘差
leverage_values <- hatvalues(model3)  # 槓桿值
dfbetas_values <- apply(dfbetas(model3), 1, max)  # 取對所有變數影響最大的 DFBETAS
dffits_values <- dffits(model3)  # DFFITS

# 建立資料框
df_diag <- data.frame(
  TIME = df$time,
  Studentized_Residuals = student_resid,
  Leverage = leverage_values,
  DFBETAS = dfbetas_values,
  DFFITS = dffits_values
)

# 標準閾值
n <- nrow(df)  # 樣本數
p <- length(coef(model3)) - 1  # 變數數量
leverage_threshold <- 2 * (p + 1) / n
dfbetas_threshold <- 2 / sqrt(n)
dffits_threshold <- 2 * sqrt((p + 1) / n)

# 標記異常值
df_diag$Outlier_StudentResid <- abs(df_diag$Studentized_Residuals) > 2
df_diag$Outlier_Leverage <- df_diag$Leverage > leverage_threshold
df_diag$Outlier_DFBETAS <- df_diag$DFBETAS > dfbetas_threshold
df_diag$Outlier_DFFITS <- abs(df_diag$DFFITS) > dffits_threshold

# 繪製圖形
p1 <- ggplot(df_diag, aes(x = TIME, y = Studentized_Residuals)) +
  geom_point(aes(color = Outlier_StudentResid)) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  ggtitle("Studentized Residuals") +
  theme_minimal()

p2 <- ggplot(df_diag, aes(x = TIME, y = Leverage)) +
  geom_point(aes(color = Outlier_Leverage)) +
  geom_hline(yintercept = leverage_threshold, linetype = "dashed", color = "red") +
  ggtitle("Leverage Values") +
  theme_minimal()

p3 <- ggplot(df_diag, aes(x = TIME, y = DFBETAS)) +
  geom_point(aes(color = Outlier_DFBETAS)) +
  geom_hline(yintercept = c(-dfbetas_threshold, dfbetas_threshold), linetype = "dashed", color = "red") +
  ggtitle("DFBETAS") +
  theme_minimal()

p4 <- ggplot(df_diag, aes(x = TIME, y = DFFITS)) +
  geom_point(aes(color = Outlier_DFFITS)) +
  geom_hline(yintercept = c(-dffits_threshold, dffits_threshold), linetype = "dashed", color = "red") +
  ggtitle("DFFITS") +
  theme_minimal()

# 顯示圖表
grid.arrange(p1, p2, p3, p4, ncol = 2)

#(d)
# 訓練資料（1950–1996）
train_df <- df[df$time <= 47, ]
test_df <- df[df$time == 48, ]  # 1997 對應 time = 48

# 建立 Quadratic Model（YIELD ~ TIME + TIME^2）
train_df$time_sq <- train_df$time^2
model <- lm(northampton ~ time + time_sq, data = train_df)

# 預測資料：1997
new_time <- 48
new_data <- data.frame(time = new_time, time_sq = new_time^2)

# 建立 95% 預測區間
pred <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)

# 實際 1997 年的值
actual_1997 <- test_df$northampton

# 顯示結果
cat("predicted YIELD (1997):", round(pred[1], 4), "\n")
cat("95% Prediction Interval: [", round(pred[2], 4), ",", round(pred[3], 4), "]\n")
cat("Actual YIELD (1997):", round(actual_1997, 4), "\n")

# 判斷是否包含actual value
if (actual_1997 >= pred[2] && actual_1997 <= pred[3]) {
  cat("The actual value IS within the prediction interval.\n")
} else {
  cat("❌ The actual value is NOT within the prediction interval.\n")
}