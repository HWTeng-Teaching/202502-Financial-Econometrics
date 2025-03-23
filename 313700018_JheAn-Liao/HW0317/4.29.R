rm(list = ls())

# 🔗 下載並載入 cex5_small 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cex5_small)
# 指定變數
FOOD   <- cex5_small$food
INCOME <- cex5_small$income
#install.packages("tseries")
#library(tseries)
#A####################################################
# A. 
# 1. 計算統計量
#FOOD statistic
mean_food   <- mean(FOOD)
median_food <- median(FOOD)
min_food    <- min(FOOD)
max_food    <- max(FOOD)
sd_food     <- sd(FOOD)
cat("Mean   =", mean_food, "\n")
cat("Median =", median_food, "\n")
cat("Min    =", min_food, "\n")
cat("Max    =", max_food, "\n")
cat("SD     =", sd_food, "\n\n")

#INCOME statistic
mean_inc   <- mean(INCOME)
median_inc <- median(INCOME)
min_inc    <- min(INCOME)
max_inc    <- max(INCOME)
sd_inc     <- sd(INCOME)

cat("Mean   =", mean_inc, "\n")
cat("Median =", median_inc, "\n")
cat("Min    =", min_inc, "\n")
cat("Max    =", max_inc, "\n")
cat("SD     =", sd_inc, "\n\n")

# 2. 繪製直方圖
par(mfrow = c(1, 2))

# (A) FOOD
hist(FOOD, 
     main = "Histogram of FOOD",
     xlab = "FOOD ($ per person per month)",
     col = "lightblue", border = "white")

# 在圖上標示平均(紅線) & 中位數(藍線)
abline(v = mean_food, col = "red", lwd = 2)
abline(v = median_food, col = "blue", lwd = 2)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"), lty = 1, lwd = 2, bty = "n")

# (B) INCOME
hist(INCOME, 
     main = "Histogram of INCOME",
     xlab = "INCOME ($100 per month)",
     col = "lightgreen", border = "white")

abline(v = mean_inc, col = "red", lwd = 2)
abline(v = median_inc, col = "blue", lwd = 2)
legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"), lty = 1, lwd = 2, bty = "n")

# 3. 觀察分配形狀 & 比較平均與中位數

#非對稱、非鐘型、平均數大於中位數

# 4. 執行Jarque-Bera test

jb_food   <- jarque.bera.test(FOOD)
jb_income <- jarque.bera.test(INCOME)

cat("\n=== Jarque-Bera Test for FOOD ===\n")
print(jb_food)

cat("\n=== Jarque-Bera Test for INCOME ===\n")
print(jb_income)

# p-value < 0.05 reject 常態性假設 樣本非常態分佈

#B#################################################### 
#B.
model1 = lm(food ~ income, data = cex5_small)
summary(model1)

plot(INCOME, FOOD,
     xlab = "INCOME ($100 per month)",
     ylab = "FOOD expenditure ($ per person per month)",
     main = "Scatter Plot of FOOD vs. INCOME with Fitted Line",
     pch = 16)
# 1. 加入迴歸線/信賴區間
abline(model1, col = "red", lwd = 2)
confint(model1, level = 0.95)

# 取得模型所有參數的 95% 信賴區間
ci_all <- confint(model1, level = 0.95)

# 印出 β₂ 的信賴區間
ci_beta2 <- ci_all[2, ]
print(ci_beta2)

#C####################################################
#C.
#計算殘差
resid_model1 <- resid(model1)
plot(cex5_small$income, resid_model1,
     xlab = "INCOME ($100 per month)",
     ylab = "Residuals",
     main = "Residuals vs. INCOME")
abline(h = 0, col = "red", lty = 10)
hist(resid_model1, breaks = 20,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue", border = "white")
jarque.bera.test(resid_model1)
#P value < 0.05 誤差項非常態
#迴歸模型的統計推論（如係數顯著性檢定、信賴區間等）大多基於「誤差項為常態」的假設
#或大樣本下近似成立
#反之，解釋變數 INCOME FOOD 本身是否常態，並非迴歸理論所要求。
#許多經濟或社會科學變數往往是偏態分配（如所得、消費），但這並不違反線性迴歸的核心假設。
#只要在給定解釋變數後，誤差項的分配是常態（或近似常態）且同方差、互相獨立，即可進行正確的推論。
#D####################################################
#D
# 迴歸係數
beta0 <- coef(model1)[1]
beta1 <- coef(model1)[2]
beta0
beta1
se_beta1 <- summary(model1)$coefficients[2,2]
se_beta1
# 斜率 beta1 的標準誤
se_beta1 <- summary(model1)$coefficients[2, 2]
se_beta1
elasticity_at <- function(x_value) {
  # fitted value at x_value
  food_hat <- beta0 + beta1 * x_value
  # point estimate of elasticity
  e_hat <- beta1 * (x_value / food_hat) 
  # approximate standard error (忽略截距與 food_hat 的隨機性)
  e_se <- se_beta1 * (x_value / food_hat)
  # 95% CI
  z_95 <- 1.96
  lower <- e_hat - z_95 * e_se
  upper <- e_hat + z_95 * e_se
  c(point_est = e_hat, ci_lower = lower, ci_upper = upper)
}
elasticity_at(19)
elasticity_at(65)
elasticity_at(100)
#並不大相同
#信賴區間方面，低收入（INCOME = 19）的區間與較高收入的區間不重疊，顯示低收入與較高收入下的彈性在統計上有顯著差異
#而 INCOME = 65 與 INCOME = 100 的信賴區間有部分重疊。

#從經濟學原則來看，對於必需品食物來說，雖然彈性應小於 1，但在低收入時受限於基本生存需求
#其彈性非常低；隨著收入上升，家庭有更多資源改善食品質量或增加食品消費，食物彈性相對會上升。

#E#####################################################
#E.
model_log <- lm(log(FOOD) ~ log(INCOME), data = cex5_small)
plot(log(cex5_small$income), log(cex5_small$food),
     xlab = "ln(INCOME)",
     ylab = "ln(FOOD)",
     main = "Log-Log model: ln(FOOD) vs ln(INCOME)",
     pch = 16)
abline(model_log, col = "red", lwd = 3)

# 取得線性模型的 R²
r2_linear <- summary(model1)$r.squared

# 取得 log-log 模型的 R²
r2_log <- summary(model_log)$r.squared

cat("線性模型 R² =", r2_linear, "\n")
cat("Log-Log 模型 R² =", r2_log, "\n")

#兩者的R squared 並無太大差別
#F#################################
#F
#所求即為log-log model 斜率的95%信賴區間
ci_log <- confint(model_log, level = 0.95)
print(ci_log)

elasticity_ci <- ci_log["log(INCOME)", ]
cat("Log-log 模型中 log(INCOME) 的彈性點估計為：", coef(model_log)["log(INCOME)"], "\n") +
cat("95% 信賴區間為：[", elasticity_ci[1], ",", elasticity_ci[2], "]\n")

#在中等收入水準情況下，linear model 跟 log log model 有部分重疊
#但在收入較極端情況下彈性並不相同
#G#####################
#G
# 提取殘差
resid_log <- resid(model_log)

# 取 ln(INCOME) 以便繪圖
ln_income <- log(cex5_small$income)
plot(ln_income, resid_log,
     xlab = "ln(INCOME)",
     ylab = "Residuals",
     main = "Residuals vs. ln(INCOME) for Log-Log Model",
     pch = 16)
abline(h = 0, col = "red", lty = 2)

hist(resid_log, breaks = 20,
     main = "Histogram of Residuals (Log-Log Model)",
     xlab = "Residuals",
     col = "lightblue", border = "white")

jb_test <- jarque.bera.test(resid_log)
jb_test

#p-value < 0.05 Reject Normality Assunption
#H##################
#H.
model_linlog <- lm(food ~ log(income), data = cex5_small)
plot(log(cex5_small$income), cex5_small$FOOD,
     xlab = "ln(INCOME)",
     ylab = "FOOD",
     main = "log - linear: FOOD vs ln(INCOME)",
     pch = 16)

# 加上最小平方法擬合線
abline(model_linlog, col = "red", lwd = 2)
r2_lin    <- summary(model1)$r.squared        # (b) 線性模型
r2_loglog <- summary(model_log)$r.squared     # (e) log–log 模型
r2_linlog <- summary(model_linlog)$r.squared  # (h) 線性–對數模型

cat("線性模型 R²         =", r2_lin, "\n") +
cat("log–log 模型 R²     =", r2_loglog, "\n") +
cat("線性–對數模型 R²    =", r2_linlog, "\n")

#就R squared 而言 線性模型最高
#在視覺上 log - linear 並無明顯線性趨勢
#I########################
#I
# 取得參數與標準誤
alpha0 <- coef(model_linlog)[1]
alpha1 <- coef(model_linlog)[2]
se_alpha1 <- summary(model_linlog)$coefficients[2, 2]

elasticity_at_linlog <- function(x_value) {
  # fitted value at x_value
  y_hat <- alpha0 + alpha1 * log(x_value)
  
  # point estimate of elasticity
  e_hat <- alpha1 / y_hat
  
  # approximate standard error (只考慮 alpha1 的隨機性)
  e_se <- se_alpha1 / y_hat
  
  # 95% CI
  z_95 <- 1.96
  lower <- e_hat - z_95 * e_se
  upper <- e_hat + z_95 * e_se
  
  c(point_est = e_hat, ci_lower = lower, ci_upper = upper)
}

# 測試收入在 19, 65, 160
vals <- c(19, 65, 160)
for (v in vals) {
  res <- elasticity_at_linlog(v)
  cat("\nAt INCOME =", v, "\n")
  print(res)
}

#J##############
#J
# 提取殘差
resid_linlog <- resid(model_linlog)

# 取 ln(INCOME) 以便繪圖
ln_income <- log(cex5_small$income)
plot(ln_income, resid_linlog,
     xlab = "ln(INCOME)",
     ylab = "Residuals",
     main = "Residuals vs ln(INCOME): Linear-Log Model",
     pch = 16)
abline(h = 0, col = "red", lty = 2)
#殘差直方圖
hist(resid_linlog, breaks = 20,
     main = "Histogram of Residuals (Linear-Log Model)",
     xlab = "Residuals",
     col = "lightblue", border = "white")
# J-B Test
jb_test_linlog <- jarque.bera.test(resid_linlog)
jb_test_linlog

# p value < 0.05 拒絕常態性假設
#K########################
#K
model_linear <- lm(food ~ income, data = cex5_small)
model_loglog <- lm(log(food) ~ log(income), data = cex5_small)
model_linlog <- lm(food ~ log(income), data = cex5_small)
fit_linear <- fitted(model_linear)
fit_loglog <- fitted(model_loglog)
fit_linlog <- fitted(model_linlog)
# 先建立 log–log 模型
#model_loglog <- lm(log(FOOD) ~ log(INCOME), data = cex5_small)
# 再提取模型的殘差，儲存在 res_loglog 中
res_loglog <- resid(model_loglog)

# 假設你已建立線性模型 model_linear
#model_linear <- lm(FOOD ~ INCOME, data = cex5_small)
# 提取線性模型的殘差，並儲存在 res_linear 變數中
res_linear <- resid(model_linear)
res_linlog <- resid(model_linlog)
# 若線性模型用的是 INCOME 本身
x_linear <- cex5_small$income

# 若 log–log 模型的自變數是 log(INCOME)
x_loglog <- log(cex5_small$income)

# 若線性–對數模型同樣是 log(INCOME)
x_linlog <- log(cex5_small$income)

# 第一張圖：回歸擬合圖
par(mfrow = c(1,3), mar = c(4,4,2,1))

## 1. 線性模型
plot(x_linear, cex5_small$food,
     main = "Linear Model: FOOD vs INCOME",
     xlab = "INCOME", ylab = "FOOD", pch = 16)
lines(sort(x_linear), fit_linear[order(x_linear)], col = "red", lwd = 2)

## 2. log–log 模型
# 需要把預測值 exp() 回到原尺度
plot(x_loglog, cex5_small$food,
     main = "Log-Log Model: FOOD vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "FOOD", pch = 16)
lines(sort(x_loglog), exp(fit_loglog[order(x_loglog)]), col = "blue", lwd = 2)

## 3. 線性–對數模型
plot(x_linlog, cex5_small$FOOD,
     main = "Linear-Log Model: FOOD vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "FOOD", pch = 16)
lines(sort(x_linlog), fit_linlog[order(x_linlog)], col = "green", lwd = 2)

# 第二張圖：殘差 vs. 解釋變數
par(mfrow = c(1,3), mar = c(4,4,2,1))

## 1. 線性模型殘差
plot(x_linear, res_linear,
     main = "Linear Model Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals", pch = 16)
abline(h = 0, col = "red", lty = 2)

## 2. log–log 模型殘差
plot(x_loglog, res_loglog,
     main = "Log-Log Model Residuals vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "blue", lty = 2)

## 3. 線性–對數模型殘差
plot(x_linlog, res_linlog,
     main = "Linear-Log Model Residuals vs ln(INCOME)",
     xlab = "ln(INCOME)", ylab = "Residuals", pch = 16)
abline(h = 0, col = "green", lty = 2)


# 第三張圖：殘差直方圖
par(mfrow = c(1,3), mar = c(4,4,2,1))

hist(res_linear,
     main = "Linear Model Residuals",
     xlab = "Residuals", col = "lightblue", border = "white")

hist(res_loglog,
     main = "Log-Log Model Residuals",
     xlab = "Residuals", col = "lightblue", border = "white")

hist(res_linlog,
     main = "Linear-Log Model Residuals",
     xlab = "Residuals", col = "lightblue", border = "white")
#log-log model 在殘差圖上平均數在視覺上較接近０ 直方圖也比較接近鐘型分配，偏鋒態較正常
#因此會prefer log log model

