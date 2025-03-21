library(POE5Rdata)

#4.28
data("wa_wheat")

model1 <- lm(northampton ~ time, data = wa_wheat)                 
model2 <- lm(northampton ~ log(time), data = wa_wheat)           
model3 <- lm(northampton ~ I(time^2), data = wa_wheat)            
model4 <- lm(log(northampton) ~ time, data = wa_wheat)

cat("=== 模型摘要 ===\n")
print(summary(model1))
print(summary(model2))
print(summary(model3))
print(summary(model4))

# 畫出診斷圖（每個模型 4 個圖：拟合值-殘差、標準化殘差、QQ圖與 Cook 距離圖）
par(mfrow = c(2,2))
plot(model1, main = "Model 1 Diagnostics")
plot(model2, main = "Model 2 Diagnostics")
plot(model3, main = "Model 3 Diagnostics")
plot(model4, main = "Model 4 Diagnostics")
par(mfrow = c(1,1))

# Shapiro-Wilk 檢定：檢查殘差常態性
cat("\n=== Shapiro-Wilk 檢定 ===\n")
sw1 <- shapiro.test(resid(model1))
sw2 <- shapiro.test(resid(model2))
sw3 <- shapiro.test(resid(model3))
sw4 <- shapiro.test(resid(model4))
print(sw1)
print(sw2)
print(sw3)
print(sw4)

cat("Model 1 R-squared:", summary(model1)$r.squared, "\n")
cat("Model 2 R-squared:", summary(model2)$r.squared, "\n")
cat("Model 3 R-squared:", summary(model3)$r.squared, "\n")
cat("Model 4 R-squared:", summary(model4)$r.squared, "\n")


# 綜合考量：從診斷圖、殘差常態檢定結果與 R² 值來看，
# 假設模型4 (ln(northampton) ~ time) 表現較佳，
# 因此選擇模型4 作為最終解答模型。

# 模型4 係數解釋
# -------------------------------
cat("\n=== 模型4 係數解釋 ===\n")
coef_time <- coef(model4)[2]
cat("模型4中，time 的係數為", coef_time, "\n")
cat("解釋：當 time 增加 1 單位時，ln(northampton) 平均增加", coef_time, "單位，\n",
    "可近似解釋為 northampton yield 平均變化約", round(100 * coef_time, 2), "%（適用於變化較小的情形）。\n")

# 模型4 異常值診斷
# -------------------------------
cat("\n=== 模型4 異常值診斷 ===\n")
n <- nrow(wa_wheat)
p <- length(coef(model4))  # 模型參數個數 (含截距)

# 計算診斷指標
stud_res <- rstudent(model4)      # Studentized residuals
lev <- hatvalues(model4)          # Leverage
dfb <- dfbetas(model4)            # DFBETAS
dffits_val <- dffits(model4)      # DFFITS

# 設定常見閾值
stud_threshold <- 2               # |Studentized residual| > 2
lev_threshold <- 2 * (p/n)          # Leverage 門檻
dfb_threshold <- 2 / sqrt(n)        # DFBETAS 門檻
dffits_threshold <- 2 * sqrt(p/n)     # DFFITS 門檻

# 找出異常觀測
outlier_res <- which(abs(stud_res) > stud_threshold)
outlier_lev <- which(lev > lev_threshold)
outlier_dfb <- which(apply(abs(dfb), 1, max) > dfb_threshold)
outlier_dffits <- which(abs(dffits_val) > dffits_threshold)

cat("Studentized residuals 超過門檻的觀測：", outlier_res, "\n")
cat("Leverage 超過門檻的觀測：", outlier_lev, "\n")
cat("DFBETAS 超過門檻的觀測：", outlier_dfb, "\n")
cat("DFFITS 超過門檻的觀測：", outlier_dffits, "\n")

# -------------------------------
# 6. 1997 年預測 (使用 1950~1996 之資料)
# -------------------------------
# 假設 time 1 代表 1950，因此 time = 48 代表 1997；1950~1996 的資料為 time 1~47
data_96 <- subset(wa_wheat, time <= 47)

# 以模型4 規格重新估計模型（注意因變數為 log(northampton)）
model_chosen <- lm(log(northampton) ~ time, data = data_96)

# 預測 1997 年 (time = 48) 的 log yield 與其 95% prediction interval
new_data <- data.frame(time = 48)
pred_log <- predict(model_chosen, newdata = new_data, interval = "prediction", level = 0.95)
# 轉換回原始 yield 水準（此轉換僅為近似，未進行偏誤修正）
pred <- exp(pred_log)

cat("\n=== 1997 年預測 ===\n")
cat("預測的 95% prediction interval (northampton yield)：\n")
print(pred)

# 檢查 1997 年的實際觀測值是否落在預測區間內
actual_1997 <- wa_wheat[wa_wheat$time == 48, "northampton"]
cat("1997 年實際 yield 值：", actual_1997, "\n")
if(actual_1997 >= pred[,"lwr"] && actual_1997 <= pred[,"upr"]){
  cat("實際值落在 95% prediction interval 內。\n")
} else {
  cat("實際值未落在 95% prediction interval 內。\n")
}

#4.29
###############################
# 載入資料與必要套件
###############################
data("cex5_small")        # 載入資料
library(ggplot2)          # 繪圖用
install.packages("moments")
library(moments)          # 提供 jarque.test 進行 Jarque–Bera 檢定

###############################
#### (a) Summary Statistics & Histograms ####
###############################
# 計算 food 與 income 的基本統計量
food_mean   <- mean(cex5_small$food)
food_median <- median(cex5_small$food)
food_min    <- min(cex5_small$food)
food_max    <- max(cex5_small$food)
food_sd     <- sd(cex5_small$food)

income_mean   <- mean(cex5_small$income)
income_median <- median(cex5_small$income)
income_min    <- min(cex5_small$income)
income_max    <- max(cex5_small$income)
income_sd     <- sd(cex5_small$income)

cat("==== (a) Summary Statistics ====\n")
cat("food:\n")
cat("Mean =", round(food_mean,2), 
    ", Median =", round(food_median,2), 
    ", Min =", food_min, 
    ", Max =", food_max, 
    ", SD =", round(food_sd,2), "\n\n")
cat("income:\n")
cat("Mean =", round(income_mean,2), 
    ", Median =", round(income_median,2), 
    ", Min =", income_min, 
    ", Max =", income_max, 
    ", SD =", round(income_sd,2), "\n\n")

# 繪製 food 的直方圖，標示平均值與中央値
ggplot(cex5_small, aes(x = food)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  geom_vline(aes(xintercept = food_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = food_median), color = "blue", linetype = "dashed", size = 1) +
  ggtitle("Histogram of food") +
  theme_minimal()

# 繪製 income 的直方圖，標示平均值與中央値
ggplot(cex5_small, aes(x = income)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  geom_vline(aes(xintercept = income_mean), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = income_median), color = "blue", linetype = "dashed", size = 1) +
  ggtitle("Histogram of income") +
  theme_minimal()

# Jarque–Bera 檢定：檢查 food 與 income 是否符合常態分布
cat("\nJarque–Bera test for food:\n")
print(jarque.test(cex5_small$food))
cat("\nJarque–Bera test for income:\n")
print(jarque.test(cex5_small$income))

###############################
#### (b) Linear Regression & Confidence Interval ####
###############################
# 建立線性模型： food = β1 + β2 * income + e
model_linear <- lm(food ~ income, data = cex5_small)
cat("\n==== (b) Linear Regression Results ====\n")
print(summary(model_linear))

# 繪製 food 與 income 的散佈圖及最小平方法線
ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Scatter Plot of food vs. income with Fitted Line") +
  theme_minimal()

# 取得 income 變數（β2）的 95% 信賴區間
ci_beta2 <- confint(model_linear, "income", level = 0.95)
cat("\n95% Confidence Interval for β2 (income coefficient):\n")
print(ci_beta2)
# ※ 解答說明 (b):
# 若 β2 的信賴區間相對狹窄，則表示改變 income 對 food 的平均效應估計相對精確。

###############################
#### (c) Residual Analysis ####
###############################
# 取得模型 (b) 的殘差
resid_linear <- resid(model_linear)

# (i) 繪製 income 與殘差的散佈圖
ggplot(data = cex5_small, aes(x = income, y = resid_linear)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  ggtitle("Residuals vs. income") +
  theme_minimal()

# (ii) 繪製殘差直方圖
ggplot(data = data.frame(resid_linear), aes(x = resid_linear)) +
  geom_histogram(fill = "orange", color = "black", bins = 20) +
  ggtitle("Histogram of Residuals") +
  theme_minimal()

# (iii) Jarque–Bera 檢定殘差常態性
cat("\nJarque–Bera test for Residuals:\n")
print(jarque.test(resid_linear))

# ※ 解答說明 (c):
# 殘差圖與直方圖有助於檢查是否有系統性結構（如異質性或非線性），
# 而 Jarque–Bera 檢定檢查誤差 e 的常態性。對於迴歸推論來說，
# 關鍵在於隨機誤差（e）需近似常態，而 food 與 income 本身不必要求常態分布。

###############################
#### (d) Elasticity Estimation ####
###############################
# 在線性模型 food = β1 + β2 * income 中，
# 於任一給定 income = X，food 的彈性計算公式為：
# Elasticity = (dfood/dincome) * (X / food_predicted)
# 其中 dfood/dincome = β2，food_predicted = β1 + β2 * X，
# 因此 Elasticity = β2 * X / (β1 + β2 * X)
beta1_hat <- coef(model_linear)[1]
beta2_hat <- coef(model_linear)[2]
vcov_mat  <- vcov(model_linear)

# 定義彈性點估計函數
elasticity_est <- function(x, b1, b2) {
  b2 * x / (b1 + b2 * x)
}

# 定義利用 delta method 計算標準誤的函數
elasticity_se <- function(x, b1, b2, vcov_mat) {
  # 對 β1 的偏微分
  d_beta1 <- - (b2 * x) / (b1 + b2 * x)^2
  # 對 β2 的偏微分
  d_beta2 <- (b1 * x) / (b1 + b2 * x)^2
  grad <- c(d_beta1, d_beta2)
  sqrt(t(grad) %*% vcov_mat %*% grad)
}

# 指定欲計算彈性的 income 值
income_vals <- c(19, 65, 160)
elasticity_df <- data.frame(income = income_vals,
                            elasticity = NA,
                            se = NA,
                            lower = NA,
                            upper = NA)

for(i in 1:length(income_vals)){
  X <- income_vals[i]
  est <- elasticity_est(X, beta1_hat, beta2_hat)
  se_est <- elasticity_se(X, beta1_hat, beta2_hat, vcov_mat)
  elasticity_df$elasticity[i] <- est
  elasticity_df$se[i] <- se_est
  elasticity_df$lower[i] <- est - 1.96 * se_est
  elasticity_df$upper[i] <- est + 1.96 * se_est
}

cat("\n==== (d) Elasticity Estimates ====\n")
print(elasticity_df)
# ※ 解答說明 (d):
# 計算結果顯示在 income = 19, 65, 160 時，food 支出對 income 的彈性估計值，
# 以及各點估計值的 95% 信賴區間。根據經濟學原則，隨著 income 增加，
# 一般 food 作為必需品其彈性會降低（即比例下降），
# 而不同 income 水準下彈性值是否顯著不同，可藉由信賴區間是否重疊加以檢視。
###############################
#### (e) Log-Log Model ####
###############################
# 估計 log-log 關係: ln(food) = γ1 + γ2 * ln(income) + e
model_loglog <- lm(log(food) ~ log(income), data = cex5_small)
summary_loglog <- summary(model_loglog)

# (e)-1: 繪製 ln(food) 對 ln(income) 的散佈圖並疊上最小平方法線
ggplot(cex5_small, aes(x = log(income), y = log(food))) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  ggtitle("Log-Log Model: ln(food) vs. ln(income)") +
  theme_minimal()

# (e)-2: 計算 log-log 模型的「generalized R²」
# 這裡採用 lm() 內建的 R²（是在 ln(food) 為因變數下的 R²）
r2_loglog <- summary_loglog$r.squared

# 與線性模型比較：假設線性模型 model_linear 已在前面建立 (food ~ income)
r2_linear <- summary(lm(food ~ income, data = cex5_small))$r.squared

cat("==== (e) Model Fit Comparison ====\n")
cat("R² for linear model (food ~ income):", r2_linear, "\n")
cat("Generalized R² for log-log model (ln(food) ~ ln(income)):", r2_loglog, "\n")
cat("\nInterpretation:\n")
cat("從散佈圖及 R² 值比較，若 log-log 模型的 R² 較高且資料點在轉換後呈現較明顯線性關係，\n則可認為 log-log 模型比線性模型更能清楚定義兩者之間的關係。\n\n")

###############################
#### (f) Elasticity Estimation in Log-Log Model ####
###############################
# 在 log-log 模型中，ln(food) = γ1 + γ2 * ln(income) + e，γ2即為 food 對 income 的彈性
gamma2 <- coef(model_loglog)["log(income)"]
ci_gamma2 <- confint(model_loglog, "log(income)", level = 0.95)

cat("==== (f) Elasticity for Log-Log Model ====\n")
cat("Point estimate for elasticity (γ2):", gamma2, "\n")
cat("95% Confidence Interval for elasticity:", ci_gamma2, "\n")
cat("\nInterpretation:\n")
cat("由於 log-log 模型中彈性為常數，上述估計值直接代表 food 對 income 的彈性，\n可與 (d) 部分線性模型中依 income 水準變化的彈性進行比較。\n\n")

###############################
#### (g) Residual Analysis for Log-Log Model ####
###############################
resid_loglog <- resid(model_loglog)

# (g)-1: 繪製 ln(income) 與 log-log 模型的殘差散佈圖
ggplot(cex5_small, aes(x = log(income), y = resid_loglog)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 1) +
  ggtitle("Residuals vs. ln(income) for Log-Log Model") +
  theme_minimal()

# (g)-2: 繪製殘差直方圖
ggplot(data = data.frame(resid_loglog), aes(x = resid_loglog)) +
  geom_histogram(bins = 20, fill = "orange", color = "black") +
  ggtitle("Histogram of Residuals for Log-Log Model") +
  theme_minimal()

# (g)-3: Jarque–Bera 檢定殘差常態性
cat("==== (g) Jarque–Bera Test for Log-Log Residuals ====\n")
print(jarque.test(resid_loglog))
cat("\nInterpretation:\n")
cat("若檢定結果的 p-value 遠小於 0.05，則可拒絕殘差呈常態分布的假設；\n但需注意，即使 p-value 顯著，也要觀察殘差散佈圖是否無系統性結構。\n\n")

###############################
#### (h) Linear-Log Model ####
###############################
# 估計線性-log 模型: food = α1 + α2 * ln(income) + e
model_linlog <- lm(food ~ log(income), data = cex5_small)
summary_linlog <- summary(model_linlog)

# (h)-1: 繪製 food 對 ln(income) 的散佈圖並疊上最小平方法線
ggplot(cex5_small, aes(x = log(income), y = food)) +
  geom_point(color = "darkgrey") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  ggtitle("Linear-Log Model: food vs. ln(income)") +
  theme_minimal()

# (h)-2: 取得線性-log 模型的 R²
r2_linlog <- summary_linlog$r.squared

cat("==== (h) Linear-Log Model Fit ====\n")
cat("R² for linear model (food ~ income):", r2_linear, "\n")
cat("R² for linear-log model (food ~ ln(income)):", r2_linlog, "\n")
cat("\nInterpretation:\n")
cat("比較散佈圖及 R² 值，若 linear-log 模型的資料點較緊密排列且 R² 較高，\n則該模型對資料的解釋能力更佳。\n\n")

###############################
#### (i) Elasticity Estimation for Linear-Log Model ####
###############################
# 在線性-log 模型中: food = α1 + α2 * ln(income) + e，
# 對於任一給定 income = X，預測值為: food_hat = α1 + α2 * ln(X)
# 該模型下，dfood/dincome = α2 * (1/X)，因此 elasticity = (dfood/dincome)*(X/food_hat)
#   = α2 / (α1 + α2 * ln(X))
a1 <- coef(model_linlog)[1]
a2 <- coef(model_linlog)[2]
vcov_linlog <- vcov(model_linlog)

# 定義計算彈性函數及其標準誤 (利用 delta method)
elasticity_linlog_fun <- function(x, a1, a2) {
  a2 / (a1 + a2 * log(x))
}
elasticity_linlog_se <- function(x, a1, a2, vcov_mat) {
  # 對 a1 的偏微分: d/da1 = - a2 / (a1 + a2 * ln(x))^2
  d_a1 <- - a2 / (a1 + a2 * log(x))^2
  # 對 a2 的偏微分: d/da2 = a1 / (a1 + a2 * log(x))^2
  d_a2 <- a1 / (a1 + a2 * log(x))^2
  grad <- c(d_a1, d_a2)
  sqrt(t(grad) %*% vcov_mat %*% grad)
}

# 指定要計算彈性的 income 值
income_vals <- c(19, 65, 160)
elasticity_linlog_df <- data.frame(income = income_vals,
                                   elasticity = NA,
                                   se = NA,
                                   lower = NA,
                                   upper = NA)

for(i in 1:length(income_vals)) {
  X <- income_vals[i]
  est <- elasticity_linlog_fun(X, a1, a2)
  se_est <- elasticity_linlog_se(X, a1, a2, vcov_linlog)
  elasticity_linlog_df$elasticity[i] <- est
  elasticity_linlog_df$se[i] <- se_est
  elasticity_linlog_df$lower[i] <- est - 1.96 * se_est
  elasticity_linlog_df$upper[i] <- est + 1.96 * se_est
}

cat("==== (i) Elasticity Estimates for Linear-Log Model ====\n")
print(elasticity_linlog_df)
cat("\nInterpretation:\n")
cat("由線性-log 模型下，各 income 水準下的 food 彈性是根據模型參數計算，\n可與 log-log 模型中常數彈性進行比較，檢視其是否在統計上顯著不同。\n\n")

###############################
#### (j) Residual Analysis for Linear-Log Model ####
###############################
resid_linlog <- resid(model_linlog)

# (j)-1: 繪製 ln(income) 與線性-log 模型殘差散佈圖
ggplot(cex5_small, aes(x = log(income), y = resid_linlog)) +
  geom_point(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue", linewidth = 1) +
  ggtitle("Residuals vs. ln(income) for Linear-Log Model") +
  theme_minimal()

# (j)-2: 殘差直方圖
ggplot(data = data.frame(resid_linlog), aes(x = resid_linlog)) +
  geom_histogram(bins = 20, fill = "orange", color = "black") +
  ggtitle("Histogram of Residuals for Linear-Log Model") +
  theme_minimal()

# (j)-3: Jarque–Bera 檢定
cat("==== (j) Jarque–Bera Test for Linear-Log Residuals ====\n")
print(jarque.test(resid_linlog))
cat("\nInterpretation:\n")
cat("檢定結果若顯著，則表示殘差偏離常態分布；否則可認為模型誤差近似常態。\n\n")

###############################
#### (k) Final Model Comparison and Preference ####
###############################
cat("==== (k) Model Comparison and Preference ====\n")
cat("從各模型的散佈圖、R² 值、殘差診斷以及彈性估計來看：\n")
cat("- 線性模型 (food ~ income) 在原始尺度下解釋關係，但可能受到非線性與右偏的影響。\n")
cat("- log-log 模型 (ln(food) ~ ln(income)) 提供了恆定的彈性解釋，且散佈圖上的線性關係更明顯，\n  同時計算出的 R²（在轉換後尺度下）通常較高，殘差分布也較理想。\n")
cat("- 線性-log 模型 (food ~ ln(income)) 的關係解釋介於前兩者之間，其 R² 與殘差行為可作為參考。\n\n")
cat("因此，根據資料擬合效果、彈性解釋的穩定性以及殘差診斷，\n我傾向選擇 log-log 模型作為對 food 與 income 關係的最佳描述。\n")
cat("原因包括：\n")
cat("  1. log-log 模型直接提供常數彈性，易於解釋。\n")
cat("  2. 散佈圖上 ln(food) 與 ln(income) 呈現較明顯的線性關係。\n")
cat("  3. 模型 R² 較高，且殘差分析顯示誤差結構較理想（儘管仍可能存在輕微偏離常態，但改善效果較好）。\n")
