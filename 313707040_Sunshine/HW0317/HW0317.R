#CH03Q1
# 給定的回歸係數與標準誤
b1 <- 7.61733
b2 <- 0.01309  # 斜率
se_b2 <- 0.00215  # b2 的標準誤
n <- 64  # 樣本數

# (a) 設定虛無與對立假設
# H0: β2 = 0 （GDP 對獎牌數無影響）
# H1: β2 > 0 （GDP 對獎牌數有正向影響）

# (b) 計算 t 統計量
t_statistic <- b2 / se_b2
t_statistic

# (c) 若 H0 為真，t 統計量服從 t 分布（自由度 n-2）
df <- n - 2
# 若 H1 為真，則 t 統計量的值應該是正的

# (d) 計算 1% 顯著水準下的臨界值（單尾檢定）
alpha <- 0.01
t_critical <- qt(1 - alpha, df)
t_critical

# (e) 進行 t 檢定
p_value <- 1 - pt(t_statistic, df)
p_value

# 判斷是否拒絕 H0
if (p_value < alpha) {
  conclusion <- "拒絕 H0，GDP 與獎牌數有顯著正相關。"
} else {
  conclusion <- "無法拒絕 H0，GDP 與獎牌數可能無顯著關係。"
}

conclusion


#CH03Q7
# 已知回歸結果
bachelor_coef <- 1.029  # 斜率
se_bachelor <- bachelor_coef / 10.75  # 從 t 值計算標準誤 (t = coef / se)
intercept_se <- 2.672  # 截距的標準誤
n <- 51  # 樣本數
df <- n - 2  # 自由度

# (a) 計算截距估計值
# t_statistic_intercept = (intercept - 10) / intercept_se
# 反推截距值：
t_statistic_intercept <- 4.31
intercept <- t_statistic_intercept * intercept_se 
intercept

# (b) 繪製回歸關係圖
library(ggplot2)
bachelor_values <- seq(0, 50, by = 1)  # 假設 BACHELOR 介於 0 到 50%
income_values <- intercept + bachelor_coef * bachelor_values

ggplot(data.frame(BACHELOR = bachelor_values, INCOME = income_values), aes(x = BACHELOR, y = INCOME)) +
  geom_line(color = "blue") +
  labs(title = "Estimated Relationship: INCOME vs. BACHELOR",
       x = "Percentage with Bachelor's Degree",
       y = "Income per Capita (in $1000s)") +
  theme_minimal()

# (c) 計算斜率的標準誤
se_bachelor

# (d) 計算 t 值來檢定截距是否等於 10
t_intercept <- (intercept - 10) / intercept_se
t_intercept

# (e) 計算 p 值（雙尾檢定）
p_value_intercept <- 2 * (1 - pt(abs(t_intercept), df))
p_value_intercept

# 繪製 t 分佈與檢定區域
t_values <- seq(-4, 4, length.out = 1000)
density_values <- dt(t_values, df)

ggplot(data.frame(t = t_values, density = density_values), aes(x = t, y = density)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = qt(0.025, df), linetype = "dashed", color = "red") +
  geom_vline(xintercept = qt(0.975, df), linetype = "dashed", color = "red") +
  geom_vline(xintercept = qt(p_value_intercept, df), linetype = "solid", color = "black") +
  labs(title = "t-distribution with Rejection Regions",
       x = "t-value",
       y = "Density") +
  theme_minimal()

# (f) 建立 99% 信賴區間
alpha <- 0.01
t_critical <- qt(1 - alpha/2, df)
ci_lower <- bachelor_coef - t_critical * se_bachelor
ci_upper <- bachelor_coef + t_critical * se_bachelor
c(ci_lower, ci_upper)

# (g) 進行假設檢定 H0: β1 = 1 vs. H1: β1 ≠ 1
t_slope <- (bachelor_coef - 1) / se_bachelor
t_critical_5 <- qt(0.975, df)  # 5% 顯著性水準對應的 t 臨界值

# 判斷是否拒絕 H0
if (abs(t_slope) > t_critical_5) {
  conclusion <- "拒絕 H0，BACHELOR 變數的斜率顯著不同於 1。"
} else {
  conclusion <- "無法拒絕 H0，BACHELOR 變數的斜率可能為 1。"
}

list(
  intercept = intercept,
  t_statistic_intercept = t_intercept,
  ci_slope = c(ci_lower, ci_upper),
  t_statistic_slope = t_slope,
  t_critical_5 = t_critical_5,
  conclusion = conclusion
)


#CH03Q17
# 都市回歸模型
urban_slope <- 2.46  # 教育年數的斜率估計值
urban_se_slope <- 0.16  # 斜率的標準誤
urban_intercept <- -10.76  # 截距估計值
urban_se_intercept <- 2.27  # 截距的標準誤
urban_n <- 986  # 樣本數
urban_df <- urban_n - 2  # 自由度

# 鄉村回歸模型
rural_slope <- 1.80  # 教育年數的斜率估計值
rural_se_slope <- 0.24  # 斜率的標準誤
rural_intercept <- -4.88  # 截距估計值
rural_se_intercept <- 3.29  # 截距的標準誤
rural_n <- 214  # 樣本數
rural_df <- rural_n - 2  # 自由度

# (a) 假設檢定 H0: urban_slope = 1.80 vs. H1: urban_slope > 1.80，顯著水準 α = 0.05
t_stat_urban <- (urban_slope - 1.80) / urban_se_slope  # 計算 t 值
t_critical_urban <- qt(0.95, urban_df)  # 右尾檢定的臨界值
reject_h0_urban <- t_stat_urban > t_critical_urban  # 判斷是否拒絕 H0

# (b) 鄉村回歸模型下，計算當 EDUC = 16 時 WAGE 的 95% 信賴區間
educ_value <- 16
rural_wage_hat <- rural_intercept + rural_slope * educ_value  # 預測工資
rural_se_wage <- sqrt(rural_se_intercept^2 + 2 * (-0.761) * educ_value + (rural_se_slope^2) * (educ_value^2))  # 修正標準誤計算
t_critical_rural <- qt(0.975, rural_df)  # 95% 信賴區間的 t 值
ci_rural <- c(rural_wage_hat - t_critical_rural * rural_se_wage,
              rural_wage_hat + t_critical_rural * rural_se_wage)  # 信賴區間
ci_rural
# (c) 都市回歸模型下，計算當 EDUC = 16 時 WAGE 的 95% 信賴區間
urban_wage_hat <- urban_intercept + urban_slope * educ_value  # 預測工資
urban_se_wage <- sqrt(urban_se_intercept^2 + 2 * (-0.345) * educ_value + (urban_se_slope^2) * (educ_value^2))  # 修正標準誤計算
t_critical_urban <- qt(0.975, urban_df)
ci_urban <- c(urban_wage_hat - t_critical_urban * urban_se_wage,
              urban_wage_hat + t_critical_urban * urban_se_wage)  # 信賴區間
ci_urban
# (d) 假設檢定 H0: rural_intercept >= 4 vs. H1: rural_intercept < 4，顯著水準 α = 0.01
t_stat_rural_intercept <- (rural_intercept - 4) / rural_se_intercept  # 計算 t 值
t_critical_rural_intercept <- qt(0.01, rural_df)  # 左尾檢定的臨界值
reject_h0_rural_intercept <- t_stat_rural_intercept < t_critical_rural_intercept  # 判斷是否拒絕 H0

# 結果輸出
list(
  t_stat_urban = t_stat_urban,  # 都市回歸的 t 值
  reject_h0_urban = reject_h0_urban,  # 是否拒絕都市回歸的 H0
  ci_rural = ci_rural,  # 鄉村回歸的 95% 信賴區間
  ci_urban = ci_urban,  # 都市回歸的 95% 信賴區間
  t_stat_rural_intercept = t_stat_rural_intercept,  # 鄉村回歸截距的 t 值
  reject_h0_rural_intercept = reject_h0_rural_intercept  # 是否拒絕鄉村回歸的 H0
)


#CH03Q19
library(POE5Rdata)
data(motel)

# (a) 繪製 MOTEL_PCT 和 COMP_PCT 對 TIME 的圖
library(ggplot2)
ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Motel and Competitor Occupancy over Time", x = "Time", y = "Occupancy Percentage") +
  scale_color_manual(values = c("Motel Occupancy" = "blue", "Competitor Occupancy" = "red"))

# 進行線性回歸
model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

# 95% 信賴區間
confint(model, level = 0.95)

# (b) 計算 90% 信賴區間 (給定 COMP_PCT = 70)
comp_pct_value <- 70
pred <- predict(model, newdata = data.frame(comp_pct = comp_pct_value), interval = "confidence", level = 0.90)
pred

# (c) 假設檢定 H0: β2 <= 0 vs H1: β2 > 0, 顯著水準 α = 0.01
t_stat <- summary(model)$coefficients[2, "t value"]
df <- summary(model)$df[2]
t_critical <- qt(0.99, df)  # 右尾檢定
reject_h0 <- t_stat > t_critical
list(t_statistic = t_stat, critical_value = t_critical, reject_H0 = reject_h0)

# (d) 假設檢定 H0: β2 = 1 vs H1: β2 ≠ 1, α = 0.01
t_stat_1 <- (summary(model)$coefficients[2, "Estimate"] - 1) / summary(model)$coefficients[2, "Std. Error"]
t_critical_2 <- qt(0.995, df)  # 兩尾檢定
reject_h0_1 <- abs(t_stat_1) > t_critical_2
list(t_statistic = t_stat_1, critical_value = t_critical_2, reject_H0 = reject_h0_1)

# (e) 計算殘差並繪製與 TIME 的關係
motel$residuals <- residuals(model)
ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "purple") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals over Time", x = "Time", y = "Residuals")

#CH04Q29
library(POE5Rdata)
data(cex5_small)

# (a)計算描述統計量
summary_food <- summary(cex5_small$food)
summary_income <- summary(cex5_small$income)
sd_food <- sd(cex5_small$food)
sd_income <- sd(cex5_small$income)

# 輸出結果
cat("FOOD Summary:\n")
print(summary_food)
cat("FOOD Standard Deviation:", sd_food, "\n")
cat("INCOME Summary:\n")
print(summary_income)
cat("INCOME Standard Deviation:", sd_income, "\n")

# 繪製直方圖
par(mfrow=c(1,2))  # 將圖形分成1行2列
hist(cex5_small$food, main="Histogram of FOOD", xlab="FOOD", col="lightblue")
abline(v=mean(cex5_small$food), col="red", lwd=2, lty=2)  # 均值
abline(v=median(cex5_small$food), col="blue", lwd=2, lty=2)  # 中位數
hist(cex5_small$income, main="Histogram of INCOME", xlab="INCOME", col="lightgreen")
abline(v=mean(cex5_small$income), col="red", lwd=2, lty=2)
abline(v=median(cex5_small$income), col="blue", lwd=2, lty=2)

# 安裝並載入moments包進行Jarque-Bera檢驗
if (!require(moments)) install.packages("moments")
library(moments)

# Jarque-Bera檢驗
jb_food <- jarque.test(cex5_small$food)
jb_income <- jarque.test(cex5_small$income)
cat("Jarque-Bera Test for FOOD:\n")
print(jb_food)
cat("Jarque-Bera Test for INCOME:\n")
print(jb_income)

# (b)線性回歸
model_linear <- lm(food ~ income, data=cex5_small)

# 模型摘要
summary(model_linear)

# β₂的95%置信區間
confint_beta2 <- confint(model_linear, "income", level=0.95)
cat("95% Confidence Interval for β₂:\n")
print(confint_beta2)

# 繪製散點圖並添加回歸線
par(mfrow=c(1,1)) 
plot(cex5_small$income, cex5_small$food, main="FOOD vs INCOME", xlab="INCOME", ylab="FOOD", pch=16)
abline(model_linear, col="red", lwd=2)

# (c)
#獲取殘差
residuals_linear <- residuals(model_linear)

# 殘差對INCOME的圖
plot(cex5_small$income, residuals_linear, main="Residuals vs INCOME", xlab="INCOME", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)

# 殘差直方圖
hist(residuals_linear, main="Histogram of Residuals", xlab="Residuals", col="lightgray")

# Jarque-Bera檢驗
jb_residuals <- jarque.test(residuals_linear)
cat("Jarque-Bera Test for Residuals:\n")
print(jb_residuals)


#(d)
# 指定INCOME值
income_values <- c(19, 65, 160)

# 預測FOOD值
food_pred <- predict(model_linear, newdata=data.frame(income=income_values))

# 計算彈性點估計：β₂ * (INCOME / FOOD)
beta2 <- coef(model_linear)["income"]
elasticity_point <- beta2 * (income_values / food_pred)
cat("Elasticity at INCOME = 19, 65, 160:\n")
print(elasticity_point)

# 簡化版95%置信區間（僅點估計範圍，完整需要delta法或bootstrap）
ci_beta2 <- confint(model_linear, "income", level=0.95)
elasticity_ci <- t(sapply(1:3, function(i) ci_beta2 * income_values[i] / food_pred[i]))
colnames(elasticity_ci) <- c("2.5%", "97.5%")
rownames(elasticity_ci) <- income_values
cat("Approximate 95% CI for Elasticity:\n")
print(elasticity_ci)


#(e)
# 創建對數變量
cex5_small$ln_FOOD <- log(cex5_small$food)
cex5_small$ln_INCOME <- log(cex5_small$income)

# log-log回歸
model_loglog <- lm(ln_FOOD ~ ln_INCOME, data=cex5_small)
summary(model_loglog)

# 散點圖與回歸線
plot(cex5_small$ln_INCOME, cex5_small$ln_FOOD, main="ln(FOOD) vs ln(INCOME)", xlab="ln(INCOME)", ylab="ln(FOOD)", pch=16)
abline(model_loglog, col="red", lwd=2)

#(f)
# log-log模型彈性為γ₂
gamma2 <- coef(model_loglog)["ln_INCOME"]
ci_gamma2 <- confint(model_loglog, "ln_INCOME", level=0.95)
cat("Elasticity (γ₂):", gamma2, "\n")
cat("95% CI for Elasticity:\n")
print(ci_gamma2)

#(g)
# 獲取殘差
residuals_loglog <- residuals(model_loglog)

# 殘差圖
plot(cex5_small$ln_INCOME, residuals_loglog, main="Residuals vs ln(INCOME)", xlab="ln(INCOME)", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)

# 殘差直方圖與檢驗
hist(residuals_loglog, main="Histogram of Residuals (Log-Log)", xlab="Residuals", col="lightgray")
jb_residuals_loglog <- jarque.test(residuals_loglog)
print(jb_residuals_loglog)

#(h)
# linear-log回歸
model_linearlog <- lm(food ~ ln_INCOME, data=cex5_small)
summary(model_linearlog)

# 散點圖
plot(cex5_small$ln_INCOME, cex5_small$food, main="FOOD vs ln(INCOME)", xlab="ln(INCOME)", ylab="FOOD", pch=16)
abline(model_linearlog, col="red", lwd=2)

# 比較R²
r2_linear <- summary(model_linear)$r.squared
r2_loglog <- summary(model_loglog)$r.squared
r2_linearlog <- summary(model_linearlog)$r.squared
cat("R² Linear:", r2_linear, "\nR² Log-Log:", r2_loglog, "\nR² Linear-Log:", r2_linearlog, "\n")

#(i)
# 預測FOOD
ln_income_values <- log(income_values)
food_pred_linearlog <- predict(model_linearlog, newdata=data.frame(ln_INCOME=ln_income_values))

# 彈性：α₂ / FOOD
alpha2 <- coef(model_linearlog)["ln_INCOME"]
elasticity_linearlog <- alpha2 / food_pred_linearlog
cat("Elasticity at INCOME = 19, 65, 160:\n")
print(elasticity_linearlog)

ci_alpha2 <- confint(model_linearlog, "ln_INCOME", level=0.95)
elasticity_ci_alpha2 <- t(sapply(1:3, function(i) ci_alpha2 / food_pred_linearlog[i]))
colnames(elasticity_ci_alpha2) <- c("2.5%", "97.5%")
rownames(elasticity_ci_alpha2) <- income_values
cat("Approximate 95% CI for Elasticity:\n")
print(elasticity_ci_alpha2)

#(j)
# 殘差分析
residuals_linearlog <- residuals(model_linearlog)
plot(cex5_small$ln_INCOME, residuals_linearlog, main="Residuals vs ln(INCOME)", xlab="ln(INCOME)", ylab="Residuals", pch=16)
abline(h=0, col="red", lwd=2)
hist(residuals_linearlog, main="Histogram of Residuals (Linear-Log)", xlab="Residuals", col="lightgray")
jb_residuals_linearlog <- jarque.test(residuals_linearlog)
print(jb_residuals_linearlog)
