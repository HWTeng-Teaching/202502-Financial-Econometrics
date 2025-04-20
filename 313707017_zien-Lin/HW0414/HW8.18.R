# 載入所需的套件和數據集
library(POE5Rdata)
data("cps5")

# 計算男性 (FEMALE = 0) 和女性 (FEMALE = 1) 的殘差平方和
# 首先進行回歸
model <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = cps5)

# 提取殘差並按性別分組
residuals <- residuals(model)
female <- cps5$female == 1
male <- cps5$female == 0

# 計算男性與女性的殘差平方和
SSR_male <- sum(residuals[male]^2)  # 男性 SSR (σ_m^2 的估計與自由度調整有關)
SSR_female <- sum(residuals[female]^2)  # 女性 SSR (σ_f^2 的估計與自由度調整有關)

# 樣本大小
n_male <- sum(male)
n_female <- sum(female)

# 計算 Goldfeld-Quandt 檢定統計量
GQ_stat <- (SSR_female / (n_female - 5)) / (SSR_male / (n_male - 5))  # 除以自由度 (k=5)

# 臨界值與檢定
alpha <- 0.05
df1 <- n_female - 5  # 女性自由度
df2 <- n_male - 5    # 男性自由度
critical_value <- qf(1 - alpha/2, df1, df2)  # 雙尾檢定的臨界值

# 檢定結果
reject_null <- GQ_stat > critical_value | GQ_stat < 1/critical_value

# 輸出結果
cat("Goldfeld-Quandt Test Statistic:", GQ_stat, "\n")
cat("Critical Value (5% level):", critical_value, "\n")
cat("Reject Null Hypothesis:", reject_null, "\n")

# b.-------------------
# 用 OLS 估計模型（修正欄位名稱為小寫）
model <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + female + black + 
              south + midwest + west, data = cps5)

# NR² 檢定：用 metro、female、black 作為異質變異的候選變數
residuals <- residuals(model)
residuals_squared <- residuals^2
aux_model1 <- lm(residuals_squared ~ metro + female + black, data = cps5)
n <- nrow(cps5)
r_squared1 <- summary(aux_model1)$r.squared
NR2_stat1 <- n * r_squared1
critical_value1 <- qchisq(0.99, 3)  # 1% 顯著水準，自由度=3
reject_null1 <- NR2_stat1 > critical_value1

# NR² 檢定：用所有解釋變數作為候選變數
aux_model2 <- lm(residuals_squared ~ educ + exper + I(exper^2) + metro + female + 
                   black + south + midwest + west, data = cps5)
r_squared2 <- summary(aux_model2)$r.squared
NR2_stat2 <- n * r_squared2
critical_value2 <- qchisq(0.99, 9)  # 1% 顯著水準，自由度=9
reject_null2 <- NR2_stat2 > critical_value2

# 輸出結果
cat("NR² 檢定 (metro, female, black):\n")
cat("統計量:", NR2_stat1, "，臨界值:", critical_value1, "，是否拒絕虛無假設:", reject_null1, "\n\n")
cat("NR² 檢定 (所有變數):\n")
cat("統計量:", NR2_stat2, "，臨界值:", critical_value2, "，是否拒絕虛無假設:", reject_null2, "\n")

# C.--------------------
library(lmtest)
# 用 OLS 估計模型（使用小寫欄位名稱）
model <- lm(log(wage) ~ educ + exper + I(exper^2) + metro + female + black + 
              south + midwest + west, data = cps5)

# 進行 White 檢定
# White 檢定需要包含所有解釋變數、它們的平方項和交叉項
# 我們可以用 lmtest 套件的 bptest() 函數，並指定 type="white"
white_test <- bptest(model, ~ educ + exper + I(exper^2) + metro + female + black + 
                       south + midwest + west + 
                       I(educ^2) + I(exper^2) + I(metro^2) + I(female^2) + I(black^2) + 
                       I(south^2) + I(midwest^2) + I(west^2) + 
                       educ:exper + educ:metro + educ:female + educ:black + educ:south + 
                       educ:midwest + educ:west + exper:metro + exper:female + exper:black + 
                       exper:south + exper:midwest + exper:west + metro:female + metro:black + 
                       metro:south + metro:midwest + metro:west + female:black + female:south + 
                       female:midwest + female:west + black:south + black:midwest + black:west + 
                       south:midwest + south:west + midwest:west, data = cps5)

# 提取 White 檢定統計量
white_stat <- white_test$statistic

# 計算自由度（解釋變數數量，包括平方項和交叉項）
# 原始變數：9 個 (educ, exper, exper^2, metro, female, black, south, midwest, west)
# 平方項：排除重複的 exper^2 和二元變數的平方項（metro, female, black, south, midwest, west 為 0/1）
# 交叉項：9 個變數兩兩組合，(9*8)/2 = 36
df <- 9 + 1 + 36  # 9（原始項）+ 1（educ 的平方項）+ 36（交叉項）= 46

# 5% 顯著水準下的臨界值
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df)

# 檢定結果
reject_null <- white_stat > critical_value

# 輸出結果
cat("White 檢定統計量:", white_stat, "\n")
cat("自由度:", df, "\n")
cat("5% 顯著水準臨界值:", critical_value, "\n")
cat("是否拒絕虛無假設 (存在異質變異):", reject_null, "\n")

# d.-------------
# 加載必要的包
library(lmtest)
library(sandwich)

# 假設數據集已加載到名為'data'的數據框中
# 如果需要加載數據，請取消註釋並調整路徑
# data <- read.csv("path/to/cps5.csv")

# 創建exper2（exper的平方）
cps5$exper2 <- cps5$exper^2

# 將wage取自然對數
cps5$lnwage <- log(cps5$wage)

# 使用OLS進行回歸，計算傳統標準誤
ols_model <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 提取傳統標準誤
ols_summary <- summary(ols_model)
ols_se <- ols_summary$coefficients[, "Std. Error"]

# 計算White異方差穩健標準誤
robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC0"))) # HC0為White估計方法

# 比較標準誤
comparison <- data.frame(
  係數 = names(ols_se),
  OLS標準誤 = ols_se,
  穩健標準誤 = robust_se,
  差異 = robust_se - ols_se
)

# 計算兩種標準誤下的95%置信區間
conf_int_ols <- confint(ols_model) # 傳統標準誤的置信區間
conf_int_robust <- cbind(
  ols_model$coefficients - 1.96 * robust_se, # 下限
  ols_model$coefficients + 1.96 * robust_se  # 上限
)

# 計算置信區間的寬度
width_ols <- conf_int_ols[, 2] - conf_int_ols[, 1]
width_robust <- conf_int_robust[, 2] - conf_int_robust[, 1]

# 比較置信區間寬度
width_comparison <- data.frame(
  係數 = names(ols_model$coefficients),
  OLS置信區間寬度 = width_ols,
  穩健置信區間寬度 = width_robust,
  更寬 = ifelse(width_robust > width_ols, "穩健", "OLS")
)

# 顯示結果
print("標準誤比較：")
print(comparison)
print("置信區間寬度比較：")
print(width_comparison)

# 檢查一致性（例如顯著性是否改變）
ols_pvalues <- ols_summary$coefficients[, "Pr(>|t|)"]
robust_pvalues <- 2 * pnorm(-abs(ols_model$coefficients / robust_se))
inconsistency <- data.frame(
  係數 = names(ols_model$coefficients),
  OLS顯著性 = ols_pvalues < 0.05,
  穩健顯著性 = robust_pvalues < 0.05
)
print("顯著性一致性檢查：")
print(inconsistency)

# e.----------------
# 創建exper2（exper的平方）
cps5$exper2 <- cps5$exper^2

# 將wage取自然對數
cps5$lnwage <- log(cps5$wage)

# 第一步：用OLS估計模型，獲取殘差
ols_model <- lm(lnwage ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)
cps5$resid <- residuals(ols_model) # 提取殘差

# 第二步：用殘差的對數平方回歸候選變量metro和exper，估計方差結構
cps5$log_resid2 <- log(cps5$resid^2) # 對殘差平方取對數
var_model <- lm(log_resid2 ~ metro + exper, data = cps5)

# 第三步：計算估計方差，並生成加權
cps5$var_fitted <- exp(fitted(var_model)) # 估計的方差
cps5$weights <- 1 / cps5$var_fitted # 加權為估計方差的倒數

# 第四步：進行加權最小二乘法（WLS），即FGLS
fgls_model <- lm(lnwage ~ educ + exper + exper2 + female + black + metro + south + midwest + west, 
                 data = cps5, weights = weights)

# 提取FGLS的標準誤和置信區間
fgls_summary <- summary(fgls_model)
fgls_se <- fgls_summary$coefficients[, "Std. Error"]
conf_int_fgls <- confint(fgls_model)

# 從第(d)小題提取OLS（White穩健標準誤）的結果
robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC0"))) # White穩健標準誤
conf_int_robust <- cbind(
  ols_model$coefficients - 1.96 * robust_se, # 下限
  ols_model$coefficients + 1.96 * robust_se  # 上限
)

# 比較置信區間的寬度
width_fgls <- conf_int_fgls[, 2] - conf_int_fgls[, 1]
width_robust <- conf_int_robust[, 2] - conf_int_robust[, 1]

# 比較結果
width_comparison <- data.frame(
  係數 = names(ols_model$coefficients),
  FGLS置信區間寬度 = width_fgls,
  OLS穩健置信區間寬度 = width_robust,
  更寬 = ifelse(width_fgls > width_robust, "FGLS", "OLS穩健")
)

# 顯示結果
print("置信區間寬度比較（FGLS vs OLS穩健）：")
print(width_comparison)

# f.---------------------
# 第一步：用OLS估計模型，獲取殘差（與第(e)小題相同）
ols_model <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)
cps5$resid <- residuals(ols_model)

# 第二步：用殘差的對數平方回歸候選變量metro和exper，估計方差結構
cps5$log_resid2 <- log(cps5$resid^2)
var_model <- lm(log_resid2 ~ metro + exper, data = cps5)

# 第三步：計算估計方差，並生成加權
cps5$var_fitted <- exp(fitted(var_model))
cps5$weights <- 1 / cps5$var_fitted

# 第四步：進行FGLS估計
fgls_model <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, 
                 data = cps5, weights = weights)

# 提取FGLS的傳統標準誤（第(e)小題的結果）
fgls_summary <- summary(fgls_model)
fgls_se <- fgls_summary$coefficients[, "Std. Error"]
conf_int_fgls <- confint(fgls_model)

# 第五步：計算FGLS的White異方差穩健標準誤
fgls_robust_se <- sqrt(diag(vcovHC(fgls_model, type = "HC0")))

# 計算FGLS（穩健標準誤）的置信區間
conf_int_fgls_robust <- cbind(
  fgls_model$coefficients - 1.96 * fgls_robust_se, # 下限
  fgls_model$coefficients + 1.96 * fgls_robust_se  # 上限
)

# 提取第(d)小題的OLS（White穩健標準誤）置信區間
ols_robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC0")))
conf_int_ols_robust <- cbind(
  ols_model$coefficients - 1.96 * ols_robust_se, # 下限
  ols_model$coefficients + 1.96 * ols_robust_se  # 上限
)

# 比較置信區間的寬度
width_fgls <- conf_int_fgls[, 2] - conf_int_fgls[, 1] # 第(e)小題的FGLS
width_fgls_robust <- conf_int_fgls_robust[, 2] - conf_int_fgls_robust[, 1] # FGLS（穩健）
width_ols_robust <- conf_int_ols_robust[, 2] - conf_int_ols_robust[, 1] # 第(d)小題的OLS（穩健）

# 比較結果
width_comparison <- data.frame(
  係數 = names(ols_model$coefficients),
  FGLS置信區間寬度 = width_fgls,
  FGLS穩健置信區間寬度 = width_fgls_robust,
  OLS穩健置信區間寬度 = width_ols_robust
)

# 顯示結果
print("置信區間寬度比較（FGLS vs FGLS穩健 vs OLS穩健）：")
print(width_comparison)
