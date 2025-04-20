install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置

# 安裝套件一次就夠了，你已經裝過
# 使用資料集
library(POE5Rdata)

# 載入資料
data("vacation", package = "POE5Rdata")

# 檢查是否成功
head(vacation)

#################a.

# 套件名稱與題目一致，變數名稱全轉大寫
vacation <- POE5Rdata::vacation
names(vacation) <- toupper(names(vacation))  # 統一成 MILES, INCOME, AGE, KIDS

# OLS 估計
model_ols <- lm(MILES ~ INCOME + AGE + KIDS, data = vacation)

# 檢視估計結果
summary(model_ols)

# 所有變數的信賴區間
confint(model_ols)

# 只看 KIDS
confint(model_ols)["KIDS", ]

###############b.

residuals <- resid(model_ols)

# 畫殘差對 INCOME
plot(vacation$INCOME, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red")

# 畫殘差對 AGE
plot(vacation$AGE, residuals,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red")



# 畫出 Residuals vs INCOME
plot(vacation$INCOME, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME", ylab = "Residuals",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red")


###################c.

# Step 1: 排序資料依據 INCOME（遞增）
vacation_sorted <- vacation[order(vacation$INCOME), ]

# Step 2: 分成三組：前 90、(中間 20)、後 90
low90  <- vacation_sorted[1:90, ]
high90 <- vacation_sorted[111:200, ]

# Step 3: 分別對前90與後90組跑 OLS
model_low  <- lm(MILES ~ INCOME + AGE + KIDS, data = low90)
model_high <- lm(MILES ~ INCOME + AGE + KIDS, data = high90)

# Step 4: 取得 SSE（平方誤差和）
sse_low  <- sum(resid(model_low)^2)
sse_high <- sum(resid(model_high)^2)

# Step 5: 計算 GQ 檢定統計量
gq_stat <- sse_high / sse_low

# Step 6: 查看自由度並查臨界值（可用 qf() 函數）
df1 <- 90 - 4  # k = 4（INCOME, AGE, KIDS, intercept）
df2 <- 90 - 4
critical_value <- qf(0.95, df1, df2)

# 顯示結果
gq_stat
critical_value

########################d.

# 安裝套件（只需一次）
# install.packages("sandwich")
# install.packages("lmtest")

library(sandwich)
library(lmtest)

# 使用 robust covariance matrix 估計模型
coeftest(model_ols, vcov. = vcovHC(model_ols, type = "HC1"))

# Robust 標準誤
robust_vcov <- vcovHC(model_ols, type = "HC1")

# 手動計算信賴區間 for KIDS
beta_kids <- coef(model_ols)["KIDS"]
se_kids_robust <- sqrt(robust_vcov["KIDS", "KIDS"])
t_crit <- qt(0.975, df = 196)  # 200 observations, 3 predictors + intercept → df = 200 - 4 = 196

lower_bound <- beta_kids - t_crit * se_kids_robust
upper_bound <- beta_kids + t_crit * se_kids_robust

c(lower_bound, upper_bound)

#######################e

# install.packages("nlme")  # 若尚未安裝
library(nlme)

# Step 1：用 gls() 實作 GLS，指定誤差變異為與 INCOME 平方成正比
# GLS 模型：Var(e_i) ∝ INCOME_i^2
model_gls <- gls(MILES ~ INCOME + AGE + KIDS,
                 weights = varFixed(~ INCOME^2),
                 data = vacation)

summary(model_gls)

# Step 2（選擇性）：估計 robust GLS 標準誤（需要額外手動做）
# Robust SE for GLS
library(sandwich)
library(lmtest)

# coeftest 結合 sandwich robust SE（方法類似 robust OLS）
coeftest(model_gls, vcov. = sandwich)


#Step 3：提取 $\beta_4$（KIDS）及其信賴區間
# GLS coefficient and standard error
beta_kids_gls <- coef(model_gls)["KIDS"]
se_kids_gls <- sqrt(vcov(model_gls)["KIDS", "KIDS"])

# 建立 95% 信賴區間
t_crit <- qt(0.975, df = 196)  # 跟 OLS 一樣自由度
lower_gls <- beta_kids_gls - t_crit * se_kids_gls
upper_gls <- beta_kids_gls + t_crit * se_kids_gls

c(lower_gls, upper_gls)



# Step 0: 載入必要套件
library(nlme)
library(clubSandwich)

# Step 1: 使用 GLS 模型，假設 Var(e_i) ∝ INCOME^2
model_gls <- gls(MILES ~ INCOME + AGE + KIDS,
                 weights = varFixed(~ INCOME^2),
                 data = vacation)

# Step 2: 指定 cluster（這邊每筆資料各自為一個群組）
cluster_id <- 1:nrow(vacation)

# Step 3: Robust 標準誤與信賴區間（CR2 = HC2 類型）
# Robust coefficient test
coef_test(model_gls, vcov = "CR2", cluster = cluster_id, test = "naive-t")

# Robust 95% confidence intervals
conf_int(model_gls, vcov = "CR2", cluster = cluster_id, level = 0.95)
