#CH8Q16
rm(list=ls())  
library(POE5Rdata)  
data("vacation") 

#(a)
# 建立迴歸模型
model <- lm(miles ~ income + age + kids, data = vacation)

# 查看回歸結果
summary(model)


# 建立信賴區間（預設為 95%）
confint(model, level = 0.95)["kids", ]


#(b)
# 取得模型殘差
residuals <- resid(model)

# 殘差對 INCOME
plot(vacation$income, residuals,
     main = "Residuals vs INCOME",
     xlab = "INCOME (in $1000s)", ylab = "Residuals",
     pch = 19, col = "steelblue")
abline(h = 0, col = "red")

# 殘差對 AGE
plot(vacation$age, residuals,
     main = "Residuals vs AGE",
     xlab = "AGE", ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red")



#(c)
vacation_sorted <- vacation[order(vacation$income), ]

# 分成前 90 和後 90 筆（中間 20 筆丟掉）
low_income <- vacation_sorted[1:90, ]
high_income <- vacation_sorted[111:200, ]  # 從第111筆開始到第200筆

# 對低收入群體
model_low <- lm(miles ~ income + age + kids, data = low_income)
SSE_low <- sum(resid(model_low)^2)

# 對高收入群體
model_high <- lm(miles ~ income + age + kids, data = high_income)
SSE_high <- sum(resid(model_high)^2)

# 檢定自由度（樣本數 - 參數個數）
df <- 90 - 4  # 3個自變數 + 截距

F_stat <- (SSE_high / df) / (SSE_low / df)
F_stat

alpha <- 0.05
crit_val <- qf(1 - alpha, df, df)  # F 分布臨界值
crit_val

# 結論
if (F_stat > crit_val) {
  cat("Reject H0: Evidence of heteroskedasticity.\n")
} else {
  cat("Fail to reject H0: No strong evidence of heteroskedasticity.\n")
}


#(d)
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

# 使用異質變異穩健標準誤（White robust）
coeftest(model, vcov = vcovHC(model, type = "HC1"))

# 手動取得 KIDS 的估計值與 robust SE
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
b_kids <- coef(model)["kids"]
se_kids <- robust_se["kids"]

# 臨界值
n <- nrow(vacation)
k <- length(coef(model))
t_crit <- qt(0.975, df = n - k)

# 信賴區間
lower <- b_kids - t_crit * se_kids
upper <- b_kids + t_crit * se_kids
c(lower, upper)



#(e)
vacation$weights_gls <- 1 / (vacation$income^2)

gls_model <- lm(miles ~ income + age + kids, data = vacation, weights = weights_gls)

summary(gls_model)
confint(gls_model, level = 0.95)["kids", ]
robust_gls_se <- vcovHC(gls_model, type = "HC1")
coeftest(gls_model, vcov. = robust_gls_se)

beta_kids_gls <- coef(gls_model)["kids"]
se_kids_gls <- sqrt(robust_gls_se["kids", "kids"])

lower_gls <- beta_kids_gls - 1.96 * se_kids_gls
upper_gls <- beta_kids_gls + 1.96 * se_kids_gls

cat("Robust GLS 95% CI for KIDS:", round(lower_gls, 3), "to", round(upper_gls, 3), "\n")
## From the narrowest to the widest:a,d,gls,robust gls






#CH8Q18

rm(list=ls())  
library(POE5Rdata)  
data("cps5") 

#(a)
# 男性組（FEMALE = 0）
male_data <- subset(cps5, female == 0)
model_male <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = male_data)
sse_male <- sum(resid(model_male)^2)

# 女性組（FEMALE = 1）
female_data <- subset(cps5, female == 1)
model_female <- lm(log(wage) ~ educ + exper + I(exper^2) + metro, data = female_data)
sse_female <- sum(resid(model_female)^2)

# 計算自由度（樣本數 - k 參數）
df_male <- nrow(male_data) - length(coef(model_male))
df_female <- nrow(female_data) - length(coef(model_female))

# 大變異數放在分子（例如女性變異較大時）
F_stat <- (sse_female / df_female) / (sse_male / df_male)
F_stat

# 使用兩邊自由度
alpha <- 0.05
crit_upper <- qf(1 - alpha/2, df_female, df_male)
crit_lower <- qf(alpha/2, df_female, df_male)

cat("F 統計量 = ", F_stat, "\n")
cat("拒絕域：F <", round(crit_lower, 3), " 或 F >", round(crit_upper, 3), "\n")

# 結論
if (F_stat < crit_lower || F_stat > crit_upper) {
  cat("→ 拒絕虛無假設：有證據顯示男性與女性誤差變異不同。\n")
} else {
  cat("→ 無法拒絕虛無假設：沒有顯著證據顯示變異不同。\n")
}


#(b)
library(lmtest)
library(sandwich)

model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)

# 只用部分變數做異質變異檢定
bptest(model, ~ metro + female + black, data = cps5)

# 全部變數進入異質變異檢定
bptest(model, ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)


#(c)
library(lmtest)
library(sandwich)

# White 檢定（等價於 studentized Breusch-Pagan）
bptest(model, ~ ., data = cps5, studentize = FALSE)

qchisq(0.95, df = 44)  # k 為迴歸變數數量（不含常數項）


#(d)
library(lmtest)
library(sandwich)

coeftest(model)  # 傳統 OLS
coeftest(model, vcov = vcovHC(model, type = "HC1"))  # robust SE
# 1. 模型與 robust 標準誤
model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5)
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))


# 2. 標準誤
ols_se <- sqrt(diag(vcov(model)))
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))

# 3. 差異比較表格
comparison <- data.frame(
  係數 = names(ols_se),
  OLS標準誤 = ols_se,
  穩健標準誤 = robust_se,
  差異 = robust_se - ols_se
)

print(comparison)

# 4. 信賴區間寬度（95%）
n <- nrow(cps5)
k <- length(coef(model))
t_crit <- qt(0.975, df = n - k)

ols_width <- 2 * t_crit * ols_se
robust_width <- 2 * t_crit * robust_se

# 5. 比較信賴區間寬度
width_comparison <- data.frame(
  係數 = names(ols_se),
  OLS置信區間寬度 = ols_width,
  穩健置信區間寬度 = robust_width,
  更寬 = ifelse(robust_width > ols_width, "穩健", "OLS")
)

cat("置信區間寬度比較：\n")
print(width_comparison)


# OLS 檢定
ols_test <- coeftest(model)
ols_sig <- ols_test[, 4] < 0.05  # 取 p 值 < 0.05 判定顯著

# Robust 檢定
robust_test <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
robust_sig <- robust_test[, 4] < 0.05  # 同樣判定顯著性

# 顯著性比較表
sig_compare <- data.frame(
  係數 = rownames(ols_test),
  OLS顯著性 = ols_sig,
  穩健顯著性 = robust_sig
)

print(sig_compare)


#(e)
ols_model <-lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5) 
robust_se <- sqrt(diag(vcovHC(ols_model, type = "HC1")))

res_sq <- resid(ols_model)^2

# 建立輔助變異數模型
aux_model <- lm(res_sq ~ metro + exper, data = cps5)

# 預測誤差變異（正的）
pred_var <- pmax(fitted(aux_model), 1e-6)  # 確保沒有負數

# 計算 FGLS 權重 = 1 / 預測變異
weights_fgls <- 1 / pred_var

fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest+west,data = cps5, weights = weights_fgls)
                 
fgls_se <- sqrt(diag(vcov(fgls_model)))  # 傳統 FGLS 標準誤

n <- nrow(cps5)
k <- length(coef(fgls_model))
t_crit <- qt(0.975, df = n - k)

# 寬度 = 2 * t * SE
fgls_width <- 2 * t_crit * fgls_se
robust_width <- 2 * t_crit * robust_se
ci_compare <- data.frame(
  係數 = names(fgls_se),
  FGLS置信區間寬度 = fgls_width,
  OLS穩健置信區間寬度 = robust_width,
  更寬 = ifelse(fgls_width > robust_width, "FGLS", "OLS穩健")
)

print(ci_compare)

#(f)
# fgls_model 是你在 (e) 小題建立的 FGLS 加權模型
# 現在我們針對它再算 robust 標準誤

# FGLS + robust SE
robust_se_fgls <- sqrt(diag(vcovHC(fgls_model, type = "HC1")))

# 信賴區間寬度（95%）
n <- nrow(cps5)
k <- length(coef(fgls_model))
t_crit <- qt(0.975, df = n - k)

fgls_width <- 2 * t_crit * sqrt(diag(vcov(fgls_model)))
fgls_robust_width <- 2 * t_crit * robust_se_fgls
ols_robust_width <- 2 * t_crit * sqrt(diag(vcovHC(ols_model, type = "HC1")))

width_comparison <- data.frame(
  係數 = names(coef(fgls_model)),
  FGLS置信區間寬度 = fgls_width,
  FGLS穩健置信區間寬度 = fgls_robust_width,
  OLS穩健置信區間寬度 = ols_robust_width
)

print("置信區間寬度比較（FGLS vs FGLS穩健 vs OLS穩健）:")
print(width_comparison)
