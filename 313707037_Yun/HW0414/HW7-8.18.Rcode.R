url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/cps5.dat"
download.file(url, destfile = "cps_5.dat")
cps5 <- read.table("cps_5.dat", header = FALSE)
colnames(cps5) <- c("AGE","ASIAN", "BLACK", "DIVORCED", "EDUC", "EXPER", "FAMINC", "FEMALE", "HRSWORK", "INSURE", "MARRIED", "MCAID", "MCARE", "METRO", "MIDWEST",
"NCHILD", "NORTHEAST", "SINGLE", "SOUTH", "UNION", "WAGE", "WEST", "WHITE")
head(cps5)  # 查看前幾行

##題目 a: Goldfeld–Quandt Test

#檢定男性與女性誤差變異數是否相同：

install.packages("car")
library(car)

# 排序資料依據 FEMALE 變數
cps5_sorted <- cps5[order(cps5$FEMALE), ]

# 擬合原始迴歸模型
model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = cps5_sorted)

# 執行 Goldfeld-Quandt test（女性 vs 男性）
gqtest(model, order.by = ~FEMALE, data = cps5_sorted, fraction = 1/3)



##題目 b: NR² Test

#使用 METRO, FEMALE, BLACK 為異質變異性的候選變數：
model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = cps5)

# 提取殘差平方
res_sq <- resid(model)^2

# 擬合殘差平方的輔助迴歸
aux_model <- lm(res_sq ~ METRO + FEMALE + BLACK, data = cps5)

# 計算 NR^2 統計量
n <- nrow(cps5)
R2 <- summary(aux_model)$r.squared
NR2 <- n * R2
print(NR2)
# 1% 顯著水準下的卡方臨界值 (自由度 = 3)
qchisq(0.99, df = 3)


#題目 c: White Test

White 檢定異質變異性（需引入 square 與交乘項）：

#Step 1: 擬合原始 OLS 模型

# 擬合基本模型
ols_model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = cps5)

#Step 2: OLS 常規信賴區間（使用常規標準誤）

# 95% 信賴區間 (常規 OLS)
confint(ols_model)


#Step 3: Robust OLS 信賴區間（使用 White robust SE）

# 載入必要套件
library(sandwich)
library(lmtest)

# Robust 標準誤 (White robust)
robust_se <- vcovHC(ols_model, type = "HC1")

# 取得係數估計與 robust 標準誤
coefs <- coef(ols_model)
se_robust <- sqrt(diag(robust_se))

# 計算 95% 信賴區間
lower_bound <- coefs - 1.96 * se_robust
upper_bound <- coefs + 1.96 * se_robust

# 組合成表格
robust_ci <- cbind(Lower = lower_bound, Estimate = coefs, Upper = upper_bound)
round(robust_ci, 4)


題目 d: OLS with robust standard errors

# Robust SEs
coeftest(model, vcov = vcovHC(model, type = "HC1"))

題目 e: FGLS (使用 METRO 與 EXPER 作為異質變異性候選變數)

#Step-by-step：FGLS 與信賴區間

#Step 1: 擬合原始 OLS 模型，取得殘差平方

ols_model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, data = cps5)

# 殘差平方
res_sq <- resid(ols_model)^2


#Step 2: 用 METRO 和 EXPER 擬合殘差平方，估計異質變異性

# 擬合輔助迴歸來預測 sigma^2
aux_model <- lm(res_sq ~ METRO + EXPER, data = cps5)

# 估計 sigma^2
sigma2_hat <- fitted(aux_model)

# 權重 = 1 / sigma^2
weights <- 1 / sigma2_hat

#Step 3: FGLS 迴歸（加權最小平方法）

fgls_model <- lm(log(WAGE) ~ EDUC + EXPER + I(EXPER^2) + FEMALE + BLACK + METRO + SOUTH + MIDWEST + WEST, 
                 data = cps5, weights = weights)

#Step 4: 計算 FGLS 的 95% 信賴區間

# FGLS 估計的 95% 常規信賴區間
confint(fgls_model)

# Robust SE on FGLS
library(sandwich)
library(lmtest)

# Robust VCOV
fgls_robust_vcov <- vcovHC(fgls_model, type = "HC1")

# Robust 標準誤
se_fgls_robust <- sqrt(diag(fgls_robust_vcov))

# 係數
coefs_fgls <- coef(fgls_model)

# 信賴區間
lower_fgls_robust <- coefs_fgls - 1.96 * se_fgls_robust
upper_fgls_robust <- coefs_fgls + 1.96 * se_fgls_robust

# 組合表格
ci_fgls_robust <- cbind(Lower = lower_fgls_robust, Estimate = coefs_fgls, Upper = upper_fgls_robust)
round(ci_fgls_robust, 6)
