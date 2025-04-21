#a

# 讀取資料
cps5 <- read.csv("cps5.csv")

# 建立變數：經驗平方
cps5$exper2 <- cps5$exper^2

# 依性別分組（男性 = 0, 女性 = 1）
cps5_male   <- subset(cps5, female == 0)
cps5_female <- subset(cps5, female == 1)

# 建立回歸模型（同樣的變數）：
formula_str <- log(wage) ~ educ + exper + exper2 + black + metro + south + midwest + west

mod_male   <- lm(formula_str, data = cps5_male)
mod_female <- lm(formula_str, data = cps5_female)

# 取得殘差平方和 (SSR)
SSR_male   <- sum(resid(mod_male)^2)
SSR_female <- sum(resid(mod_female)^2)

# 自由度
df_male   <- df.residual(mod_male)
df_female <- df.residual(mod_female)

# 估計變異數
sigma2_male   <- SSR_male / df_male
sigma2_female <- SSR_female / df_female

# 計算 F 統計量
F_stat <- sigma2_male / sigma2_female
F_stat

# 設定顯著水準
alpha <- 0.05

# 拒絕域臨界值
F_lower <- qf(alpha / 2, df_male, df_female)
F_upper <- qf(1 - alpha / 2, df_male, df_female)

# 顯示拒絕域
cat("拒絕域：F <", round(F_lower, 4), "或 F >", round(F_upper, 4), "\n")
cat("F 統計量：", round(F_stat, 4), "\n")

#b
# 讀取資料與建立 exper²
cps5 <- read.csv("cps5.csv")
cps5$exper2 <- cps5$exper^2

# 建立原始模型
mod_ols <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 計算殘差平方
u2 <- resid(mod_ols)^2

# 建立輔助迴歸模型（用 METRO、FEMALE、BLACK）
aux_model <- lm(u2 ~ metro + female + black, data = cps5)

# 計算 R² 以及 NR²
R2_partial <- summary(aux_model)$r.squared
n <- nobs(mod_ols)
NR2_partial <- n * R2_partial
cat("NR² = ", NR2_partial, "\n")

# 計算 1% 顯著水準的臨界值，自由度為 3（因為有3個變數）
crit_val_partial <- qchisq(0.99, df = 3)
cat("臨界值 = ", crit_val_partial, "\n")

# 結論
if (NR2_partial > crit_val_partial) {
  cat("結論：拒絕虛無假設，存在異質變異數。\n")
} else {
  cat("結論：無法拒絕虛無假設，無顯著異質變異數證據。\n")
}

#b-2
# 建立完整輔助模型
aux_model_all <- lm(u2 ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 計算 R² 與 NR²
R2_all <- summary(aux_model_all)$r.squared
NR2_all <- n * R2_all
cat("NR² (全部變數) = ", NR2_all, "\n")

# 自由度 = 9（輔助模型有9個解釋變數）
crit_val_all <- qchisq(0.99, df = 9)
cat("臨界值 = ", crit_val_all, "\n")

# 結論
if (NR2_all > crit_val_all) {
  cat("結論：拒絕虛無假設，存在異質變異數。\n")
} else {
  cat("結論：無法拒絕虛無假設，無顯著異質變異數證據。\n")
}




#c
# 讀取資料與建立 exper²
cps5 <- read.csv("cps5.csv")
cps5$exper2 <- cps5$exper^2

# 建立原始模型
mod_ols <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 計算殘差平方
u2 <- resid(mod_ols)^2

# 建立變數平方與交互項（這是 White test 的核心）
cps5$educ2    <- cps5$educ^2
cps5$exper2_2 <- cps5$exper2^2
cps5$metro_female <- cps5$metro * cps5$female
cps5$educ_exper   <- cps5$educ * cps5$exper
# 你可以加入更多交互項以更接近 White test 全項設計

# 建立輔助模型
aux_model_white <- lm(u2 ~ educ + exper + exper2 + female + black + metro + south + midwest + west +
                        educ2 + exper2_2 + metro_female + educ_exper, data = cps5)

# NR² 檢定統計量
R2_white <- summary(aux_model_white)$r.squared
n <- nobs(mod_ols)
NR2_white <- n * R2_white

# 自由度（輔助模型中的變數數 = 13）
df_white <- length(coef(aux_model_white)) - 1

# 臨界值（5%）
crit_val_white <- qchisq(0.95, df = df_white)

# 輸出結果
cat("White test 統計量 = ", NR2_white, "\n")
cat("自由度 =", df_white, ", 臨界值 =", crit_val_white, "\n")

if (NR2_white > crit_val_white) {
  cat("結論：拒絕虛無假設，存在異質變異數。\n")
} else {
  cat("結論：無法拒絕虛無假設，無明顯異質變異數。\n")
}

#d
# 載入套件（若尚未安裝請先執行 install.packages）
# install.packages("sandwich")
# install.packages("lmtest")
library(sandwich)
library(lmtest)

# 讀取資料與新增變數
cps5 <- read.csv("cps5.csv")
cps5$exper2 <- cps5$exper^2

# 建立 OLS 模型
model_ols <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# 取得傳統標準誤信賴區間
ci_ols <- confint(model_ols, level = 0.95)
width_ols <- ci_ols[,2] - ci_ols[,1]

# 計算 White heteroskedasticity robust 標準誤與信賴區間
robust_se <- sqrt(diag(vcovHC(model_ols, type = "HC0")))
coef_vals <- coef(model_ols)
ci_robust <- cbind(
  coef_vals - 1.96 * robust_se,
  coef_vals + 1.96 * robust_se
)
colnames(ci_robust) <- c("Lower (Robust)", "Upper (Robust)")
width_robust <- ci_robust[,2] - ci_robust[,1]

# 合併比較表
comparison <- data.frame(
  Coef = names(coef_vals),
  Width_OLS = round(width_ols, 4),
  Width_Robust = round(width_robust, 4),
  Narrower_With_Robust = width_robust < width_ols,
  Wider_With_Robust = width_robust > width_ols
)

# 顯示結果
print(comparison)

#e
# 載入套件（如未安裝請先安裝）
# install.packages("sandwich")
# install.packages("lmtest")
library(sandwich)
library(lmtest)

# 讀取與建立資料
cps5 <- read.csv("cps5.csv")
cps5$exper2 <- cps5$exper^2

# Step 1: OLS 初始模型
model_ols <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)
resid_sq <- resid(model_ols)^2

# Step 2: 建立異質變異數模型，log(u^2) ~ metro + exper
aux_model <- lm(log(resid_sq) ~ metro + exper, data = cps5)
log_hatsigma2 <- fitted(aux_model)
hatsigma <- exp(log_hatsigma2 / 2)

# Step 3: 對原始模型變數進行加權（1/sigma 作為權重）做 GLS
X <- model.matrix(model_ols)
y <- log(cps5$wage)

# 將 y 與 X 加權
y_star <- y / hatsigma
X_star <- X / hatsigma

# FGLS 回歸
model_fgls <- lm(y_star ~ X_star - 1)  # 不含截距，因為 X_star 已含常數

# Step 4: 計算 FGLS 標準誤與信賴區間
fgls_se <- sqrt(diag(vcov(model_fgls)))
fgls_coef <- coef(model_fgls)

ci_fgls <- cbind(
  fgls_coef - 1.96 * fgls_se,
  fgls_coef + 1.96 * fgls_se
)
colnames(ci_fgls) <- c("Lower (FGLS)", "Upper (FGLS)")

# Step 5: 比較 OLS+robust CI 與 FGLS CI
robust_se <- sqrt(diag(vcovHC(model_ols, type = "HC0")))
coef_vals <- coef(model_ols)
ci_robust <- cbind(
  coef_vals - 1.96 * robust_se,
  coef_vals + 1.96 * robust_se
)
colnames(ci_robust) <- c("Lower (Robust)", "Upper (Robust)")

# 比較寬度
width_fgls <- ci_fgls[,2] - ci_fgls[,1]
width_robust <- ci_robust[,2] - ci_robust[,1]

# 合併結果
comparison <- data.frame(
  Coef = rownames(ci_robust),
  Width_Robust = round(width_robust, 4),
  Width_FGLS = round(width_fgls, 4),
  FGLS_Narrower = width_fgls < width_robust
)

print(comparison)

#e

# OLS 基礎模型
model_ols <- lm(log(wage) ~ educ + exper + exper2 + female + black + metro + south + midwest + west, data = cps5)

# Step 1: 初步估計殘差平方，做異質變異模型
resid_sq <- resid(model_ols)^2
aux_model <- lm(log(resid_sq) ~ metro + exper, data = cps5)
log_hatsigma2 <- fitted(aux_model)
hatsigma <- exp(log_hatsigma2 / 2)

# Step 2: 做 FGLS 回歸（加權）
X <- model.matrix(model_ols)
y <- log(cps5$wage)
y_star <- y / hatsigma
X_star <- X / hatsigma
model_fgls <- lm(y_star ~ X_star - 1)  # -1 去掉多餘截距

# Step 3: White robust 標準誤 on FGLS
robust_fgls_se <- sqrt(diag(vcovHC(model_fgls, type = "HC0")))
coef_fgls <- coef(model_fgls)
ci_fgls_robust <- cbind(
  coef_fgls - 1.96 * robust_fgls_se,
  coef_fgls + 1.96 * robust_fgls_se
)
colnames(ci_fgls_robust) <- c("Lower (FGLS+Robust)", "Upper (FGLS+Robust)")
width_fgls_robust <- ci_fgls_robust[,2] - ci_fgls_robust[,1]

# Step 4: 再次取 OLS + robust CI
robust_ols_se <- sqrt(diag(vcovHC(model_ols, type = "HC0")))
coef_ols <- coef(model_ols)
ci_ols_robust <- cbind(
  coef_ols - 1.96 * robust_ols_se,
  coef_ols + 1.96 * robust_ols_se
)
width_ols_robust <- ci_ols_robust[,2] - ci_ols_robust[,1]

# Step 5: 取 (e) 中 FGLS 的常規 SE CI
fgls_se <- sqrt(diag(vcov(model_fgls)))
ci_fgls <- cbind(
  coef_fgls - 1.96 * fgls_se,
  coef_fgls + 1.96 * fgls_se
)
width_fgls <- ci_fgls[,2] - ci_fgls[,1]

# Step 6: 合併結果比較
comparison <- data.frame(
  Coef = rownames(ci_fgls),
  Width_OLS_Robust = round(width_ols_robust, 4),
  Width_FGLS = round(width_fgls, 4),
  Width_FGLS_Robust = round(width_fgls_robust, 4)
)

print(comparison)