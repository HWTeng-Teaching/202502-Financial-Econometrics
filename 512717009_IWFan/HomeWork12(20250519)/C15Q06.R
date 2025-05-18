# ==================================================
# 📦 套件
# ==================================================
library(plm)
library(lmtest)
library(sandwich)

# ==================================================
# 📥 載入正確資料來源：nls_panel.rdata
# ==================================================
load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/nls_panel.rdata"))

# 建立變數 exper^2
nls_panel$exper2 <- nls_panel$exper^2

# 設定 panel 結構
pdata <- pdata.frame(nls_panel, index = c("id", "year"))

# ==================================================
# (a) OLS：1987 與 1988 分別估計
# ==================================================
ols_1987 <- lm(lwage ~ exper + exper2 + south + union, data = subset(nls_panel, year == 87))
ols_1988 <- lm(lwage ~ exper + exper2 + south + union, data = subset(nls_panel, year == 88))
summary(ols_1987)
summary(ols_1988)

# ==================================================
# (b) 隨機效果模型（RE）
# ==================================================
re_model <- plm(lwage ~ exper + exper2 + south + union, data = pdata, model = "random")
summary(re_model)

# ==================================================
# (c) 固定效果模型（FE）與 95% CI（conventional SE）
# ==================================================
fe_model <- plm(lwage ~ exper + exper2 + south + union, data = pdata, model = "within")
summary(fe_model)

# 手動 CI 計算
fe_se <- summary(fe_model)$coefficients[, "Std. Error"]
fe_coef <- summary(fe_model)$coefficients[, "Estimate"]
fe_ci <- data.frame(
  Estimate = round(fe_coef, 4),
  Lower = round(fe_coef - 1.96 * fe_se, 4),
  Upper = round(fe_coef + 1.96 * fe_se, 4)
)
print(fe_ci)

# ==================================================
# (d) F 檢定：檢驗是否需使用 FE
# ==================================================
pooling_model <- plm(lwage ~ exper + exper2 + south + union, data = pdata, model = "pooling")
pFtest(fe_model, pooling_model)

# ==================================================
# (e) Robust SE（Arellano 型）與 SE 比值
# ==================================================
robust_se <- vcovHC(fe_model, method = "arellano", type = "HC1")
coeftest(fe_model, vcov = robust_se)

# 比值計算：傳統 / robust
robust_vals <- sqrt(diag(robust_se))
se_ratios <- round(fe_se / robust_vals, 4)
print(se_ratios)

# ==================================================
# (f) Hausman 檢定與 RE / FE 比值
# ==================================================
hausman_result <- phtest(fe_model, re_model)
print(hausman_result)

# RE / FE 比值
re_est <- summary(re_model)$coefficients[, "Estimate"]
fe_est <- summary(fe_model)$coefficients[, "Estimate"]
coef_ratio <- round(re_est[names(fe_est)] / fe_est, 2)
print(coef_ratio)

