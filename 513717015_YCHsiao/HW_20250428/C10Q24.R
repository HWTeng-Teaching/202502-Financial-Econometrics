#######C10Q24 #############


install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(AER)
library(POE5Rdata)
data("mroz")  # 載入資料

# 建立 IV/2SLS 模型（注意變數名稱要完全對）
model_2sls <- ivreg(lfp ~ educ + exper | mothereduc + fathereduc + exper, data = mroz)

# 取得殘差
resid_iv <- resid(model_2sls)

# 繪圖：EXPER vs 殘差
plot(mroz$exper, resid_iv,
     xlab = "EXPER",
     ylab = "IV/2SLS Residuals",
     main = "Residuals vs. Exper",
     pch = 20, col = "blue")
abline(h = 0, lty = 2, col = "red")

#############(b)############

# Step 1: 殘差平方
resid_sq <- resid_iv^2

# Step 2: 檢定迴歸 (對常數 + exper)
bp_model <- lm(resid_sq ~ mroz$exper)

# Step 3: 取得 R^2 與樣本數
r_squared <- summary(bp_model)$r.squared
n <- nrow(mroz)

# Step 4: 計算 NR^2 與 p-value
nr2 <- n * r_squared
p_value <- 1 - pchisq(nr2, df = 1)

# 輸出結果
cat("Breusch-Pagan Test (NR^2):\n")
cat("Test statistic = ", round(nr2, 3), "\n")
cat("p-value = ", round(p_value, 5), "\n")


######C. 

# 需要 sandwich 套件和 lmtest 套件
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)

# 使用 coeftest() + vcovHC() 計算 robust 標準誤
robust_se <- coeftest(model_2sls, vcov = vcovHC(model_2sls, type = "HC1"))

# 顯示結果
print(robust_se)

# 抓出 EDU 的估計值與 robust SE
educ_coef <- robust_se["educ", "Estimate"]
educ_se <- robust_se["educ", "Std. Error"]

# 計算 95% 信賴區間
lower_bound <- educ_coef - 1.96 * educ_se
upper_bound <- educ_coef + 1.96 * educ_se

cat("\n95% CI for EDUC coefficient (Robust SE): [", round(lower_bound, 4), ",", round(upper_bound, 4), "]\n")

############d.###########

# 需要 boot 套件
install.packages("boot")  # 如果尚未安裝
library(boot)

# 自訂 bootstrap 函數：每次抽樣後估計 IV/2SLS 並回傳 coef(educ)
boot_iv <- function(data, indices) {
  d <- data[indices, ]  # 依據 index 抽樣
  model <- ivreg(lfp ~ educ + exper | mothereduc + fathereduc + exper, data = d)
  return(coef(model))  # 回傳所有係數
}

# 執行 bootstrap，B = 200 次
set.seed(123)  # 設定隨機種子以便重現
boot_result <- boot(data = mroz, statistic = boot_iv, R = 200)

# 查看 bootstrap 的標準誤（注意順序：intercept, educ, exper）
boot_se <- apply(boot_result$t, 2, sd)
print(boot_se)

# 抓出 educ 的估計值與 bootstrap SE
educ_coef_boot <- coef(model_2sls)["educ"]
educ_se_boot <- boot_se[2]

# 計算 95% 信賴區間
lower_ci <- educ_coef_boot - 1.96 * educ_se_boot
upper_ci <- educ_coef_boot + 1.96 * educ_se_boot

cat("\nBootstrap SE for EDUC =", round(educ_se_boot, 5), "\n")
cat("95% CI for EDUC coefficient (Bootstrap): [", round(lower_ci, 4), ",", round(upper_ci, 4), "]\n")

