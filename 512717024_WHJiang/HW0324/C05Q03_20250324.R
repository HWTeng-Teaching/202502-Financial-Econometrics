#C05Q03

##############################################################################
# 前置設定：清除環境、載入套件與資料
##############################################################################

rm(list = ls())  # 清除工作環境

# 載入必要套件
library(dplyr)
library(lmtest)


# -------------------------------
# (a) 填補空缺數值的計算
# -------------------------------

# 1. t-statistic for β₁
beta1_coeff <- 1.4515       # β₁ 的估計係數
beta1_se    <- 2.2019       # β₁ 的標準誤
t_beta1     <- beta1_coeff / beta1_se
cat("t-statistic for β₁ =", round(t_beta1, 4), "\n")  
# 預期輸出約 0.6593

# 2. Standard error for β₂
beta2_coeff <- 2.7648       # β₂ 的估計係數
t_beta2     <- 5.7103       # β₂ 的 t 統計量
beta2_se    <- beta2_coeff / t_beta2
cat("Standard error for β₂ =", round(beta2_se, 4), "\n")
# 預期輸出約 0.4840

# 3. Estimate for β₃
t_beta3   <- -3.9376        # β₃ 的 t 統計量
beta3_se  <- 0.3695         # β₃ 的標準誤
beta3_est <- t_beta3 * beta3_se
cat("Estimate for β₃ =", round(beta3_est, 4), "\n")
# 預期輸出約 -1.4547

# 4. R² 的計算
# 給定: N = 1200, s_y = 6.39547
N <- 1200
s_y <- 6.39547
SST <- (N - 1) * (s_y^2)    # SST = (N-1) * (s_y)^2
# 根據解答，此處使用 SSE = 46221.6 來計算 R²
SSE <- 46221.6
R2 <- 1 - SSE / SST
cat("R-squared =", round(R2, 4), "\n")
# 預期輸出約 0.0575

# 5. 迴歸標準誤 (σ̂)
# 公式: σ̂ = sqrt(SSE / (N - K))
# 此處 K 為參數數量（截距與 3 個解釋變數，所以 K = 4）
K <- 4
sigma_hat <- sqrt(46221.62 / (N - K))
cat("Standard error of regression (σ̂) =", round(sigma_hat, 3), "\n")
# 預期輸出約 6.217

# -------------------------------
# (b) 估計係數的解釋
# -------------------------------
cat("\nInterpretation of the Coefficients:\n")
cat("β₂ =", beta2_coeff, 
    ": A 1% increase in total expenditure increases alcohol budget share by", beta2_coeff, "percentage points.\n")
cat("β₃ =", beta3_est, 
    ": Each additional child reduces alcohol budget share by", abs(beta3_est), "percentage points.\n")
beta4 <- -0.1503  # 由題目給定 β₄
cat("β₄ =", beta4, 
    ": Each additional year in household head age reduces alcohol budget share by", abs(beta4), "percentage points.\n")

# -------------------------------
# (c) 95% 信賴區間對 β₄ 的計算
# -------------------------------
SE_beta4 <- 0.0235       # β₄ 的標準誤
CI_lower <- beta4 - 1.96 * SE_beta4
CI_upper <- beta4 + 1.96 * SE_beta4
cat("\n95% Confidence Interval for β₄: [", round(CI_lower, 4), ",", round(CI_upper, 4), "]\n")
# 預期輸出約 [–0.1964, –0.1042]

# -------------------------------
# (d) Significance at the 5% Level
# -------------------------------
cat("\nSignificance at 5% level:\n")
cat("β₁: p-value = 0.5099 -> Not significant\n")
cat("β₂, β₃, and β₄: p-values < 0.05 -> Statistically significant\n")

# -------------------------------
# (e) 假說檢定：測試 β₃ = –2 vs. β₃ ≠ –2
# -------------------------------
# 設定假說:
# H0: β₃ = -2
# H1: β₃ ≠ -2
t_test_beta3 <- (beta3_est - (-2)) / beta3_se
cat("\nt-statistic for testing H0: β₃ = -2:", round(t_test_beta3, 3), "\n")
# 預期輸出約 1.476，由於 |1.476| < 1.96，故在 5% 水準下無法拒絕 H0

# 判斷結論：若 |t| < 1.96 則無法拒絕 H0
if (abs(t_test_beta3) < 1.96) {
  cat("Conclusion: Fail to reject H0. There is insufficient evidence at the 5% level to conclude that β₃ differs from -2.\n")
} else {
  cat("Conclusion: Reject H0. There is evidence at the 5% level to conclude that β₃ differs from -2.\n")
}

