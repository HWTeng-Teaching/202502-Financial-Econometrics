# C02Q16
###############################################################################
#程式碼說明

#1.參數設定與計算：
#根據題目給定的回歸參數與已知樣本均值，程式計算出：
#(a) 鄉村（Rural）的平均教育年數與工資彈性。
#(b) 都市（Urban）的平均工資、工資彈性及其標準誤。
#(c) 針對 12 與 16 年教育分別預測工資。

# 2.結果摘要表格：
#透過 results_table 資料框整理出各區域的關鍵結果，可直接印出以供檢查。

#3.圖形呈現：
#利用基礎繪圖功能，畫出教育年數（橫軸）與預測工資（縱軸）的關係曲線，分別用藍色 (Urban) 與紅色 (Rural) 表示，並標出教育 12 與 16 年的預測工資點，並加上圖例說明。
##############################

# 清除環境變數
rm(list = ls())

##############################
# 參數設定 (根據題目給定數值)
##############################
# Urban regression parameters:
urban_intercept <- -10.76
urban_beta      <- 2.46
n_urban         <- 986
# Urban given mean EDUC:
urban_mean_EDUC <- 13.68
# For urban, the SE for beta is given as 0.16 and for intercept 2.27 (未來若需要可用)
urban_se_beta   <- 0.16

# Rural regression parameters:
rural_intercept <- -4.88
rural_beta      <- 1.80
n_rural         <- 214
# Given mean wage for rural:
rural_mean_WAGE <- 19.74

##############################
# (a) Rural: Elasticity Calculation
##############################
# 由於迴歸線通過均值點，所以：
# mean_EDUC_rural = (mean_WAGE - rural_intercept) / rural_beta
rural_mean_EDUC <- (rural_mean_WAGE - rural_intercept) / rural_beta
# 計算彈性： elasticity = beta * (mean_EDUC / mean_WAGE)
elasticity_rural <- rural_beta * (rural_mean_EDUC / rural_mean_WAGE)
# 輸出結果 (應約為1.25)
cat("(a) Rural Elasticity Calculation:\n")
cat("Mean EDUC (Rural) =", round(rural_mean_EDUC, 2), "\n")
cat("Elasticity (Rural) =", round(elasticity_rural, 2), "\n\n")

##############################
# (b) Urban: Standard Error of Elasticity
##############################
# 首先計算 Urban 的 mean WAGE:
urban_mean_WAGE <- urban_intercept + urban_beta * urban_mean_EDUC
# Urban elasticity = urban_beta * (urban_mean_EDUC / urban_mean_WAGE)
elasticity_urban <- urban_beta * (urban_mean_EDUC / urban_mean_WAGE)
# 由於均值被視為固定，則 SE(elasticity) = (mean_EDUC / mean_WAGE) * SE(beta)
se_elasticity_urban <- (urban_mean_EDUC / urban_mean_WAGE) * urban_se_beta

cat("(b) Urban Elasticity and its Standard Error:\n")
cat("Mean WAGE (Urban) =", round(urban_mean_WAGE, 2), "\n")
cat("Elasticity (Urban) =", round(elasticity_urban, 2), "\n")
cat("SE(Elasticity, Urban) =", round(se_elasticity_urban, 3), "\n\n")

##############################
# (c) Predicted Wages for 12 and 16 years of EDUC
##############################
# 預測公式： WAGE = intercept + beta * EDUC

# Urban predictions
urban_pred_12 <- urban_intercept + urban_beta * 12
urban_pred_16 <- urban_intercept + urban_beta * 16

# Rural predictions
rural_pred_12 <- rural_intercept + rural_beta * 12
rural_pred_16 <- rural_intercept + rural_beta * 16

cat("(c) Predicted Wages:\n")
cat("Urban: 12 years =", round(urban_pred_12, 2), ", 16 years =", round(urban_pred_16, 2), "\n")
cat("Rural: 12 years =", round(rural_pred_12, 2), ", 16 years =", round(rural_pred_16, 2), "\n\n")

##############################
# 整理結果成表格 (供摘要用)
##############################
results_table <- data.frame(
  Area = c("Urban", "Rural"),
  Mean_EDUC = c(urban_mean_EDUC, round(rural_mean_EDUC, 2)),
  Mean_WAGE = c(round(urban_mean_WAGE,2), rural_mean_WAGE),
  Beta = c(urban_beta, rural_beta),
  Elasticity = c(round(elasticity_urban, 2), round(elasticity_rural, 2)),
  SE_Elasticity = c(round(se_elasticity_urban, 3), NA),
  Pred_12 = c(round(urban_pred_12, 2), round(rural_pred_12, 2)),
  Pred_16 = c(round(urban_pred_16, 2), round(rural_pred_16, 2))
)
print(results_table)

##############################
# 繪製圖表：預測工資 vs. EDUC (Urban 與 Rural)
##############################
# 設定教育年數區間 (例如從10到20)
educ_range <- seq(10, 20, by = 0.1)

# 計算預測工資
urban_wage <- urban_intercept + urban_beta * educ_range
rural_wage <- rural_intercept + rural_beta * educ_range

# 繪圖 (以基礎繪圖為例)
plot(educ_range, urban_wage, type = "l", lwd = 2, col = "blue",
     ylim = range(c(urban_wage, rural_wage)),
     xlab = "Years of Education", ylab = "Predicted Wage",
     main = "Predicted Wage vs. Education (Urban & Rural)")
lines(educ_range, rural_wage, lwd = 2, col = "red")
# 加入在 12 與 16 年的預測點
points(c(12, 16), c(urban_pred_12, urban_pred_16), col = "blue", pch = 16, cex = 1.5)
points(c(12, 16), c(rural_pred_12, rural_pred_16), col = "red", pch = 16, cex = 1.5)
# 加入圖例
legend("topleft", legend = c("Urban", "Rural"),
       col = c("blue", "red"), lwd = 2, bty = "n")
