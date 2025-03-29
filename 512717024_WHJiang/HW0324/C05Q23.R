# (1) 假設已經有一個名為 "cocaine.csv" 的檔案，並包含 PRICE, QUANT, QUAL, TREND 變數
# 若已經有 data frame，就可以省略 read.csv 的步驟
cocaine <- read.csv("cocaine.csv")

# 檢視前幾筆資料
head(cocaine)

# (2) 建立線性迴歸模型
model <- lm(PRICE ~ QUANT + QUAL + TREND, data = cocaine)

# (3) 查看模型摘要
summary(model)

# (c) 取得 R^2 來衡量解釋力
r_squared <- summary(model)$r.squared
cat("R-squared =", r_squared, "\n")

# (4d) 假設檢定：H0: β2 = 0 vs. H1: β2 < 0
# 以下示範兩種方式：

# ---- 方法1：使用 summary() 的結果，手動計算單尾 p-value ----
coef_info <- coef(summary(model))
beta2_est  <- coef_info["QUANT", "Estimate"]    # β2 的估計值
beta2_se   <- coef_info["QUANT", "Std. Error"]  # β2 的標準誤
t_value    <- coef_info["QUANT", "t value"]     # t 統計量（兩尾檢定用）
df         <- model$df.residual                 # 自由度

# 因為 summary() 給的是兩尾檢定的 t-value，若要單尾檢定 (β2 < 0)，
# 則單尾 p-value = pt(t_value, df = df)
p_value_one_sided <- pt(t_value, df = df)  # 左尾檢定
cat("For QUANT < 0 test, t-value =", t_value, 
    "one-sided p-value =", p_value_one_sided, "\n")

# ---- 方法2：使用 car 套件的 linearHypothesis() 進行檢定 (兩尾或單尾需要自行解讀) ----
# install.packages("car")  # 若尚未安裝
library(car)
lh_test_quant <- linearHypothesis(model, "QUANT = 0")
print(lh_test_quant)
# 由於 linearHypothesis() 預設提供兩尾檢定，需要自行解讀 F 統計量與 p-value

# (4e) 假設檢定：H0: β3 = 0 vs. H1: β3 > 0
beta3_est   <- coef_info["QUAL", "Estimate"]  
beta3_se    <- coef_info["QUAL", "Std. Error"] 
t_value_3   <- coef_info["QUAL", "t value"]    
# 單尾檢定 (β3 > 0)，則單尾 p-value = 1 - pt(t_value_3, df)
p_value_one_sided_qual <- 1 - pt(t_value_3, df = df)
cat("For QUAL > 0 test, t-value =", t_value_3, 
    "one-sided p-value =", p_value_one_sided_qual, "\n")

# 或使用 linearHypothesis() 檢定 (兩尾結果自行解讀)
lh_test_qual <- linearHypothesis(model, "QUAL = 0")
print(lh_test_qual)

# (5f) TREND 係數即為每年價格平均變動量
beta4_est <- coef_info["TREND", "Estimate"]
cat("Estimated annual change in price (β4) =", beta4_est, "\n")
