#5.33
rm(list = ls())

# 🔗 下載並載入 cex5_small 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cps5_small)

#A####
cps5_small$l_wage <- log(cps5_small$wage)
model <- lm(l_wage ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = cps5_small)
summary(model)
#根據回歸輸出，截距、educ、exper、exper² 與 educ×exper 這五個項目的 p 值均遠低於 0.01，
#顯示它們在 1% 顯著水準下均顯著不為零；唯獨 educ² 的 p 值約 0.115，未達到 10% 顯著水準，因此該項並不顯著
#B####
#EDUC 增加：若b2為正（或負），邊際效應會往上（或往下）調整。
#EXPER 增加：若b5為負（如本例），則隨著經驗提升，教育的邊際報酬會降低。
#這反映了教育與經驗之間存在交互關係：在不同的教育年數和工作經驗水準下，
#教育對工資的影響並不固定，而是會隨著這兩個變數的改變而動態調整。

#C####
# 2. 取得估計係數
coefs <- coef(model)
b1 <- coefs["educ"]
b2 <- coefs["I(educ^2)"]
b5 <- coefs["I(educ * exper)"]

# 3. 為每個觀察值計算邊際效應
cps5_small$marginal_effect <- b1 + 2*b2*cps5_small$educ + b5*cps5_small$exper

# 4. 繪製邊際效應的直方圖
hist(cps5_small$marginal_effect,
     main = "Histogram of Marginal Effects of EDUC on ln(WAGE)",
     xlab = "Marginal Effect")

# 5. 計算中位數、第 5 與第 95 百分位
median_me  <- median(cps5_small$marginal_effect, na.rm = TRUE)
quantiles_me <- quantile(cps5_small$marginal_effect, probs = c(0.05, 0.95), na.rm = TRUE)

# 顯示結果
median_me
quantiles_me
#D####
#EDUC↑：若b5>0，則經驗的邊際效應提高；反之若 b5<0，則隨教育增加，經驗的邊際效應降低。
#EXPER↑：若 b4<0，邊際效應呈現遞減；若 b4>0，邊際效應遞增。
#這些項目反映了教育與經驗對工資的相互影響方式，不同教育程度、不同經驗水準下，
#經驗對工資的貢獻並不固定，而是會動態調整。
#E####
# 1) 從模型中擷取估計係數
coefs <- coef(model)

# 2) 指派相關係數（需與您實際模型中的變數名稱對應）
b3 <- coefs["exper"]               # β3
b4 <- coefs["I(exper^2)"]          # β4
b5 <- coefs["I(educ * exper)"]     # β5

# 3) 計算每筆觀察對 exper 的邊際效應: ∂ln(WAGE)/∂EXPER = β3 + 2*β4*EXPER + β5*EDUC
cps5_small$marginal_exper <- b3 + 2*b4*cps5_small$exper + b5*cps5_small$educ

# 4) 繪製直方圖
hist(cps5_small$marginal_exper,
     main = "Histogram of Marginal Effects wrt EXPER",
     xlab = "Marginal Effect on ln(WAGE)")

# 5) 計算中位數、5th、95th 百分位
me_median    <- median(cps5_small$marginal_exper, na.rm = TRUE)
me_5th       <- quantile(cps5_small$marginal_exper, probs = 0.05, na.rm = TRUE)
me_95th      <- quantile(cps5_small$marginal_exper, probs = 0.95, na.rm = TRUE)

# 6) 輸出結果
cat("中位數 (Median):",    me_median, "\n",
    "第5百分位 (5th):",   me_5th,    "\n",
    "第95百分位 (95th):", me_95th,   "\n")
#F####
# David:   EDUC = 17, EXPER = 8
# Svetlana: EDUC = 16, EXPER = 18
#
# 預期對數工資分別為：
#   E[ln(WAGE_S)] = β0 + β1*16 + β2*16^2 + β3*18 + β4*18^2 + β5*(16*18)
#   E[ln(WAGE_D)] = β0 + β1*17 + β2*17^2 + β3*8  + β4*8^2  + β5*(17*8)
#
# 差異定義為：
#   Δ = E[ln(WAGE_S)] - E[ln(WAGE_D)]
#
# 經過代數整理，可得：
#   Δ = -β1 - 33*β2 + 10*β3 + 260*β4 + 152*β5
#
# 我們的假設檢定設定為：
#   H0: Δ ≥ 0    (即 Svetlana 的預期對數工資 ≥ David 的)
#   H1: Δ <  0    (即 Svetlana 的預期對數工資 < David 的)
#G####
# 原先兩人條件：
#   David:   EDUC = 17, EXPER = 8
#   Svetlana: EDUC = 16, EXPER = 18
#
# 原先預期對數工資的差額 (Δ_old) 為：
#   Δ_old = E[ln(WAGE_S)] - E[ln(WAGE_D)]
#         = -β1 - 33*β2 + 10*β3 + 260*β4 + 152*β5
#
# 經過 8 年後，條件變為（教育不變，只經驗增加 8 年）：
#   David:   EDUC = 17, EXPER = 8 + 8 = 16
#   Svetlana: EDUC = 16, EXPER = 18 + 8 = 26
#
# 此時預期對數工資的差額 (Δ_new) 為：
#   Δ_new = -β1 - 33*β2 + 10*β3 + 420*β4 + 144*β5
#
# 兩者之間的變化量為：
#   Δ_new - Δ_old = (420 - 260)*β4 + (144 - 152)*β5 
#                  = 160*β4 - 8*β5

# 假設我們已經得到模型參數估計值，以下為估計值（請根據實際模型替換）
beta1 <- 0.08954     # EDUC 的係數
beta2 <- 0.001458    # EDUC^2 的係數
beta3 <- 0.04488     # EXPER 的係數
beta4 <- -0.000468   # EXPER^2 的係數
beta5 <- -0.001010   # EDUC*EXPER 的係數

# 計算原先的 Δ_old
Delta_old <- -beta1 - 33*beta2 + 10*beta3 + 260*beta4 + 152*beta5

# 計算經過 8 年後的 Δ_new
Delta_new <- -beta1 - 33*beta2 + 10*beta3 + 420*beta4 + 144*beta5

# 計算變化量
Delta_change <- Delta_new - Delta_old

cat("原先的 Δ_old =", Delta_old, "\n")
cat("經過 8 年後的 Δ_new =", Delta_new, "\n")
cat("變化量 Δ_new - Δ_old =", Delta_change, "\n")

# 解釋：
# 原先 Δ_old = -β1 - 33β2 + 10β3 + 260β4 + 152β5，
# 現在 Δ_new = -β1 - 33β2 + 10β3 + 420β4 + 144β5，
# 因為經驗變數的二次項隨著 EXPER 的增加改變更快，
# Δ_new 與 Δ_old 的差別為 160β4 - 8β5。
#
# 依據上述估計值：
#   160*β4 = 160 * (-0.000468) ≈ -0.07488
#   -8*β5  = -8 * (-0.001010) ≈ 0.00808
#   所以變化量約為 -0.07488 + 0.00808 = -0.06680
#
# 意即經過 8 年後，Δ_new 比 Δ_old 小約 0.067。
#
# 如果原先的檢定結果接近臨界值（例如 Δ_old 稍微大於 0），那麼這樣的減少可能使得
# Δ_new 變成負值，從而導致檢定結果不同。也就是說，當兩人的經驗都增加 8 年後，
# 由於經驗二次效應（以及交互項）的影響，Svetlana 與 David 的預期對數工資差額會發生改變，
# 減少的差額意味著 David 的預期對數工資可能會變得更高，從而改變原來的檢定結果。

cat("\n結論：\n")
cat("經過8年後，由於工作經驗的非線性效應，兩人預期對數工資的差額減少了約0.067單位。\n")
cat("因此，原先的檢定結果（Svetlana的預期對數工資至少不低於David的）可能會改變，\n")
cat("這取決於原先差額是否接近臨界值。若原先差額原本不明顯，則增加經驗後可能顯著地使得\n")
cat("David的預期對數工資高於Svetlana的。\n")
#H####
#模型為
#ln(WAGE) = β₀ + β₁·EDUC + β₂·EDUC² + β₃·EXPER + β₄·EXPER² + β₅·(EDUC×EXPER) + e
#而經驗的邊際效應為：
# ∂ln(WAGE)/∂EXPER = β₃ + 2β₄·EXPER + β₅·EDUC
# 
# 對於 Wendy (EDUC = 12, EXPER = 17) 與 Jill (EDUC = 16, EXPER = 1)，分別有：
# 
# Wendy 的邊際效應：
# ME_W = β₃ + 2β₄·17 + β₅·12
# 
# Jill 的邊際效應：
# ME_J = β₃ + 2β₄·1 + β₅·16
# 
# 要求兩人的邊際效應相等，即 ME_W = ME_J，則有：
# 
# β₃ + 34β₄ + 12β₅ = β₃ + 2β₄ + 16β₅
# 
# 消去 β₃ 後：
# 34β₄ + 12β₅ = 2β₄ + 16β₅
# 
# 整理得：
# 32β₄ = 4β₅→β₅ = 8β₄
# 
# 因此，我們的檢定可以表示為：
# 
# H₀: β₅ = 8β₄(兩人的經驗邊際效應相同)
# H₁: β₅ ≠ 8β₄(兩人的經驗邊際效應不同)
#I####
# 給定模型參數估計值
beta3 <- 0.04488
beta4 <- -0.000468
beta5 <- -0.001010

# 給定標準誤（根據模型摘要）
SE_beta3 <- 0.007297
SE_beta4 <- 7.601e-05
SE_beta5 <- 0.0003791

# Jill 的教育水準為 16 年，目前經驗為 1 年

# 計算使邊際效應等於零時的經驗年數
# 邊際效應 = beta3 + 2*beta4*EXPER + beta5*16 = 0  ==> 
# EXPER_0 = - (beta3 + 16*beta5) / (2*beta4)
EXPER_0 <- - (beta3 + 16*beta5) / (2 * beta4)

# Jill 還需要的額外經驗 T (目前有 1 年)
T_hat <- EXPER_0 - 1
cat("T 的點估計 =", T_hat, "\n")

# 定義函數 f = T = - (beta3 + 16*beta5)/(2*beta4) - 1
# 利用 delta 方法計算偏導數
# ∂f/∂beta3 = -1/(2*beta4)
# ∂f/∂beta5 = -16/(2*beta4) = -8/beta4
# ∂f/∂beta4 = (beta3 + 16*beta5)/(2*beta4^2)
df_dbeta3 <- -1 / (2 * beta4)
df_dbeta5 <- -8 / beta4
df_dbeta4 <- (beta3 + 16 * beta5) / (2 * beta4^2)

cat("偏導數：\n")
cat("∂f/∂beta3 =", df_dbeta3, "\n")
cat("∂f/∂beta5 =", df_dbeta5, "\n")
cat("∂f/∂beta4 =", df_dbeta4, "\n")

# 假設參數間獨立，則 T 的變異數可近似為：
Var_T <- (df_dbeta3^2) * (SE_beta3^2) +
  (df_dbeta5^2) * (SE_beta5^2) +
  (df_dbeta4^2) * (SE_beta4^2)
SE_T <- sqrt(Var_T)
cat("T 的標準誤 =", SE_T, "\n")

# 95% 信賴區間
lower <- T_hat - 1.96 * SE_T
upper <- T_hat + 1.96 * SE_T
cat("T 的95%信賴區間: [", lower, ",", upper, "]\n")
