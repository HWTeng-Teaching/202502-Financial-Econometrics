# 載入必要套件
library(car)  # 提供 linearHypothesis 函數

# 讀取資料 (請確認 "commute5.csv" 檔案位於工作目錄中)
data <- read.csv("commute5.csv")

# (a) 估計模型
# 模型： TIME = β1 + β2*DEPART + β3*REDS + β4*TRAINS + e
model <- lm(TIME ~ DEPART + REDS + TRAINS, data = data)
summary(model)

# (b) 計算各係數的 95% 信賴區間
ci <- confint(model)
print(ci)
# 若信賴區間較窄，代表估計較精確；反之則估計不精確。

# (c) 檢定每個紅燈的預期延遲是否小於 2 分鐘
# H0: β3 >= 2  vs.  H1: β3 < 2 (左尾檢定)
beta3 <- coef(model)["REDS"]
se_beta3 <- summary(model)$coefficients["REDS", "Std. Error"]
t_stat_red <- (beta3 - 2) / se_beta3
p_value_red <- pnorm(t_stat_red)  # 左尾檢定 p-value
cat("檢定 (c) - 紅燈延遲：t-stat =", t_stat_red, "p-value =", p_value_red, "\n")
# 若 p-value < 0.05，則拒絕虛無假設。

# (d) 檢定火車延遲是否等於 3 分鐘 (雙尾檢定，顯著水準 10%)
# H0: β4 = 3  vs.  H1: β4 ≠ 3
beta4 <- coef(model)["TRAINS"]
se_beta4 <- summary(model)$coefficients["TRAINS", "Std. Error"]
t_stat_train <- (beta4 - 3) / se_beta4
p_value_train <- 2 * (1 - pnorm(abs(t_stat_train)))  # 雙尾檢定 p-value
cat("檢定 (d) - 火車延遲：t-stat =", t_stat_train, "p-value =", p_value_train, "\n")
# 比較 p_value_train 與 0.10

# (e) 檢定出發時間對行程時間的影響
# Bill 若在 7:00 AM 出發則 DEPART = 30 (6:30 AM 後 30 分鐘)，7:30 AM 為 DEPART = 60，
# 預期差異 ΔTIME = 30β2
# H0: 30β2 >= 10  (即 β2 >= 10/30 = 0.3333) vs. H1: 30β2 < 10  (β2 < 0.3333)
beta2 <- coef(model)["DEPART"]
se_beta2 <- summary(model)$coefficients["DEPART", "Std. Error"]
t_stat_depart <- (beta2 - 0.3333) / se_beta2
p_value_depart <- pnorm(t_stat_depart)  # 左尾檢定 p-value
cat("檢定 (e) - 出發時間影響：t-stat =", t_stat_depart, "p-value =", p_value_depart, "\n")

# (f) 檢定火車延遲是否至少為紅燈延遲的三倍
# H0: β4 >= 3β3  vs.  H1: β4 < 3β3  等價於檢定 (β4 - 3β3) >= 0
# 利用 linearHypothesis 進行檢定：
lh_test <- linearHypothesis(model, hypothesis.matrix = "TRAINS - 3*REDS = 0", test = "F")
print(lh_test)
# 注意檢定結果中可檢視是否拒絕虛無假設 (若 p-value < 0.05，則支持對立假設)

# (g) 檢定 7:00 AM 出發 (DEPART = 30) 且遇 6 紅燈與 1 火車是否能在 7:45 AM 前到達
# 此時 E(TIME|X) = β1 + β2*30 + β3*6 + β4*1
L <- c(1, 30, 6, 1)  # 對應 (Intercept, DEPART, REDS, TRAINS)
est_time <- sum(coef(model) * L)
cov_mat <- vcov(model)
se_time <- sqrt(t(L) %*% cov_mat %*% L)
# H0: E(TIME|X) <= 45 vs. H1: E(TIME|X) > 45 (右尾檢定)
t_stat_time <- (est_time - 45) / se_time
p_value_time <- 1 - pnorm(t_stat_time)
cat("檢定 (g) - 準時到達：估計時間 =", est_time, "SE =", se_time,
    "t-stat =", t_stat_time, "p-value =", p_value_time, "\n")

# (h) 討論：若 Bill 必須確保不遲到 7:45 AM，
# 則目前 (g) 部分的假設設定（H0: E(TIME|X) <= 45 vs. H1: E(TIME|X) > 45）可能不完全符合決策目標。
# 若必須確保準時，理想上應設定：
# H0: E(TIME|X) > 45 (存在遲到風險) vs. H1: E(TIME|X) <= 45 (確保準時)。
# 反轉假設會改變檢定中 Type I 與 Type II 錯誤的意涵，需依決策風險重新設定檢定架構。
cat("檢定 (h) - 若 Bill 必須準時，則虛無與對立假設應反轉，並改變檢定焦點。\n")
